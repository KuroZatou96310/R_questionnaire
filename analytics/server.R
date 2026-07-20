library(shiny)
library(showtext)

# Shiny Server の既定ロケールが C の場合でも、日本語をUTF-8として表示する
try(Sys.setlocale("LC_CTYPE", "C.UTF-8"), silent = TRUE)

source("db.R")

shinyServer(function(input, output, session){

  survey_questions <- reactiveVal(NULL)
  survey_answers   <- reactiveVal(NULL)


  #-------------------------
  # アンケート読込
  #-------------------------
  observeEvent(input$load,{

    req(
      nzchar(input$survey_id),
      nzchar(input$survey_pw)
    )

    db <- load_db()

    if(!(input$survey_id %in% names(db))){
      showNotification(
        "アンケートが見つかりません",
        type = "error"
      )
      return()
    }

    if(db[[input$survey_id]]$password !=
      hash_pw(input$survey_pw)){

      showNotification(
        "パスワードが違います",
        type = "error"
      )
      return()
    }

    survey_questions(
      load_questions(input$survey_id)
    )

    survey_answers(
      load_answers(input$survey_id)
    )

    showNotification(
      "読み込みました"
    )

  })

  #-------------------------
  # 回答数
  #-------------------------
  output$response_count <- renderText({

    req(survey_answers())

    paste(
      "回答数：",
      length(unique(
        survey_answers()$response_id
      ))
    )

  })

  #-------------------------
  # UI生成
  #-------------------------
  output$analysis_ui <- renderUI({

    req(
      survey_questions(),
      survey_answers()
    )

    qs <- survey_questions()

    tagList(

      lapply(names(qs),function(id){

        q <- qs[[id]]

        tagList(

          hr(),

          h3(q$title),

          p(q$desc),

          plotOutput(
            paste0("plot_",id),
            height = "350px"
          ),

          tableOutput(
            paste0("table_",id)
          )

        )

      })

    )

  })

  #-------------------------
  # クロス集計
  #-------------------------
  categorical_question_ids <- reactive({

    req(survey_questions())

    ids <- names(survey_questions())
    ids[vapply(
      survey_questions()[ids],
      function(q) q$type %in% c("single", "select", "multiple", "date"),
      logical(1)
    )]

  })

  output$crosstab_ui <- renderUI({

    ids <- categorical_question_ids()

    if (length(ids) < 2) {
      return(helpText("クロス集計には、選択式・複数選択式・日付の設問が2つ以上必要です。"))
    }

    qs <- survey_questions()
    choices <- setNames(ids, vapply(qs[ids], function(q) q$title, character(1)))

    tagList(
      selectInput("cross_row", "行にする設問", choices = choices, selected = ids[1]),
      selectInput("cross_col", "列にする設問", choices = choices, selected = ids[2])
    )

  })

  expand_categorical_answers <- function(data, question_type) {

    if (is.null(question_type) || question_type != "multiple" || nrow(data) == 0) {
      return(data)
    }

    expanded <- lapply(seq_len(nrow(data)), function(i) {

      choices <- trimws(unlist(strsplit(as.character(data$answer_text[i]), ",", fixed = TRUE)))
      choices <- choices[nzchar(choices) & !is.na(choices)]

      if (length(choices) == 0) {
        return(NULL)
      }

      data.frame(
        response_id = rep(data$response_id[i], length(choices)),
        answer_text = choices,
        stringsAsFactors = FALSE
      )

    })

    expanded <- Filter(Negate(is.null), expanded)

    if (length(expanded) == 0) {
      return(data.frame(response_id = character(), answer_text = character()))
    }

    do.call(rbind, expanded)

  }

  # 回答していない設問（NULL、空文字）を除外して、必要な2列を必ず持つ形にする
  answered_question_data <- function(answers, question_id) {

    is_target <- answers$question_id == question_id
    answer_text <- as.character(answers$answer_text)
    has_answer <- !is.na(answer_text) & nzchar(trimws(answer_text))
    rows <- which(is_target & has_answer)

    data.frame(
      response_id = as.character(answers$response_id[rows]),
      answer_text = answer_text[rows],
      stringsAsFactors = FALSE
    )

  }

  crosstab_data <- reactive({

    req(survey_answers(), survey_questions(), input$cross_row, input$cross_col)

    qs <- survey_questions()

    empty_result <- function(message) {
      result <- data.frame(
        response_id = character(),
        answer_text_row = character(),
        answer_text_col = character(),
        stringsAsFactors = FALSE
      )
      attr(result, "message") <- message
      result
    }

    if (!isTRUE(input$cross_row != input$cross_col)) {
      return(empty_result("行と列には異なる設問を選んでください。"))
    }

    if (!(input$cross_row %in% names(qs) && input$cross_col %in% names(qs))) {
      return(empty_result("設問を選択してください。"))
    }

    ans <- survey_answers()
    row_data <- answered_question_data(ans, input$cross_row)
    col_data <- answered_question_data(ans, input$cross_col)

    row_data <- expand_categorical_answers(row_data, qs[[input$cross_row]]$type)
    col_data <- expand_categorical_answers(col_data, qs[[input$cross_col]]$type)

    result <- merge(row_data, col_data, by = "response_id", suffixes = c("_row", "_col"))

    if (nrow(result) == 0) {
      attr(result, "message") <- "両方の設問に回答した人がいません。"
    }

    result

  })

  output$crosstab_table <- renderTable({

    d <- crosstab_data()

    message <- attr(d, "message")
    if (!is.null(message)) {
      result <- data.frame(message = message, stringsAsFactors = FALSE)
      names(result) <- "案内"
      return(result)
    }

    as.data.frame.matrix(table(d$answer_text_row, d$answer_text_col))

  }, rownames = TRUE)

  output$crosstab_note <- renderText({

    d <- crosstab_data()
    message <- attr(d, "message")
    if (!is.null(message)) return(message)

    paste0("両方の設問に回答した ", length(unique(d$response_id)), " 人を集計しています。複数選択式は選択肢ごとに集計されます。")

  })

  #-------------------------
  # 数値同士の分析
  #-------------------------
  numeric_question_ids <- reactive({

    req(survey_questions())

    ids <- names(survey_questions())
    ids[vapply(
      survey_questions()[ids],
      function(q) q$type %in% c("numeric", "slider"),
      logical(1)
    )]

  })

  output$numeric_analysis_ui <- renderUI({

    ids <- numeric_question_ids()

    if (length(ids) < 2) {
      return(helpText("数値同士の分析には、数値入力またはスライダーの設問が2つ以上必要です。"))
    }

    qs <- survey_questions()
    choices <- setNames(ids, vapply(qs[ids], function(q) q$title, character(1)))

    tagList(
      selectInput("numeric_x", "横軸の設問", choices = choices, selected = ids[1]),
      selectInput("numeric_y", "縦軸の設問", choices = choices, selected = ids[2])
    )

  })

  numeric_relation_data <- reactive({

    req(survey_answers(), survey_questions(), input$numeric_x, input$numeric_y)

    empty_result <- function(message) {
      result <- data.frame(response_id = character(), x = numeric(), y = numeric())
      attr(result, "message") <- message
      result
    }

    if (!isTRUE(input$numeric_x != input$numeric_y)) {
      return(empty_result("横軸と縦軸には異なる設問を選んでください。"))
    }

    ans <- survey_answers()
    x_answers <- answered_question_data(ans, input$numeric_x)
    y_answers <- answered_question_data(ans, input$numeric_y)
    x <- data.frame(response_id = x_answers$response_id, x = x_answers$answer_text)
    y <- data.frame(response_id = y_answers$response_id, y = y_answers$answer_text)

    d <- merge(x, y, by = "response_id")
    d$x <- suppressWarnings(as.numeric(d$x))
    d$y <- suppressWarnings(as.numeric(d$y))
    d <- d[complete.cases(d$x, d$y), , drop = FALSE]

    if (nrow(d) == 0) {
      attr(d, "message") <- "両方の設問に数値で回答した人がいません。"
    }

    d

  })

  output$numeric_relation_plot <- renderPlot({

    d <- numeric_relation_data()
    message <- attr(d, "message")

    if (!is.null(message)) {
      plot.new()
      text(0.5, 0.5, message)
      return()
    }

    qs <- survey_questions()
    plot(d$x, d$y,
      xlab = qs[[input$numeric_x]]$title,
      ylab = qs[[input$numeric_y]]$title,
      main = "数値同士の関係",
      pch = 19,
      col = rgb(0.15, 0.45, 0.75, 0.65)
    )

    if (nrow(d) >= 2 && length(unique(d$x)) > 1) {
      abline(lm(y ~ x, data = d), col = "tomato", lwd = 2)
    }

  })

  output$numeric_relation_table <- renderTable({

    d <- numeric_relation_data()
    message <- attr(d, "message")

    if (is.null(message) && nrow(d) < 2) {
      message <- "相関を計算するには、2人以上の回答が必要です。"
    }

    if (!is.null(message)) {
      result <- data.frame(message = message, stringsAsFactors = FALSE)
      names(result) <- "案内"
      return(result)
    }

    result <- data.frame(
      metric = c("有効回答数", "Pearson相関係数", "Spearman相関係数"),
      value = c(
        nrow(d),
        round(cor(d$x, d$y, method = "pearson"), 3),
        round(cor(d$x, d$y, method = "spearman"), 3)
      ),
      check.names = FALSE
    )

    names(result) <- c("指標", "値")
    result

  }, rownames = FALSE)

  #-------------------------
  # グラフ・表生成
  #-------------------------
  observe({

    req(
      survey_questions(),
      survey_answers()
    )

    qs <- survey_questions()
    ans <- survey_answers()

    for(id in names(qs)){

      local({

        qid <- id
        q <- qs[[qid]]

        #---------------------
        # グラフ
        #---------------------

        output[[paste0("plot_",qid)]] <- renderPlot({

          par(family = "jp")


          d <- subset(
            ans,
            question_id == qid
          )

          if(nrow(d)==0){

            plot.new()

            text(
              0.5,
              0.5,
              "回答なし"
            )

            return()

          }

          switch(

            q$type,

            single = {

              tb <- table(d$answer_text)

              barplot(
                tb,
                col="skyblue",
                las=2,
                main=q$title
              )

            },

            select = {

              tb <- table(d$answer_text)

              barplot(
                tb,
                col="skyblue",
                las=2,
                main=q$title
              )

            },

            multiple = {

              x <- unlist(
                strsplit(
                  d$answer_text,
                  ","
                )
              )

              tb <- table(trimws(x))

              barplot(
                tb,
                col="orange",
                las=2,
                main=q$title
              )

            },

            numeric = {

              hist(
                as.numeric(d$answer_text),
                main=q$title,
                col="lightgreen",
                xlab=""
              )

            },

            slider = {

              hist(
                as.numeric(d$answer_text),
                main=q$title,
                col="lightgreen",
                xlab=""
              )

            },

            date = {

              tb <- table(d$answer_text)

              barplot(
                tb,
                las=2,
                col="pink",
                main=q$title
              )

            },

            text = {

              plot.new()

              text(
                0.5,
                0.5,
                "自由記述は下表を参照"
              )

            }

          )

        })

        #---------------------
        # 表
        #---------------------

        output[[paste0("table_",qid)]] <- renderTable({

          d <- subset(
            ans,
            question_id == qid
          )

          switch(

            q$type,

            single = {

              tb <- as.data.frame(
                table(d$answer_text)
              )

              names(tb) <- c(
                "回答",
                "件数"
              )

              tb

            },

            select = {

              tb <- as.data.frame(
                table(d$answer_text)
              )

              names(tb) <- c(
                "回答",
                "件数"
              )

              tb

            },

            multiple = {

              x <- unlist(
                strsplit(
                  d$answer_text,
                  ","
                )
              )

              tb <- as.data.frame(
                table(trimws(x))
              )

              names(tb) <- c(
                "回答",
                "件数"
              )

              tb

            },

            numeric = {

              x <- as.numeric(
                d$answer_text
              )

              data.frame(

                key=c(
                  "平均",
                  "中央値",
                  "最小",
                  "最大",
                  "標準偏差"
                ),

                value=c(
                  mean(x,na.rm=TRUE),
                  median(x,na.rm=TRUE),
                  min(x,na.rm=TRUE),
                  max(x,na.rm=TRUE),
                  sd(x,na.rm=TRUE)
                )

              )

            },

            slider = {

              x <- as.numeric(
                d$answer_text
              )

              data.frame(

                key=c(
                  "平均",
                  "中央値",
                  "最小",
                  "最大",
                  "標準偏差"
                ),

                value=c(
                  mean(x,na.rm=TRUE),
                  median(x,na.rm=TRUE),
                  min(x,na.rm=TRUE),
                  max(x,na.rm=TRUE),
                  sd(x,na.rm=TRUE)
                )

              )

            },

            date = {

              tb <- as.data.frame(
                table(d$answer_text)
              )

              names(tb) <- c(
                "日付",
                "件数"
              )

              tb

            },

            text = {

              data.frame(

                answer_text_=d$answer_text,

                stringsAsFactors=FALSE

              )

            }

          )

        })

      })

    }

  })

  #-------------------------
  # CSVダウンロード
  #-------------------------
  output$download_csv <- downloadHandler(

    filename=function(){

      paste0(
        input$survey_id,
        "_answers.csv"
      )

    },

    content=function(file){

      write.csv(

        survey_answers(),

        file,

        row.names=FALSE,

        fileEncoding="UTF-8"

      )

    }

  )

})
