library(shiny)
library(showtext)
library(ggplot2)

# Shiny Server の既定ロケールが C の場合でも、日本語をUTF-8として表示する
try(Sys.setlocale("LC_CTYPE", "C.UTF-8"), silent = TRUE)
font_add("jp", "/usr/share/fonts/opentype/noto/NotoSerifCJK-Bold.ttc")
showtext_auto()

source("db.R")

analysis_theme <- function() {
  theme_minimal(base_family = "jp", base_size = 13) +
    theme(
      plot.title = element_text(face = "bold", margin = margin(b = 14)),
      axis.title = element_text(face = "bold"),
      panel.grid.major.y = element_blank(),
      panel.grid.minor = element_blank()
    )
}

frequency_plot <- function(values, title, fill = "#2C7FB8") {
  counts <- as.data.frame(table(values), stringsAsFactors = FALSE)
  names(counts) <- c("answer", "count")
  counts <- counts[counts$answer != "" & !is.na(counts$answer), , drop = FALSE]

  ggplot(counts, aes(x = reorder(answer, count), y = count)) +
    geom_col(fill = fill, width = 0.68) +
    geom_text(aes(label = count), hjust = -0.2, family = "jp") +
    coord_flip(clip = "off") +
    labs(title = title, x = NULL, y = "回答数") +
    expand_limits(y = max(counts$count, 0) * 1.15) +
    analysis_theme()
}

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
      selectInput("cross_col", "列にする設問", choices = choices, selected = ids[2]),
      radioButtons(
        "cross_display",
        "表示方法",
        choices = c("件数" = "count", "構成比（％）" = "percent"),
        selected = "count",
        inline = TRUE
      )
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

  question_categories <- function(question, observed_answers) {

    configured <- character()

    if (!is.null(question$options)) {
      configured <- enc2utf8(as.character(unlist(question$options, use.names = FALSE)))
    }

    observed <- enc2utf8(as.character(observed_answers))
    observed <- observed[!is.na(observed) & nzchar(trimws(observed))]

    unique(c(configured, observed))

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

    row_categories <- question_categories(qs[[input$cross_row]], row_data$answer_text)
    col_categories <- question_categories(qs[[input$cross_col]], col_data$answer_text)

    result <- merge(row_data, col_data, by = "response_id", suffixes = c("_row", "_col"))

    attr(result, "row_title") <- qs[[input$cross_row]]$title
    attr(result, "col_title") <- qs[[input$cross_col]]$title
    attr(result, "row_categories") <- row_categories
    attr(result, "col_categories") <- col_categories

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

    counts <- table(
      factor(d$answer_text_row, levels = attr(d, "row_categories")),
      factor(d$answer_text_col, levels = attr(d, "col_categories"))
    )

    if (identical(input$cross_display, "percent")) {
      total <- sum(counts)
      values <- counts / total * 100
      values <- matrix(
        paste0(format(round(values, 1), nsmall = 1, trim = TRUE), "%"),
        nrow = nrow(counts),
        dimnames = dimnames(counts)
      )
      row_totals <- paste0(format(round(rowSums(counts) / total * 100, 1), nsmall = 1, trim = TRUE), "%")
      col_totals <- paste0(format(round(colSums(counts) / total * 100, 1), nsmall = 1, trim = TRUE), "%")
      grand_total <- "100.0%"
    } else {
      values <- counts
      row_totals <- rowSums(counts)
      col_totals <- colSums(counts)
      grand_total <- sum(counts)
    }

    values <- cbind(values, "合計" = row_totals)
    values <- rbind(values, "合計" = c(col_totals, grand_total))

    result <- data.frame(
      row_label = rownames(values),
      values,
      check.names = FALSE,
      stringsAsFactors = FALSE
    )
    names(result)[1] <- paste0(
      attr(d, "row_title"),
      "＼",
      attr(d, "col_title")
    )
    result

  }, rownames = FALSE)

  output$crosstab_heading <- renderText({
    if (identical(input$cross_display, "percent")) {
      "クロス集計表（構成比）"
    } else {
      "クロス集計表（件数）"
    }
  })

  output$crosstab_note <- renderText({

    d <- crosstab_data()
    message <- attr(d, "message")
    if (!is.null(message)) return(message)

    paste0(
      "行：", attr(d, "row_title"), " ＼ 列：", attr(d, "col_title"),
      "。両方の設問に回答した ", length(unique(d$response_id)),
      " 人を集計しています。複数選択式は選択肢ごとに集計されます。"
    )

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
    plot_data <- data.frame(x = d$x, y = d$y)
    p <- ggplot(plot_data, aes(x = x, y = y)) +
      geom_point(size = 3, alpha = 0.72, color = "#2C7FB8") +
      labs(
        title = "数値同士の関係",
        x = qs[[input$numeric_x]]$title,
        y = qs[[input$numeric_y]]$title
      ) +
      analysis_theme()

    if (nrow(d) >= 2 && length(unique(d$x)) > 1) {
      p <- p + geom_smooth(method = "lm", se = TRUE, color = "#D95F0E", fill = "#FDD49E")
    }

    print(p)

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
              print(frequency_plot(d$answer_text, q$title))

            },

            select = {
              print(frequency_plot(d$answer_text, q$title))

            },

            multiple = {

              x <- unlist(
                strsplit(
                  d$answer_text,
                  ","
                )
              )

              print(frequency_plot(trimws(x), q$title, "#F28E2B"))

            },

            numeric = {

              values <- suppressWarnings(as.numeric(d$answer_text))
              print(
                ggplot(data.frame(value = values), aes(x = value)) +
                  geom_histogram(bins = 12, fill = "#59A14F", color = "white") +
                  labs(title = q$title, x = "値", y = "回答数") +
                  analysis_theme()
              )

            },

            slider = {

              values <- suppressWarnings(as.numeric(d$answer_text))
              print(
                ggplot(data.frame(value = values), aes(x = value)) +
                  geom_histogram(bins = 12, fill = "#59A14F", color = "white") +
                  labs(title = q$title, x = "値", y = "回答数") +
                  analysis_theme()
              )

            },

            date = {

              print(frequency_plot(d$answer_text, q$title, "#AF7AA1"))

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


