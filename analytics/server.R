library(shiny)
library(showtext)

font_add(
  "jp",
  "/usr/share/fonts/opentype/noto/NotoSerifCJK-Bold.ttc"
)
showtext_auto()


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