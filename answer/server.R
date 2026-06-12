library(shiny)

source("db.R")

shinyServer(function(input, output, session){

  survey_questions <- reactiveVal(NULL)

  observeEvent(input$load, {

    req(input$survey_id)

    qs <- load_questions(
      input$survey_id
    )

    if(length(qs)==0){

      showNotification(
        "アンケートが見つかりません",
        type="error"
      )

      return()
    }

    survey_questions(qs)

  })

  output$question_ui <- renderUI({

    qs <- survey_questions()

    req(qs)

    tagList(

      lapply(qs,function(q){

        label <- tagList(
          strong(q$title),
          br(),
          q$desc
        )

        opts <- unlist(q$options)

        switch(

          q$type,

          "single" =
            radioButtons(
              q$id,
              label,
              choices=opts
            ),

          "multiple" =
            checkboxGroupInput(
              q$id,
              label,
              choices=opts
            ),

          "select" =
            selectInput(
              q$id,
              label,
              choices=opts
            ),

          "numeric" =
            numericInput(
              q$id,
              label,
              value=NA
            ),

          "text" =
            textAreaInput(
              q$id,
              label
            ),

          "slider" =
            sliderInput(
              q$id,
              label,
              min=q$min,
              max=q$max,
              value=q$min
            ),

          "date" =
            dateInput(
              q$id,
              label
            )

        )

      })

    )

  })

  observeEvent(input$submit, {

    qs <- survey_questions()

    req(qs)

    answers <- list()

    for(q in qs){

      answers[[q$id]] <- input[[q$id]]

    }

    save_response(
      input$survey_id,
      answers
    )

    showNotification(
      "回答を送信しました"
    )

  })

})