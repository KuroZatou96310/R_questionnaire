library(shiny)
library(bslib)

source("db.R")

shinyServer(function(input, output, session){

  survey_questions <-reactiveVal(NULL)
observeEvent(session, {

  query <- getQueryString()
  if (is.null(query$id)) return()

  updateTextInput(session, "survey_id", value = query$id)

  data <- load_questions(query$id)
  if (length(data) > 0) {
    survey_questions(data)
  }

})
 
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

      card(
        card_header(
          input$survey_id
        ),
        card_body(
          tagList(
            lapply(qs, function(q){

              label <- tagList(
                strong(q$title),
                br(),
                q$desc
              )

              opts <- unlist(q$options)
              card(
                card_header(
                  q$title
                ),
                card_body(
                  q$desc,
                ),
                card_footer(
                  switch(
                    q$type,
                    "single"   = radioButtons(q$id, NULL, choices = opts),
                    "multiple" = checkboxGroupInput(q$id, NULL, choices = opts),
                    "select"   = selectInput(q$id, NULL, choices = opts),
                    "numeric"  = numericInput(q$id, NULL, value = NA),
                    "text"     = textAreaInput(q$id, NULL),
                    "slider"   = sliderInput(q$id, NULL, min = q$min, max = q$max, value = q$min),
                    "date"     = dateInput(q$id, NULL)
                  )
                )
              )

            })
          )
        )
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