library(shiny)
library(bslib)

source("db.R")

shinyServer(function(input, output, session){

  survey_questions <- reactiveVal(NULL)

  load_survey <- function(id) {
    qs <- load_questions(id)
    if (length(qs) == 0) {
      showNotification(
        "アンケートが見つかりません",
        type="error"
      )
      return(FALSE)
    }

    survey_questions(qs)
    TRUE
  }

  observeEvent(getQueryString(), {
    id <- getQueryString()$id
    if (is.null(id) || !nzchar(id)) return()

    updateTextInput(session, "survey_id", value = id)
    load_survey(id)
  }, ignoreInit = FALSE)

  observeEvent(input$load, {
    req(input$survey_id)
    id <- trimws(input$survey_id)
    req(nzchar(id))

    updateQueryString(
      paste0("?id=", utils::URLencode(id, reserved = TRUE)),
      mode = "push",
      session = session
    )
  })

  output$question_ui <- renderUI({
    qs <- survey_questions()
    req(qs)

    tagList(
      div(
        class = "survey-card",
        div(class = "survey-id", "回答するアンケート"),
        h2(class = "h4 mb-0", input$survey_id)
      ),
      lapply(qs, answer_question_ui),
      actionButton("submit", "回答を送信する", class = "btn-primary answer-submit")
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
