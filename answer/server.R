library(shiny)
library(bslib)

source("../common/db.R")

shinyServer(function(input, output, session){

  survey_questions <- reactiveVal(NULL)
  submitted <- reactiveVal(FALSE)

  load_survey <- function(id) {
    qs <- load_questions(id)
    if (length(qs) == 0) {
      showNotification(
        "アンケートが見つかりません",
        type="error"
      )
      return(FALSE)
    }

    random_start <- load_random_start(id)
    if (!is.null(random_start) && random_start >= 1 && random_start <= length(qs)) {
      fixed <- if (random_start > 1) qs[seq_len(random_start - 1)] else list()
      randomized <- qs[seq.int(random_start, length(qs))]
      qs <- c(fixed, randomized[sample(seq_along(randomized))])
    }

    survey_questions(qs)
    submitted(FALSE)
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
    if (isTRUE(submitted())) {
      return(div(
        class = "completion-card",
        h2("アンケートの回答を終了しました"),
        p("ご回答ありがとうございました。")
      ))
    }

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
    unanswered <- character(0)

    for (q in qs) {
      value <- input[[q$id]]
      has_value <- !is.null(value) && length(value) > 0 &&
        any(!is.na(value) & nzchar(trimws(as.character(value))))
      if (!has_value) {
        unanswered <- c(unanswered, q$title)
      } else {
        answers[[q$id]] <- value
      }
    }

    if (length(unanswered)) {
      showModal(modalDialog(
        title = "未回答の設問",
        p("以下の設問に回答してください。"),
        tags$ul(lapply(unanswered, tags$li)),
        easyClose = TRUE,
        footer = modalButton("戻る")
      ))
      return()
    }

    save_response(
      input$survey_id,
      answers
    )

    showNotification(
      "回答を送信しました"
    )

    submitted(TRUE)

  })

})
