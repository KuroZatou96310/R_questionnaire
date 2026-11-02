library(shiny)

randomize_survey_questions <- function(questions, groups) {
  question_order <- names(questions)
  for (group_id in names(groups)) {
    group_questions <- question_order[vapply(questions[question_order], function(question) {
      identical(question$group_id, group_id)
    }, logical(1))]
    if (isTRUE(groups[[group_id]]$settings$randomize_questions) && length(group_questions) > 1) {
      positions <- which(question_order %in% group_questions)
      question_order[positions] <- sample(group_questions)
    }
  }

  questions <- questions[question_order]
  for (question_id in names(questions)) {
    options <- questions[[question_id]]$options
    if (isTRUE(questions[[question_id]]$settings$randomize_choices) && length(options) > 1) {
      questions[[question_id]]$options <- sample(options)
    }
  }
  questions
}

# ui.R と server.R の両方から使う設問 UI
answer_question_ui <- function(q, value = NULL) {
  options <- unlist(q$options, use.names = FALSE)

  input <- switch(
    q$type,
    single   = radioButtons(q$id, NULL, choices = options, selected = if (is.null(value)) character(0) else value),
    multiple = checkboxGroupInput(q$id, NULL, choices = options, selected = value),
    select   = tags$select(
      id = q$id,
      class = "form-control shiny-input-select",
      tags$option("選択してください", value = "", selected = NA, disabled = NA),
      lapply(options, function(option) tags$option(option, value = option, selected = identical(option, value)))
    ),
    numeric  = numericInput(q$id, NULL, value = if (is.null(value)) NA else value),
    text     = textAreaInput(q$id, NULL, value = if (is.null(value)) "" else value, rows = 3),
    slider   = sliderInput(q$id, NULL, min = q$min, max = q$max, value = if (is.null(value)) q$min else value),
    date     = {
      date_input <- dateInput(q$id, NULL, value = if (is.null(value)) Sys.Date() else value)
      date_input$children[[2]]$attribs[["data-initial-date"]] <- ""
      date_input$children[[2]]$attribs$placeholder <- "日付を選択"
      date_input
    },
    NULL
  )

  div(
    class = "question-card",
    div(class = "question-title", q$title),
    if (!is.null(q$desc) && length(q$desc) && !is.na(q$desc) && nzchar(q$desc)) {
      div(class = "question-description", q$desc)
    },
    div(class = "question-input", input)
  )
}
