library(shiny)

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
