library(shiny)

# ui.R と server.R の両方から使う設問 UI
answer_question_ui <- function(q) {
  options <- unlist(q$options, use.names = FALSE)

  input <- switch(
    q$type,
    single   = radioButtons(q$id, NULL, choices = options),
    multiple = checkboxGroupInput(q$id, NULL, choices = options),
    select   = selectInput(q$id, NULL, choices = options, selectize = FALSE),
    numeric  = numericInput(q$id, NULL, value = NA),
    text     = textAreaInput(q$id, NULL, rows = 3),
    slider   = sliderInput(q$id, NULL, min = q$min, max = q$max, value = q$min),
    date     = dateInput(q$id, NULL),
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
