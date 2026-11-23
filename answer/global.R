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
  input <- question_response_input(q$id, q, value)

  div(
    class = "question-card",
    div(class = "question-title", q$title),
    if (!is.null(q$desc) && length(q$desc) && !is.na(q$desc) && nzchar(q$desc)) {
      div(class = "question-description", q$desc)
    },
    div(class = "question-input", input)
  )
}
