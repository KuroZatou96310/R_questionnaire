library(shiny)
library(bslib)

source("../common/db.R")

shinyServer(function(input, output, session){

  survey_questions <- reactiveVal(NULL)
  survey_groups <- reactiveVal(NULL)
  active_group <- reactiveVal(NULL)
  answer_values <- reactiveValues()
  submitted <- reactiveVal(FALSE)

  load_survey <- function(id) {
    groups <- load_groups(id)
    qs <- load_questions(id)
    if (length(qs) == 0) {
      showNotification(
        "アンケートが見つかりません",
        type="error"
      )
      return(FALSE)
    }

    qs <- randomize_survey_questions(qs, groups)

    survey_groups(groups)
    survey_questions(qs)
    active_group(names(groups)[1])
    for (question_id in names(answer_values)) answer_values[[question_id]] <- NULL
    submitted(FALSE)
    TRUE
  }

  observe({
    qs <- survey_questions()
    if (is.null(qs)) return()
    for (question_id in names(qs)) {
      value <- input[[question_id]]
      if (!is.null(value)) answer_values[[question_id]] <- value
    }
  })

  visible_group_ids <- reactive({
    groups <- survey_groups()
    req(groups)
    ids <- names(groups)
    ids[vapply(ids, function(group_id) {
      condition <- groups[[group_id]]$settings$visibility
      if (is.null(condition)) return(TRUE)
      value <- input[[condition$question_id]]
      if (is.null(value)) value <- answer_values[[condition$question_id]]
      if (is.null(value)) return(FALSE)
      matches <- as.character(condition$value) %in% as.character(value)
      if (identical(condition$operator, "not_equals")) !matches else matches
    }, logical(1))]
  })

  observe({
    ids <- visible_group_ids()
    current <- active_group()
    if (length(ids) && (is.null(current) || !current %in% ids)) active_group(ids[1])
  })

  current_answer <- function(question_id) {
    value <- input[[question_id]]
    if (is.null(value)) answer_values[[question_id]] else value
  }

  missing_questions <- function(question_ids, qs) {
    question_ids[vapply(question_ids, function(id) {
      value <- current_answer(id)
      required <- qs[[id]]$settings$required
      if (is.null(required)) required <- TRUE
      isTRUE(required) && (is.null(value) || !length(value) || !any(!is.na(value) & nzchar(trimws(as.character(value)))))
    }, logical(1))]
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
    groups <- survey_groups()
    req(qs, groups)
    group_ids <- visible_group_ids()
    current_id <- active_group()
    req(length(group_ids), current_id)
    group_index <- match(current_id, group_ids)
    group <- groups[[current_id]]
    group_question_ids <- names(qs)[vapply(qs, function(q) identical(q$group_id, current_id), logical(1))]
    group_question_ids <- group_question_ids[group_question_ids %in% names(qs)]

    tagList(
      div(class = "survey-card",
        div(class = "survey-id", "回答するアンケート"),
        h2(class = "h4 mb-0", input$survey_id),
        p(paste0("グループ ", group_index, " / ", length(group_ids)))
      ),
      h3(group$title),
      lapply(group_question_ids, function(id) answer_question_ui(qs[[id]], current_answer(id))),
      div(class = "group-navigation",
        if (group_index > 1) actionButton("group_previous", "前へ") else NULL,
        if (group_index < length(group_ids)) {
          actionButton("group_next", "次へ", class = "btn-primary")
        } else {
          actionButton("submit", "回答を送信する", class = "btn-primary answer-submit")
        }
      )
    )
  })

  observeEvent(input$group_previous, {
    ids <- visible_group_ids()
    index <- match(active_group(), ids)
    if (!is.na(index) && index > 1) active_group(ids[index - 1])
  })

  observeEvent(input$group_next, {
    qs <- survey_questions()
    current_id <- active_group()
    current_ids <- names(qs)[vapply(qs, function(q) identical(q$group_id, current_id), logical(1))]
    missing_ids <- missing_questions(current_ids, qs)
    if (length(missing_ids)) {
      showModal(modalDialog(
        title = "未回答の設問",
        p("このグループの設問に回答してください。"),
        tags$ul(lapply(vapply(qs[missing_ids], function(q) q$title, character(1)), tags$li)),
        easyClose = TRUE,
        footer = modalButton("戻る")
      ))
      return()
    }
    ids <- visible_group_ids()
    index <- match(current_id, ids)
    if (!is.na(index) && index < length(ids)) active_group(ids[index + 1])
  })

  observeEvent(input$submit, {

    qs <- survey_questions()
    groups <- survey_groups()
    req(qs, groups)

    visible_ids <- visible_group_ids()
    visible_questions <- names(qs)[vapply(qs, function(q) q$group_id %in% visible_ids, logical(1))]
    unanswered_ids <- missing_questions(visible_questions, qs)

    if (length(unanswered_ids)) {
      showModal(modalDialog(
        title = "未回答の設問",
        p("以下の設問に回答してください。"),
        tags$ul(lapply(vapply(qs[unanswered_ids], function(q) q$title, character(1)), tags$li)),
        easyClose = TRUE,
        footer = modalButton("戻る")
      ))
      return()
    }

    answers <- lapply(visible_questions, current_answer)
    answers <- answers[vapply(answers, function(value) !is.null(value) && length(value) && any(!is.na(value) & nzchar(trimws(as.character(value)))), logical(1))]
    names(answers) <- visible_questions

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
