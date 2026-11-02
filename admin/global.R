# UI helpers for editing questions and their groups.

insert_question_button <- function(payload) {
  tags$button(
    type = "button",
    class = "insert-question-button",
    onclick = sprintf("Shiny.setInputValue('insert_question','%s',{priority:'event'})", payload),
    "+"
  )
}

clone_survey_state <- function(questions, question_order, groups, group_order) {
  question_ids <- names(questions)
  group_ids <- names(groups)
  question_map <- setNames(vapply(question_ids, function(id) uuid::UUIDgenerate(), character(1)), question_ids)
  group_map <- setNames(vapply(group_ids, function(id) uuid::UUIDgenerate(), character(1)), group_ids)

  cloned_questions <- lapply(question_ids, function(old_id) {
    question <- questions[[old_id]]
    question$id <- unname(question_map[[old_id]])
    question$group_id <- unname(group_map[[question$group_id]])
    question
  })
  names(cloned_questions) <- unname(question_map[question_ids])

  cloned_groups <- lapply(group_ids, function(old_id) {
    group <- groups[[old_id]]
    group$id <- unname(group_map[[old_id]])
    condition_id <- group$settings$visibility$question_id
    if (!is.null(condition_id)) group$settings$visibility$question_id <- unname(question_map[[condition_id]])
    group
  })
  names(cloned_groups) <- unname(group_map[group_ids])

  list(
    questions = cloned_questions,
    order = unname(question_map[question_order]),
    groups = cloned_groups,
    group_order = unname(group_map[group_order])
  )
}

question_options_ui <- function(question_id, question) {
  settings <- question$settings
  if (is.null(settings)) settings <- list()
  randomize_id <- paste0("question_randomize_choices_", question_id)
  required_id <- paste0("question_required_", question_id)

  tagList(
    checkboxInput(randomize_id, "選択肢をランダム表示", isTRUE(settings$randomize_choices)),
    checkboxInput(required_id, "回答必須", if (is.null(settings$required)) TRUE else isTRUE(settings$required)),
    tags$button(
      type = "button",
      class = "btn btn-primary btn-sm",
      onclick = sprintf("Shiny.setInputValue('save_question_options_for','%s',{priority:'event'})", question_id),
      "オプションを保存"
    )
  )
}

question_editor_panel_ui <- function(question_id, question) {
  suffix <- question_id
  options_text <- paste(unlist(question$options), collapse = "\n")
  tagList(
    textInput(paste0("question_edit_title_", suffix), "設問タイトル", value = question$title),
    textAreaInput(paste0("question_edit_desc_", suffix), "説明（任意）", value = question$desc),
    selectInput(paste0("question_edit_type_", suffix), "回答形式",
      choices = c("単一選択" = "single", "複数選択" = "multiple", "プルダウン" = "select", "数値" = "numeric", "自由記述" = "text", "スライダー" = "slider", "日付" = "date"),
      selected = question$type
    ),
    conditionalPanel(
      condition = sprintf("input['question_edit_type_%s'] == 'single' || input['question_edit_type_%s'] == 'multiple' || input['question_edit_type_%s'] == 'select'", suffix, suffix, suffix),
      textAreaInput(paste0("question_edit_options_", suffix), "選択肢（1行に1つ）", value = options_text)
    ),
    conditionalPanel(
      condition = sprintf("input['question_edit_type_%s'] == 'slider'", suffix),
      numericInput(paste0("question_edit_min_", suffix), "下限", value = if (is.null(question$min)) 0 else question$min),
      numericInput(paste0("question_edit_max_", suffix), "上限", value = if (is.null(question$max)) 100 else question$max)
    ),
    tags$button(
      type = "button", class = "btn btn-primary btn-sm",
      onclick = sprintf("Shiny.setInputValue('save_question_edit_for','%s',{priority:'event'})", question_id),
      "設問を保存"
    ),
    tags$button(
      type = "button", class = "btn btn-default btn-sm",
      onclick = sprintf("Shiny.setInputValue('cancel_question_edit_for','%s',{priority:'event'})", question_id),
      "閉じる"
    )
  )
}

question_detail_panel_ui <- function(question_id, question, tab) {
  div(class = "question-detail-panel",
    if (identical(tab, "edit")) question_editor_panel_ui(question_id, question)
    else question_options_ui(question_id, question)
  )
}

question_group_settings_ui <- function(group_id, group, previous_questions) {
  settings <- group$settings
  condition <- settings$visibility
  randomize_questions_id <- paste0("group_randomize_questions_", group_id)
  has_visibility_id <- paste0("group_has_visibility_", group_id)
  condition_question_id <- paste0("group_condition_question_", group_id)
  condition_operator_id <- paste0("group_condition_operator_", group_id)
  condition_value_id <- paste0("group_condition_value_", group_id)

  condition_choices <- setNames(
    vapply(previous_questions, function(question) question$id, character(1)),
    vapply(previous_questions, function(question) question$title, character(1))
  )

  tagList(
    checkboxInput(randomize_questions_id, "グループ内の設問をランダム表示", isTRUE(settings$randomize_questions)),
    checkboxInput(has_visibility_id, "回答条件でこのグループを表示", !is.null(condition)),
    conditionalPanel(
      condition = sprintf("input['%s'] == true", has_visibility_id),
      selectInput(condition_question_id, "判定に使う設問", choices = condition_choices,
                  selected = if (!is.null(condition)) condition$question_id else NULL),
      selectInput(condition_operator_id, "条件",
                  choices = c("次と等しい" = "equals", "次と等しくない" = "not_equals"),
                  selected = if (!is.null(condition)) condition$operator else "equals"),
      textInput(condition_value_id, "回答値", value = if (!is.null(condition)) condition$value else "")
    ),
    tags$button(
      type = "button",
      class = "btn btn-primary btn-sm",
      onclick = sprintf("Shiny.setInputValue('save_group_settings_for','%s',{priority:'event'})", group_id),
      "グループ設定を保存"
    )
  )
}

question_group_board_ui <- function(groups, group_order, questions, question_order,
                                   editing_group, editing_question_options, editing_question_tab) {
  if (!length(question_order)) {
    return(tagList(
      p("設問はまだありません。"),
      insert_question_button("new")
    ))
  }

  group_cards <- lapply(group_order, function(group_id) {
    group <- groups[[group_id]]
    question_ids <- question_order[vapply(questions[question_order], function(question) {
      identical(question$group_id, group_id)
    }, logical(1))]
    group_index <- match(group_id, group_order)
    previous_group_ids <- if (group_index > 1) group_order[seq_len(group_index - 1)] else character()
    previous_ids <- question_order[vapply(questions[question_order], function(question) {
      question$group_id %in% previous_group_ids
    }, logical(1))]
    previous_questions <- questions[previous_ids]

    question_items <- lapply(seq_along(question_ids), function(question_index) {
      question_id <- question_ids[question_index]
      question <- questions[[question_id]]
      title <- if (is.null(question$title) || is.na(question$title) || !nzchar(question$title)) {
        "設問タイトルを入力"
      } else question$title
      settings_open <- identical(editing_question_options, question_id)
      panel_tab <- if (identical(editing_question_tab, "edit") && settings_open) "edit" else "options"
      question_number <- match(question_id, question_order)

      tagList(
        div(class = "question-card", `data-question-id` = question_id, draggable = "true",
          div(class = "question-card-heading",
            span(class = "drag-handle", "⠿"),
            span(class = "question-number", paste0(question_number, ".")),
            strong(title),
            span(class = "question-type", question$type)
          ),
          if (!is.null(question$desc) && length(question$desc) && !is.na(question$desc) && nzchar(question$desc)) {
            div(class = "question-description", question$desc)
          },
          if (!is.null(question$options)) div(class = "question-options", paste("選択肢:", paste(unlist(question$options), collapse = ", "))),
          if (!is.null(question$min) && question$type == "slider") div(class = "question-options", paste0("範囲: ", question$min, "〜", question$max)),
          div(class = "question-actions",
            tags$button(
              type = "button", class = paste("btn btn-default btn-sm", if (panel_tab == "edit" && settings_open) "active" else ""),
              onclick = sprintf("Shiny.setInputValue('select_question_panel_tab','%s|edit',{priority:'event'})", question_id),
              "編集"
            ),
            tags$button(
              type = "button", class = paste("btn btn-default btn-sm", if (panel_tab == "options" && settings_open) "active" else ""),
              onclick = sprintf("Shiny.setInputValue('select_question_panel_tab','%s|options',{priority:'event'})", question_id),
              "オプション"
            ),
            actionButton(
              paste0("del_", question_id), "削除",
              onclick = sprintf("Shiny.setInputValue('delete_q','%s',{priority:'event'})", question_id),
              class = "btn-danger btn-sm"
            )
          ),
          if (settings_open) question_detail_panel_ui(question_id, question, panel_tab)
        ),
        if (question_index < length(question_ids)) insert_question_button(paste("in", group_id, question_ids[question_index + 1], sep = "|"))
      )
    })

    tagList(
      div(class = paste("group-card", if (length(question_ids) > 1) "multi-question-group" else "single-question-group"),
        `data-group-id` = group_id,
        div(class = "group-options-row",
          span(if (length(question_ids) > 1) paste0(length(question_ids), "設問をグループ化中") else "単独設問"),
          tags$button(
            type = "button", class = "btn btn-default btn-sm",
            onclick = sprintf("Shiny.setInputValue('toggle_group_settings','%s',{priority:'event'})", group_id),
            if (identical(editing_group, group_id)) "グループ設定を閉じる" else "グループ設定"
          )
        ),
        div(class = "question-list", `data-group-id` = group_id, question_items),
        if (identical(editing_group, group_id)) {
          div(class = "group-settings-panel",
            question_group_settings_ui(group_id, group, previous_questions)
          )
        }
      )
    )
  })

  board_items <- c(
    list(insert_question_button(paste("before_group", group_order[1], sep = "|"))),
    unlist(lapply(seq_along(group_cards), function(i) list(
      group_cards[[i]],
      insert_question_button(paste("after_group", group_order[i], sep = "|"))
    )), recursive = FALSE)
  )

  tagList(
    p(class = "drag-instructions", "設問カード中央に重ねるとグループ化し、上下端にドロップすると順番を変えられます。設問間の＋で追加し、編集・オプションボタンから内容を設定します。"),
    div(class = "group-board", board_items)
  )
}
