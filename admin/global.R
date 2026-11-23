# UI helpers for editing questions and their groups.

insert_question_button <- function(payload, final = FALSE) {
  tags$button(
    type = "button",
    class = paste("insert-question-button", if (final) "insert-question-button-final" else ""),
    `data-idle-label` = "設問を追加",
    onclick = sprintf("Shiny.setInputValue('insert_question','%s',{priority:'event'})", payload),
    "設問を追加"
  )
}

board_action <- function(event, payload, label, disabled = FALSE, danger = FALSE) {
  tags$button(type = "button", class = paste("btn", if (danger) "btn-danger" else "btn-default", "btn-sm"),
    disabled = if (disabled) "disabled" else NULL,
    onclick = sprintf("Shiny.setInputValue('%s','%s',{priority:'event'})", event, payload), label)
}

duplicate_question <- function(question) {
  question$id <- NULL
  question
}

move_item <- function(order, id, direction) {
  from <- match(id, order)
  to <- from + if (direction == "down") 1L else -1L
  if (is.na(from) || to < 1L || to > length(order)) return(order)
  order[c(from, to)] <- order[c(to, from)]
  order
}

sanitize_group_visibility <- function(groups) {
  group_order <- names(groups)
  for (i in seq_along(group_order)) {
    group_id <- group_order[i]
    clean <- function(settings) {
      condition <- settings$visibility
      if (is.null(condition)) return(settings)
      condition_id <- condition$question_id
      source_position <- which(vapply(groups, function(group) condition_id %in% names(group$questions), logical(1)))
      if (is.null(condition_id) || !length(source_position) || source_position[1] >= i) settings$visibility <- NULL
      settings
    }
    groups[[group_id]]$settings <- clean(groups[[group_id]]$settings)
  }
  groups
}

remember_saved_ids <- function(groups, id_map) {
  for (group_id in names(groups)) {
    groups[[group_id]]$id <- unname(id_map$groups[group_id])
    for (question_id in names(groups[[group_id]]$questions)) {
      groups[[group_id]]$questions[[question_id]]$id <- unname(id_map$questions[question_id])
    }
  }
  groups
}

question_options_ui <- function(question_id, question) {
  settings <- question$settings
  if (is.null(settings)) settings <- list()
  display <- if (is.null(settings$display) || !length(settings$display)) "radio" else as.character(settings$display[1])
  randomize_id <- paste0("question_randomize_choices_", question_id)
  required_id <- paste0("question_required_", question_id)
  display_id <- paste0("question_edit_display_", question_id)
  long_text_id <- paste0("question_long_text_", question_id)

  tagList(
    conditionalPanel(
      condition = sprintf("input['question_edit_type_%s'] == 'single_numeric'", question_id),
      selectInput(paste0(display_id, "_numeric"), "表示方法",
                  choices = c("縦並び" = "radio", "横並び" = "horizontal", "プルダウン" = "dropdown", "スライダー" = "slider"),
                  selected = if (display %in% c("radio", "horizontal", "dropdown", "slider")) display else "radio")
    ),
    conditionalPanel(
      condition = sprintf("input['question_edit_type_%s'] == 'single_numeric' && input['%s_numeric'] == 'horizontal'", question_id, display_id),
      textInput(paste0("question_min_label_", question_id), "最小値側の説明", value = settings$min_label),
      textInput(paste0("question_max_label_", question_id), "最大値側の説明", value = settings$max_label)
    ),
    conditionalPanel(
      condition = sprintf("input['question_edit_type_%s'] == 'single_text'", question_id),
      selectInput(paste0(display_id, "_text"), "表示方法",
                  choices = c("縦並び" = "radio", "横並び" = "horizontal", "プルダウン" = "dropdown"),
                  selected = if (display %in% c("radio", "horizontal", "dropdown")) display else "radio")
    ),
    conditionalPanel(
      condition = sprintf("input['question_edit_type_%s'] == 'multiple'", question_id),
      selectInput(paste0(display_id, "_multiple"), "表示方法",
                  choices = c("縦並び" = "radio", "横並び" = "horizontal"),
                  selected = if (identical(settings$display, "horizontal")) "horizontal" else "radio")
    ),
    conditionalPanel(
      condition = sprintf("input['question_edit_type_%s'] == 'text'", question_id),
      checkboxInput(long_text_id, "長文入力", isTRUE(settings$long_text))
    ),
    conditionalPanel(
      condition = sprintf("input['question_edit_type_%s'] == 'single_numeric' || input['question_edit_type_%s'] == 'single_text' || input['question_edit_type_%s'] == 'multiple'", question_id, question_id, question_id),
      checkboxInput(randomize_id, "選択肢をランダム表示", isTRUE(settings$randomize_choices))
    ),
    checkboxInput(required_id, "回答必須", if (is.null(settings$required)) TRUE else isTRUE(settings$required)),
    NULL
  )
}

question_editor_panel_ui <- function(question_id, question) {
  suffix <- question_id
  numeric_options <- suppressWarnings(as.numeric(unlist(question$options)))
  numeric_options <- numeric_options[is.finite(numeric_options)]
  min_default <- if (!is.null(question$min)) question$min else if (length(numeric_options)) min(numeric_options) else 1
  max_default <- if (!is.null(question$max)) question$max else if (length(numeric_options)) max(numeric_options) else 5
  tagList(
    textInput(paste0("question_edit_title_", suffix), "設問タイトル", value = question$title),
    textAreaInput(paste0("question_edit_desc_", suffix), "説明（任意）", value = question$desc),
    selectInput(paste0("question_edit_type_", suffix), "回答形式",
      choices = c("択一選択（数字）" = "single_numeric", "択一選択（テキスト）" = "single_text", "複数選択" = "multiple", "自由記載（数字）" = "numeric", "自由記載（テキスト）" = "text", "日付" = "date"),
      selected = question$type
    ),
    conditionalPanel(
      condition = sprintf("input['question_edit_type_%s'] == 'single_text' || input['question_edit_type_%s'] == 'multiple'", suffix, suffix),
      selectizeInput(paste0("question_edit_options_", suffix), "選択肢",
        choices = unlist(question$options, use.names = FALSE),
        selected = unlist(question$options, use.names = FALSE), multiple = TRUE,
        options = list(create = TRUE, persist = FALSE, placeholder = "選択肢を入力", plugins = list("remove_button")))
    ),
    conditionalPanel(
      condition = sprintf("input['question_edit_type_%s'] == 'single_numeric'", suffix),
      numericInput(paste0("question_edit_min_", suffix), "下限", value = min_default, step = 1),
      numericInput(paste0("question_edit_max_", suffix), "上限", value = max_default, step = 1)
    ),
    conditionalPanel(
      condition = sprintf("input['question_edit_type_%s'] == 'numeric'", suffix),
      numericInput(paste0("question_edit_numeric_min_", suffix), "下限（任意）", value = question$min, step = "any"),
      numericInput(paste0("question_edit_numeric_max_", suffix), "上限（任意）", value = question$max, step = "any")
    ),
    NULL
  )
}

question_detail_panel_ui <- function(question_id, question) {
  div(class = "question-detail-panel",
    div(class = "question-edit-column", h4("設問内容"), question_editor_panel_ui(question_id, question)),
    div(class = "question-options-column", h4("オプション"), question_options_ui(question_id, question))
  )
}

question_group_settings_ui <- function(group_id, group, previous_questions) {
  settings <- if (is.null(group$settings) || !is.list(group$settings)) list() else group$settings
  condition <- settings$visibility
  randomize_questions_id <- paste0("group_randomize_questions_", group_id)
  has_visibility_id <- paste0("group_has_visibility_", group_id)
  condition_question_id <- paste0("group_condition_question_", group_id)
  condition_operator_id <- paste0("group_condition_operator_", group_id)
  condition_value_id <- paste0("group_condition_value_", group_id)

  condition_choices <- if (length(previous_questions)) {
    titles <- vapply(previous_questions, function(question) {
      title <- question$title
      if (is.null(title) || length(title) == 0 || is.na(title[[1]])) "（無題の設問）" else as.character(title[[1]])
    }, character(1))
    setNames(names(previous_questions), titles)
  } else {
    character()
  }

  tagList(
    textInput(paste0("group_title_", group_id), "グループ名", value = group$title),
    checkboxInput(paste0("group_page_", group_id), "このグループをページとして表示", isTRUE(settings$page)),
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
    NULL
  )
}

question_group_board_ui <- function(groups, editing_question_options) {
  group_order <- names(groups)
  question_order <- names(flatten_survey_groups(groups))
  if (!length(group_order)) {
    return(tagList(
      p("設問はまだありません。"),
      insert_question_button("new", final = TRUE),
      actionButton("add_group", "グループを追加", class = "group-add-button")
    ))
  }

  group_cards <- lapply(group_order, function(group_id) {
    group <- groups[[group_id]]
    question_ids <- names(group$questions)
    group_index <- match(group_id, group_order)
    previous_group_ids <- if (group_index > 1) group_order[seq_len(group_index - 1)] else character()
    previous_questions <- flatten_survey_groups(groups[previous_group_ids])
    question_items <- lapply(seq_along(question_ids), function(question_index) {
      question_id <- question_ids[question_index]
      question <- group$questions[[question_id]]
      title <- if (is.null(question$title) || is.na(question$title) || !nzchar(question$title)) {
        "設問タイトルを入力"
      } else question$title
      settings_open <- question_id %in% editing_question_options
      question_number <- match(question_id, question_order)
      up_disabled <- if (length(question_ids) > 1) question_index == 1 else group_index == 1
      down_disabled <- if (length(question_ids) > 1) question_index == length(question_ids) else group_index == length(group_order)

      tagList(
        insert_question_button(paste("in", group_id, question_id, sep = "|")),
        div(class = "question-row", `data-question-id` = question_id,
          span(class = "question-number", paste0(question_number, ".")),
          div(class = paste("question-card", if (settings_open) "question-card-open" else ""), `data-question-id` = question_id,
            div(class = "question-card-heading",
              span(class = "drag-grip", `aria-hidden` = "true", "⠿"),
              strong(title),
              span(class = "question-type", switch(question$type,
                single_numeric = "択一選択（数字）", single_text = "択一選択（テキスト）",
                numeric = "自由記載（数字）", text = "自由記載（テキスト）",
                multiple = "複数選択", date = "日付", question$type
              ))
            ),
            if (!is.null(question$desc) && length(question$desc) && !is.na(question$desc) && nzchar(question$desc)) {
              div(class = "question-description", question$desc)
            },
            if (!is.null(question$options) && question$type != "single_numeric") div(class = "question-options", paste("選択肢:", paste(unlist(question$options), collapse = ", "))),
            if (!is.null(question$min) && question$type %in% c("single_numeric", "numeric")) div(class = "question-options", paste0("範囲: ", question$min, "〜", question$max)),
            div(class = "question-actions",
              board_action("move_question", paste(question_id, "up", sep = "|"), "↑", length(question_ids) == 1 || up_disabled),
              board_action("move_question", paste(question_id, "down", sep = "|"), "↓", length(question_ids) == 1 || down_disabled),
              board_action("question_action", paste("copy", question_id, sep = "|"), "複製"),
              board_action("question_action", paste("delete", question_id, sep = "|"), "削除", danger = TRUE)
            ),
            question_detail_panel_ui(question_id, question)
          )
        )
      )
    })

    div(class = "group-card multi-question-group",
      `data-group-id` = group_id,
      div(class = "group-options-row",
        if (!is.null(group$title) && length(group$title) && !is.na(group$title[1]) && nzchar(trimws(as.character(group$title[1])))) {
          strong(class = "group-title", group$title)
        },
        div(class = "group-actions",
          board_action("group_action", paste("move", group_id, "up", sep = "|"), "↑", group_index == 1),
          board_action("group_action", paste("move", group_id, "down", sep = "|"), "↓", group_index == length(group_order)),
          board_action("group_action", paste("copy", group_id, sep = "|"), "グループ複製"),
          board_action("group_action", paste("delete", group_id, sep = "|"), "グループごと削除", danger = TRUE),
          board_action("toggle_group_settings", group_id, "グループ設定")
        )
      ),
      div(class = "question-list", `data-group-id` = group_id,
        question_items,
        insert_question_button(paste("after_group", group_id, sep = "|"), final = TRUE)
      )
    )
  })

  div(class = "group-board", group_cards,
    actionButton("add_group", "グループを追加", class = "group-add-button")
  )
}
