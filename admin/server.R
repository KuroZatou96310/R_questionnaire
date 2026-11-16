library(shiny)
library(qrcode)
source("../common/db.R")

likert_preset_questions <- function(items, max_score = 5, prefix = "") {
  lapply(seq_along(items), function(i) list(
    title = paste0(prefix, items[[i]]),
    desc = paste0("1〜", max_score, "で回答してください。"),
    type = "single_numeric",
    options = as.character(seq_len(max_score)),
    min = NULL,
    max = NULL
  ))
}

standard_presets <- list(
  SUS = list(
    title = "SUS（System Usability Scale）",
    questions = likert_preset_questions(c(
      "頻繁にこのシステムを使いたいと思う。",
      "このシステムは不必要に複雑だと感じた。",
      "このシステムは使いやすいと思った。",
      "このシステムを使うには専門家のサポートが必要だと思う。",
      "このシステムの機能はよく統合されていると感じた。",
      "このシステムには一貫性がないと感じた。",
      "多くの人がすぐに使えるようになると思う。",
      "このシステムを使うのは非常に面倒だと感じた。",
      "このシステムを自信を持って使えると感じた。",
      "使い始める前に多くのことを学ぶ必要があると感じた。"
    ), 5, "SUS ")
  ),
  `UMUX-Lite` = list(
    title = "UMUX-Lite",
    questions = likert_preset_questions(c(
      "このシステムの機能は、私の要求を満たしている。",
      "このシステムは使いやすい。"
    ), 7, "UMUX-Lite ")
  ),
  UEQ = list(
    title = "UEQ（全26項目）",
    questions = likert_preset_questions(c(
      "煩わしい — 楽しい", "理解しにくい — 理解しやすい", "創造的でない — 創造的な",
      "学びにくい — 学びやすい", "価値がない — 価値がある", "退屈な — 刺激的な",
      "面白くない — 面白い", "予測できない — 予測できる", "遅い — 速い",
      "独創的でない — 独創的な", "妨げになる — 支援してくれる", "悪い — 良い",
      "複雑な — 簡単な", "好ましくない — 好ましい", "一般的な — 先進的な",
      "不快な — 快適な", "安全でない — 安全な", "やる気を起こさせない — やる気を起こさせる",
      "期待に合わない — 期待に合う", "非効率的な — 効率的な", "わかりにくい — わかりやすい",
      "実用的でない — 実用的な", "整理されていない — 整理された",
      "魅力的でない — 魅力的な", "親しみにくい — 親しみやすい", "保守的な — 革新的な"
    ), 7, "UEQ ")
  ),
  `UEQ-S` = list(
    title = "UEQ-S（短縮版）",
    questions = likert_preset_questions(c(
      "妨げになる — 支援してくれる",
      "複雑な — 簡単な",
      "退屈な — 面白い",
      "典型的な — 斬新な",
      "不快な — 快適な",
      "保守的な — 革新的な",
      "遅い — 速い",
      "わかりにくい — わかりやすい"
    ), 7, "UEQ-S ")
  ),
  `NASA-TLX` = list(
    title = "NASA-TLX",
    questions = lapply(c("知的・知覚的要求", "身体的要求", "時間的切迫感", "作業達成度", "努力", "不満・ストレス"), function(item) list(
      title = paste0("NASA-TLX：", item),
      desc = "0（低い）〜100（高い）で回答してください。",
      type = "slider", options = NULL, min = 0, max = 100
    ))
  ),
  TAM = list(
    title = "TAM（Technology Acceptance Model）",
    questions = likert_preset_questions(c(
      "このシステムを使うことで作業の成果が向上する。",
      "このシステムは作業に役立つ。",
      "このシステムを使うことで作業が早くなる。",
      "このシステムの操作方法を身につけるのは簡単だ。",
      "このシステムは柔軟に操作できる。",
      "このシステム全体として使いやすい。"
    ), 7, "TAM ")
  ),
  GEQ = list(
    title = "GEQ（ゲーム体験質問紙の一般的評価項目）",
    questions = likert_preset_questions(c(
      "この体験は楽しかった。",
      "この体験に夢中になった。",
      "この体験に満足した。",
      "もう一度この体験をしたい。",
      "この体験は緊張を感じさせた。"
    ), 5, "GEQ ")
  ),
  IMI = list(
    title = "IMI（Intrinsic Motivation Inventory）",
    questions = likert_preset_questions(c(
      "この活動は楽しかった。",
      "この活動は面白かった。",
      "この活動は自分にとって重要だった。",
      "この活動に努力した。",
      "この活動を自分で選んでいると感じた。",
      "この活動に自信を持って取り組めた。",
      "この活動に集中できた。"
    ), 7, "IMI ")
  )
)

shinyServer(function(input, output, session) {
  
  # 作業中アンケート（順序付き）
  store <- reactiveValues(
    order = character(0),
    questions = list(),
    group_order = character(0),
    groups = list()
  )
  current_id <- reactiveVal(NULL)
  editing_question_options <- reactiveVal(NULL)
  active_group_settings <- reactiveVal(NULL)
  group_settings_draft <- reactiveVal(list())

  output$access_link_ui <- renderUI({
    survey_id <- current_id()
    if (is.null(survey_id) || !nzchar(survey_id)) return(NULL)
    tags$a(
      href = paste0("../Access/?id=", utils::URLencode(survey_id, reserved = TRUE)),
      target = "_blank",
      rel = "noopener",
      class = "btn btn-default",
      style = "width:100%; margin-top:10px;",
      "QRコードを生成"
    )
  })

  observeEvent(input$toggle_question, {
    question_id <- input$toggle_question
    req(question_id %in% names(store$questions))
    store$questions <- capture_question_editor_inputs(store$questions)
    editing_question_options(if (identical(editing_question_options(), question_id)) NULL else question_id)
  })

  capture_question_editor_inputs <- function(questions) {
    for (question_id in names(questions)) {
      question <- questions[[question_id]]
      title <- input[[paste0("question_edit_title_", question_id)]]
      description <- input[[paste0("question_edit_desc_", question_id)]]
      type <- input[[paste0("question_edit_type_", question_id)]]
      if (!is.null(title)) question$title <- title
      if (!is.null(description)) question$desc <- description
      if (!is.null(type)) question$type <- type

      if (question$type %in% c("single", "single_text", "multiple", "select")) {
        option_text <- input[[paste0("question_edit_options_", question_id)]]
        if (!is.null(option_text)) {
          options <- trimws(strsplit(option_text, "\n", fixed = TRUE)[[1]])
          options <- options[nzchar(options)]
          question$options <- if (length(options)) options else NULL
        }
      } else {
        question$options <- NULL
      }

      if (question$type %in% c("slider", "single_numeric", "numeric")) {
        for (field in c("min", "max")) {
          value <- input[[paste0("question_edit_", field, "_", question_id)]]
          if (is.null(value) || !length(value) || is.na(value)) question[[field]] <- NULL
          else question[[field]] <- suppressWarnings(as.numeric(value))
        }
      } else {
        question$min <- question$max <- NULL
      }

      randomize <- input[[paste0("question_randomize_choices_", question_id)]]
      required <- input[[paste0("question_required_", question_id)]]
      if (!is.null(randomize) || !is.null(required)) {
        settings <- question$settings
        if (is.null(settings)) settings <- list()
        if (!is.null(randomize)) settings$randomize_choices <- isTRUE(randomize)
        if (!is.null(required)) settings$required <- isTRUE(required)
        question$settings <- settings
      }
      display <- input[[paste0("question_edit_display_", question_id)]]
      if (!is.null(display)) {
        if (is.null(question$settings)) question$settings <- list()
        question$settings$display <- display
      }
      long_text <- input[[paste0("question_long_text_", question_id)]]
      if (!is.null(long_text)) {
        if (is.null(question$settings)) question$settings <- list()
        question$settings$long_text <- isTRUE(long_text)
      }
      questions[[question_id]] <- question
    }
    questions
  }


  # ---------- 既存アンケート読み込み ----------
  observeEvent(input$load_survey, {
    req(nzchar(input$survey_id), nzchar(input$survey_pw))
    
    db  <- load_db()
    sid <- input$survey_id
    
    if (!(sid %in% names(db))) {
      showNotification("指定IDのアンケートは存在しません", type = "error")
      return()
    }
    
    # パスワードチェック（ハッシュ比較）
    if (is.null(db[[sid]]$password) ||
        db[[sid]]$password != hash_pw(input$survey_pw)) {
      showNotification("パスワードが正しくありません", type = "error")
      return()
    }
    
    # SQLiteから質問を読み込む
    groups <- load_groups(sid)
    qlist <- load_questions(sid)
    
    if (length(qlist) == 0) {
      showNotification("設問が見つかりません", type = "error")
      return()
    }
    
    # id をキーにした名前付きリストに変換
    ids <- names(qlist)
    
    store$order     <- ids
    store$questions <- qlist
    store$groups <- groups
    store$group_order <- names(groups)
    group_settings_draft(list())
    editing_question_options(ids[[1]])
    current_id(sid)
    showNotification("アンケートを読み込みました", type = "message")
  })

  # ---------- 標準化プリセット追加 ----------
  observeEvent(input$add_preset, {

    preset <- standard_presets[[input$preset_name]]
    req(preset)

    store$questions <- capture_question_editor_inputs(store$questions)
    qs <- store$questions
    groups <- store$groups
    group_order <- store$group_order
    group_id <- UUIDgenerate()
    groups[[group_id]] <- list(id = group_id, title = next_group_title(groups), settings = list(page = FALSE))
    group_order <- c(group_order, group_id)
    ids <- character(0)

    for (template in preset$questions) {
      qid <- UUIDgenerate()
      template$id <- qid
      template$group_id <- group_id
      template$settings <- list(randomize_choices = FALSE, required = TRUE)
      qs[[qid]] <- template
      ids <- c(ids, qid)
    }

    store$groups <- groups
    store$group_order <- group_order
    store$questions <- qs
    store$order <- c(store$order, ids)
    showNotification(paste0(preset$title, "を追加しました。保存して反映してください。"), type = "message")

  })

  observeEvent(input$add_group, {
    store$questions <- capture_question_editor_inputs(store$questions)
    groups <- store$groups
    group_id <- UUIDgenerate()
    groups[[group_id]] <- list(id = group_id, title = next_group_title(groups), settings = list(page = FALSE))
    store$groups <- groups
    store$group_order <- c(store$group_order, group_id)
  })

  observeEvent(input$toggle_group_settings, {
    group_id <- input$toggle_group_settings
    req(group_id %in% store$group_order)
    group_index <- match(group_id, store$group_order)
    previous_group_ids <- if (group_index > 1) store$group_order[seq_len(group_index - 1)] else character()
    previous_ids <- store$order[vapply(store$questions[store$order], function(q) q$group_id %in% previous_group_ids, logical(1))]
    active_group_settings(group_id)
    group <- store$groups[[group_id]]
    drafts <- group_settings_draft()
    if (!is.null(drafts[[group_id]])) group$settings <- drafts[[group_id]]
    showModal(modalDialog(
      title = "グループ設定",
      question_group_settings_ui(group_id, group, store$questions[previous_ids]),
      footer = NULL,
      easyClose = TRUE
    ))
  })

  observe({
    group_id <- active_group_settings()
    req(!is.null(group_id), group_id %in% store$group_order)
    page <- input[[paste0("group_page_", group_id)]]
    randomize <- input[[paste0("group_randomize_questions_", group_id)]]
    has_visibility <- input[[paste0("group_has_visibility_", group_id)]]
    req(!is.null(page), !is.null(randomize), !is.null(has_visibility))

    settings <- list(page = isTRUE(page), randomize_questions = isTRUE(randomize))
    if (isTRUE(has_visibility)) {
      condition_question <- input[[paste0("group_condition_question_", group_id)]]
      condition_value <- input[[paste0("group_condition_value_", group_id)]]
      req(!is.null(condition_question), !is.null(condition_value), nzchar(condition_value))
      settings$visibility <- list(
        question_id = condition_question,
        operator = input[[paste0("group_condition_operator_", group_id)]],
        value = condition_value
      )
    }

    drafts <- group_settings_draft()
    if (!identical(drafts[[group_id]], settings)) {
      drafts[[group_id]] <- settings
      group_settings_draft(drafts)
    }
  })

  observeEvent(input$move_question, {
    parts <- strsplit(input$move_question, "|", fixed = TRUE)[[1]]
    req(length(parts) == 2, parts[1] %in% names(store$questions))
    qid <- parts[1]
    store$questions <- capture_question_editor_inputs(store$questions)
    group_id <- store$questions[[qid]]$group_id
    ids <- store$order[vapply(store$questions[store$order], function(q) identical(q$group_id, group_id), logical(1))]
    if (length(ids) > 1) {
      i <- match(qid, ids) + if (parts[2] == "down") 1 else -1
      if (i < 1 || i > length(ids)) return()
      positions <- which(store$order %in% ids)
      ids <- ids[order(match(ids, store$order))]
      old <- match(qid, ids)
      ids[c(old, i)] <- ids[c(i, old)]
      order <- store$order
      order[positions] <- ids
      store$order <- order
    } else {
      i <- match(group_id, store$group_order) + if (parts[2] == "down") 1 else -1
      if (i < 1 || i > length(store$group_order)) return()
      group_order <- store$group_order
      old <- match(group_id, group_order)
      group_order[c(old, i)] <- group_order[c(i, old)]
      store$group_order <- group_order
    }
  })

  observeEvent(input$group_layout, {
    payload <- tryCatch(jsonlite::fromJSON(input$group_layout, simplifyVector = FALSE), error = function(e) NULL)
    if (is.null(payload)) return()
    layout <- if (is.list(payload) && !is.null(payload$groups)) payload$groups else payload
    if (is.null(layout) || !length(layout)) return()

    group_ids <- vapply(layout, function(group) as.character(group$group_id), character(1))
    question_ids <- unlist(lapply(layout, function(group) unlist(group$question_ids, use.names = FALSE)), use.names = FALSE)
    if (anyNA(group_ids) || any(!nzchar(group_ids)) || anyDuplicated(group_ids) ||
        anyDuplicated(question_ids) || any(!nzchar(question_ids)) ||
        length(question_ids) != length(store$order) ||
        !setequal(question_ids, store$order)) {
      showNotification("ドラッグ後の配置を反映できませんでした。", type = "error")
      return()
    }

    questions <- capture_question_editor_inputs(store$questions)
    question_drafts <- if (is.list(payload) && !is.null(payload$question_drafts)) payload$question_drafts else list()
    for (draft in question_drafts) {
      question_id <- as.character(draft$id)
      if (length(question_id) != 1 || is.na(question_id) || !question_id %in% names(questions)) next
      question <- questions[[question_id]]
      for (field in c("title", "desc", "type")) {
        if (!is.null(draft[[field]])) question[[field]] <- as.character(draft[[field]])
      }
      if (!is.null(draft$options)) {
        options <- trimws(strsplit(as.character(draft$options), "\n", fixed = TRUE)[[1]])
        options <- options[nzchar(options)]
        question$options <- if (length(options)) options else NULL
      }
      for (field in c("min", "max")) {
        value <- draft[[field]]
        value_text <- if (is.null(value) || !length(value)) "" else as.character(value)
        if (length(value_text) == 1 && !is.na(value_text) && nzchar(value_text)) {
          question[[field]] <- suppressWarnings(as.numeric(value))
        }
      }
      if (!is.null(draft$randomize_choices) || !is.null(draft$required)) {
        settings <- question$settings
        if (is.null(settings)) settings <- list()
        question$settings <- modifyList(settings, list(
          randomize_choices = isTRUE(draft$randomize_choices),
          required = isTRUE(draft$required)
        ))
      }
      questions[[question_id]] <- question
    }
    for (group in layout) {
      group_id <- as.character(group$group_id)
      for (question_id in unlist(group$question_ids, use.names = FALSE)) {
        questions[[question_id]]$group_id <- group_id
      }
    }

    groups <- store$groups
    for (group_id in setdiff(group_ids, names(groups))) {
      groups[[group_id]] <- list(
        id = group_id,
        title = paste0("設問グループ", length(groups) + 1),
        settings = list(page = FALSE)
      )
    }
    for (group_id in group_ids) {
      condition <- groups[[group_id]]$settings$visibility
      if (is.null(condition)) next
      source_question <- questions[[condition$question_id]]
      source_position <- if (is.null(source_question)) NA_integer_ else match(source_question$group_id, group_ids)
      target_position <- match(group_id, group_ids)
      if (is.na(source_position) || is.na(target_position) || source_position >= target_position) {
        groups[[group_id]]$settings$visibility <- NULL
      }
    }

    groups <- groups[group_ids]
    store$group_order <- group_ids
    store$order <- question_ids
    store$questions <- questions
    store$groups <- groups
  })
  
  # ---------- 設問の挿入・編集 ----------
  observeEvent(input$insert_question, {
    payload <- strsplit(input$insert_question, "|", fixed = TRUE)[[1]]
    if (!length(payload) || !nzchar(payload[1])) return()
    kind <- payload[1]
    store$questions <- capture_question_editor_inputs(store$questions)
    qid <- UUIDgenerate()
    groups <- store$groups
    group_order <- store$group_order
    order <- store$order

    if (!length(group_order)) {
      group_id <- UUIDgenerate()
      groups[[group_id]] <- list(id = group_id, title = next_group_title(groups), settings = list(page = FALSE))
      group_order <- c(group_order, group_id)
    }
    if (kind == "in") {
      if (length(payload) < 3 || !payload[2] %in% group_order || !payload[3] %in% order ||
          !identical(store$questions[[payload[3]]]$group_id, payload[2])) return()
      group_id <- payload[2]
      insert_at <- match(payload[3], order)
    } else if (kind == "after_group" && length(payload) >= 2 && payload[2] %in% group_order) {
      group_id <- payload[2]
      group_positions <- which(vapply(store$questions[order], function(q) identical(q$group_id, group_id), logical(1)))
      insert_at <- if (length(group_positions)) tail(group_positions, 1) + 1 else length(order) + 1
    } else {
      group_id <- tail(group_order, 1)
      group_positions <- which(vapply(store$questions[order], function(q) identical(q$group_id, group_id), logical(1)))
      insert_at <- if (length(group_positions)) tail(group_positions, 1) + 1 else length(order) + 1
    }

    questions <- store$questions
    questions[[qid]] <- list(id = qid, group_id = group_id, title = "", desc = "", type = "text", options = NULL,
                             settings = list(randomize_choices = FALSE, required = TRUE), min = NULL, max = NULL)
    store$questions <- questions
    store$order <- append(order, qid, after = insert_at - 1)
    store$groups <- groups
    store$group_order <- group_order
    editing_question_options(qid)
  })

  # ---------- 削除 ----------
  observeEvent(input$delete_q, ignoreInit = TRUE, {
    id <- input$delete_q
    req(nzchar(id))
    
    req(id %in% names(store$questions))
    questions <- capture_question_editor_inputs(store$questions)
    qs <- questions
    qs[[id]] <- NULL
    store$questions <- qs
    store$order     <- store$order[store$order != id]
    if (identical(editing_question_options(), id)) {
      editing_question_options(if (length(store$order)) store$order[[1]] else NULL)
    }
    groups <- store$groups
    for (group_id in store$group_order) {
      condition <- groups[[group_id]]$settings$visibility
      if (!is.null(condition) && identical(condition$question_id, id)) {
        groups[[group_id]]$settings$visibility <- NULL
      }
    }
    store$groups <- groups
  })

  # ---------- 設問一覧 UI ----------
  output$question_list_ui <- renderUI({
    question_group_board_ui(
      groups = store$groups,
      group_order = store$group_order,
      questions = store$questions,
      question_order = store$order,
      editing_question_options = editing_question_options()
    )
  })
  
  # ---------- プレビュー ----------
  output$answer_preview_ui <- renderUI({
    if (length(store$order) == 0)
      return(p("設問が作成されるとプレビューが表示されます。"))
    
    tagList(
      lapply(store$group_order, function(group_id) {
        ids <- store$order[vapply(store$questions[store$order], function(q) identical(q$group_id, group_id), logical(1))]
        tagList(
          h3(store$groups[[group_id]]$title),
          lapply(ids, function(id) {
            q <- store$questions[[id]]
            label <- tags$div(
              strong(q$title),
              tags$div(style = "color:#666;font-size:0.9em;", q$desc)
            )
            opts <- unlist(q$options)
            switch(
              q$type,
              "single"   = radioButtons(paste0("p_", id), label, choices = opts),
              "single_numeric" = radioButtons(paste0("p_", id), label, choices = opts),
              "single_text" = if (identical(q$settings$display, "dropdown")) selectInput(paste0("p_", id), label, choices = opts) else radioButtons(paste0("p_", id), label, choices = opts),
              "multiple" = checkboxGroupInput(paste0("p_", id), label, choices = opts),
              "select"   = selectInput(paste0("p_", id), label, choices = opts),
              "numeric"  = numericInput(paste0("p_", id), label, value = NA, min = q$min, max = q$max, step = "any"),
              "text"     = if (isTRUE(q$settings$long_text)) textAreaInput(paste0("p_", id), label) else textInput(paste0("p_", id), label),
              "slider"   = sliderInput(paste0("p_", id), label, min = q$min, max = q$max, value = q$min),
              "date"     = dateInput(paste0("p_", id), label, value = Sys.Date())
            )
          })
        )
      })
    )
  })
  
  # ---------- 保存処理 ----------
  observeEvent(input$save_survey, {
    if (length(store$order) == 0) {
      showNotification("設問が1件もありません", type = "error")
      return()
    }
    if (is.null(input$survey_pw) || !nzchar(input$survey_pw)) {
      showNotification("パスワードを入力してください", type = "error")
      return()
    }

    questions_to_save <- store$questions
    for (qid in names(questions_to_save)) {
      title <- input[[paste0("question_edit_title_", qid)]]
      if (is.null(title)) next
      title <- trimws(title)
      if (!nzchar(title)) {
        showNotification("設問タイトルを入力してください。", type = "error")
        return()
      }
      type <- input[[paste0("question_edit_type_", qid)]]
      question <- questions_to_save[[qid]]
      question$title <- title
      question$desc <- input[[paste0("question_edit_desc_", qid)]]
      question$type <- type
      if (identical(type, "single_numeric")) {
        minimum <- input[[paste0("question_edit_min_", qid)]]
        maximum <- input[[paste0("question_edit_max_", qid)]]
        if (is.null(minimum) || is.null(maximum) || is.na(minimum) || is.na(maximum) || minimum > maximum || minimum != floor(minimum) || maximum != floor(maximum)) {
          showNotification("数字択一の範囲は整数で指定してください。", type = "error")
          return()
        }
        question$min <- minimum
        question$max <- maximum
        question$options <- as.character(seq.int(minimum, maximum))
      } else if (type %in% c("single", "single_text", "multiple", "select")) {
        option_text <- input[[paste0("question_edit_options_", qid)]]
        options <- trimws(strsplit(gsub("\r\n?", "\n", if (is.null(option_text)) "" else option_text), "\n", fixed = TRUE)[[1]])
        options <- options[nzchar(options)]
        if (!length(options)) {
          showNotification("選択肢を1つ以上入力してください。", type = "error")
          return()
        }
        question$options <- options
      } else question$options <- NULL
      if (type %in% c("slider", "numeric")) {
        minimum <- input[[paste0("question_edit_min_", qid)]]
        maximum <- input[[paste0("question_edit_max_", qid)]]
        if (is.null(minimum) || !length(minimum) || is.na(minimum)) minimum <- NULL
        if (is.null(maximum) || !length(maximum) || is.na(maximum)) maximum <- NULL
        question$min <- minimum
        question$max <- maximum
        if (type == "slider" && (is.null(question$max) || question$max <= question$min)) {
          showNotification("上限は下限より大きくしてください。", type = "error")
          return()
        }
        if (type == "numeric" && !is.null(question$min) && !is.null(question$max) && question$min > question$max) {
          showNotification("上限は下限以上にしてください。", type = "error")
          return()
        }
      } else if (type != "single_numeric") {
        question$min <- question$max <- NULL
      }
      if (!is.null(input[[paste0("question_randomize_choices_", qid)]])) {
        question$settings <- list(
          randomize_choices = isTRUE(input[[paste0("question_randomize_choices_", qid)]]),
          required = isTRUE(input[[paste0("question_required_", qid)]]),
          display = input[[paste0("question_edit_display_", qid)]],
          long_text = isTRUE(input[[paste0("question_long_text_", qid)]])
        )
      }
      questions_to_save[[qid]] <- question
    }

    groups_to_save <- store$groups
    settings_draft <- group_settings_draft()
    for (group_id in store$group_order) {
      if (!is.null(settings_draft[[group_id]])) groups_to_save[[group_id]]$settings <- settings_draft[[group_id]]
    }

    sid <- trimws(input$survey_id)
    if (!nzchar(sid)) sid <- paste0("survey_", UUIDgenerate())
    previous_sid <- current_id()
    is_copy <- !is.null(previous_sid) && nzchar(previous_sid) && !identical(previous_sid, sid)

    if (is_copy && sid %in% names(load_db())) {
      showNotification("変更先のアンケートIDはすでに使われています。", type = "error")
      return()
    }

    tryCatch({
      saved_state <- if (is_copy) clone_survey_state(questions_to_save, store$order, groups_to_save, store$group_order) else list(
        questions = questions_to_save,
        order = store$order,
        groups = groups_to_save,
        group_order = store$group_order
      )

      save_db(
        survey_id = sid,
        password_hash = hash_pw(input$survey_pw),
        questions_list = saved_state$questions,
        question_ids = saved_state$order,
        groups_list = saved_state$groups,
        group_ids = saved_state$group_order
      )

      store$questions <- saved_state$questions
      store$order <- saved_state$order
      store$groups <- saved_state$groups
      store$group_order <- saved_state$group_order
      group_settings_draft(list())

      if (is_copy) {
        editing_question_options(if (length(store$order)) store$order[[1]] else NULL)
      }
      current_id(sid)
      updateTextInput(session, "survey_id", value = sid)
      showNotification(paste("保存しました（ID:", sid, ")"), type = "message")
    }, error = function(e) {
      showNotification(paste("保存エラー:", e$message), type = "error")
    })
  })

})
