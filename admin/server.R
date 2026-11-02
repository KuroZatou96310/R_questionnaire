library(shiny)
library(qrcode)
source("db.R")

likert_preset_questions <- function(items, max_score = 5, prefix = "") {
  lapply(seq_along(items), function(i) list(
    title = paste0(prefix, items[[i]]),
    desc = paste0("1〜", max_score, "で回答してください。"),
    type = "single",
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

# Define server logic required to draw a histogram
shinyServer(function(input, output, session) {
  
  # 作業中アンケート（順序付き）
  store <- reactiveValues(
    order = character(0),
    questions = list(),
    group_order = character(0),
    groups = list()
  )
  current_id <- reactiveVal(NULL)
  editing_group <- reactiveVal(NULL)
  edit_options_target <- reactiveVal(NULL)
  editor_question_id <- reactiveVal(NULL)
  editing_question_options <- reactiveVal(NULL)
  editing_question_tab <- reactiveVal("options")
  new_question_ids <- reactiveVal(character())

  clone_survey_state <- function() {
    question_ids <- names(store$questions)
    group_ids <- names(store$groups)
    question_map <- setNames(vapply(question_ids, function(id) UUIDgenerate(), character(1)), question_ids)
    group_map <- setNames(vapply(group_ids, function(id) UUIDgenerate(), character(1)), group_ids)

    questions <- lapply(question_ids, function(old_id) {
      question <- store$questions[[old_id]]
      question$id <- unname(question_map[[old_id]])
      question$group_id <- unname(group_map[[question$group_id]])
      question
    })
    names(questions) <- unname(question_map[question_ids])

    groups <- lapply(group_ids, function(old_id) {
      group <- store$groups[[old_id]]
      group$id <- unname(group_map[[old_id]])
      if (!is.null(group$settings$visibility$question_id)) {
        old_question_id <- group$settings$visibility$question_id
        group$settings$visibility$question_id <- unname(question_map[[old_question_id]])
      }
      group
    })
    names(groups) <- unname(group_map[group_ids])

    list(
      questions = questions,
      order = unname(question_map[store$order]),
      groups = groups,
      group_order = unname(group_map[store$group_order])
    )
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
    current_id(sid)
    updateNumericInput(session, "random_start", value = load_random_start(sid))
    showNotification("アンケートを読み込みました", type = "message")
  })

  # ---------- 標準化プリセット追加 ----------
  observeEvent(input$add_preset, {

    preset <- standard_presets[[input$preset_name]]
    req(preset)

    qs <- store$questions
    ids <- character(0)

    for (template in preset$questions) {
      qid <- UUIDgenerate()
      group_id <- UUIDgenerate()
      template$id <- qid
      template$group_id <- group_id
      template$settings <- list(randomize_choices = FALSE, required = TRUE)
      qs[[qid]] <- template
      groups <- store$groups
      groups[[group_id]] <- list(id = group_id, title = paste0("設問グループ", length(store$group_order) + 1), settings = list(page = "group"))
      store$groups <- groups
      store$group_order <- c(store$group_order, group_id)
      ids <- c(ids, qid)
    }

    store$questions <- qs
    store$order <- c(store$order, ids)
    showNotification(paste0(preset$title, "を追加しました。保存して反映してください。"), type = "message")

  })

  observeEvent(input$toggle_group_settings, {
    group_id <- input$toggle_group_settings
    if (identical(editing_group(), group_id)) editing_group(NULL) else editing_group(group_id)
  })

  observeEvent(input$group_layout, {
    layout <- tryCatch(jsonlite::fromJSON(input$group_layout, simplifyVector = FALSE), error = function(e) NULL)
    if (is.null(layout) || !length(layout)) return()

    group_ids <- vapply(layout, function(group) as.character(group$group_id), character(1))
    question_ids <- unlist(lapply(layout, function(group) unlist(group$question_ids, use.names = FALSE)), use.names = FALSE)
    if (anyDuplicated(group_ids) || anyDuplicated(question_ids) ||
        length(question_ids) != length(store$order) ||
        !setequal(question_ids, store$order)) {
      showNotification("ドラッグ後の配置を反映できませんでした。", type = "error")
      return()
    }

    questions <- store$questions
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
        settings = list(page = "group")
      )
    }
    for (group_id in group_ids) {
      condition <- groups[[group_id]]$settings$visibility
      if (is.null(condition)) next
      source_question <- questions[[condition$question_id]]
      if (is.null(source_question) ||
          match(source_question$group_id, group_ids) >= match(group_id, group_ids)) {
        groups[[group_id]]$settings$visibility <- NULL
      }
    }

    groups <- groups[group_ids]
    store$group_order <- group_ids
    store$order <- question_ids
    store$questions <- questions
    store$groups <- groups
    if (!is.null(editing_group()) && !editing_group() %in% group_ids) editing_group(NULL)
  })

  observeEvent(input$save_group_settings_for, {
    group_id <- input$save_group_settings_for
    req(group_id, group_id %in% store$group_order)
    has_visibility <- isTRUE(input[[paste0("group_has_visibility_", group_id)]])
    settings <- list(
      page = "group",
      randomize_questions = isTRUE(input[[paste0("group_randomize_questions_", group_id)]]),
    )
    if (has_visibility) {
      condition_question <- input[[paste0("group_condition_question_", group_id)]]
      condition_value <- input[[paste0("group_condition_value_", group_id)]]
      if (is.null(condition_question) || !nzchar(condition_value)) {
        showNotification("表示条件に設問と回答値を指定してください。", type = "error")
        return()
      }
      settings$visibility <- list(
        question_id = condition_question,
        operator = input[[paste0("group_condition_operator_", group_id)]],
        value = condition_value
      )
    }
    groups <- store$groups
    groups[[group_id]]$settings <- settings
    store$groups <- groups
    showNotification("グループ設定を反映しました。アンケート保存で確定します。", type = "message")
  })
  
  # ---------- 設問の挿入・編集 ----------
  observeEvent(input$edit_question, {
    qid <- input$edit_question
    req(qid %in% names(store$questions))
    editor_question_id(qid)
    editing_question_options(qid)
    editing_question_tab("edit")
  })

  observeEvent(input$select_question_panel_tab, {
    values <- strsplit(input$select_question_panel_tab, "|", fixed = TRUE)[[1]]
    req(length(values) == 2, values[1] %in% names(store$questions))
    editor_question_id(if (values[2] == "edit") values[1] else NULL)
    editing_question_options(values[1])
    editing_question_tab(values[2])
  })

  observeEvent(input$insert_question, {
    payload <- strsplit(input$insert_question, "|", fixed = TRUE)[[1]]
    kind <- payload[1]
    qid <- UUIDgenerate()
    new_group <- kind %in% c("new", "before_group", "after_group")
    groups <- store$groups
    group_order <- store$group_order
    order <- store$order

    if (new_group) {
      group_id <- UUIDgenerate()
      position <- if (kind == "before_group") match(payload[2], group_order) else if (kind == "after_group") match(payload[2], group_order) + 1 else length(group_order) + 1
      if (is.na(position)) position <- length(group_order) + 1
      groups[[group_id]] <- list(id = group_id, title = paste0("設問グループ", length(groups) + 1), settings = list(page = "group"))
      group_order <- append(group_order, group_id, after = position - 1)
      insert_at <- if (kind == "before_group") match(payload[2], order) else if (kind == "after_group") match(tail(order[vapply(store$questions[order], function(q) identical(q$group_id, payload[2]), logical(1))], 1), order) + 1 else length(order) + 1
      if (is.na(insert_at)) insert_at <- length(order) + 1
    } else {
      group_id <- payload[2]
      before_id <- if (length(payload) >= 3) payload[3] else ""
      insert_at <- if (nzchar(before_id)) match(before_id, order) else max(which(vapply(store$questions[order], function(q) identical(q$group_id, group_id), logical(1)))) + 1
      if (!length(insert_at) || is.na(insert_at)) insert_at <- length(order) + 1
    }

    questions <- store$questions
    questions[[qid]] <- list(id = qid, group_id = group_id, title = "", desc = "", type = "text", options = NULL,
                             settings = list(randomize_choices = FALSE, required = TRUE), min = NULL, max = NULL)
    store$questions <- questions
    store$order <- append(order, qid, after = insert_at - 1)
    store$groups <- groups
    store$group_order <- group_order
    new_question_ids(c(new_question_ids(), qid))
    editor_question_id(qid)
    editing_question_options(qid)
    editing_question_tab("edit")
  })

  observeEvent(input$save_question_edit_for, {
    qid <- input$save_question_edit_for
    req(qid %in% names(store$questions))
    title <- trimws(input[[paste0("question_edit_title_", qid)]])
    if (is.null(title) || !nzchar(title)) {
      showNotification("設問タイトルを入力してください。", type = "error")
      return()
    }
    type <- input[[paste0("question_edit_type_", qid)]]
    options <- NULL
    if (type %in% c("single", "multiple", "select")) {
      option_text <- input[[paste0("question_edit_options_", qid)]]
      options <- trimws(strsplit(gsub("\r\n?", "\n", if (is.null(option_text)) "" else option_text), "\n", fixed = TRUE)[[1]])
      options <- options[nzchar(options)]
      if (!length(options)) {
        showNotification("選択肢を1つ以上入力してください。", type = "error")
        return()
      }
    }
    minv <- maxv <- NULL
    if (type == "slider") {
      minv <- input[[paste0("question_edit_min_", qid)]]
      maxv <- input[[paste0("question_edit_max_", qid)]]
      if (is.null(maxv) || maxv <= minv) {
        showNotification("上限は下限より大きくしてください。", type = "error")
        return()
      }
    }
    questions <- store$questions
    q <- questions[[qid]]
    q$title <- title
    q$desc <- input[[paste0("question_edit_desc_", qid)]]
    q$type <- type
    q$options <- options
    q$min <- minv
    q$max <- maxv
    questions[[qid]] <- q
    store$questions <- questions
    new_question_ids(setdiff(new_question_ids(), qid))
    editor_question_id(qid)
    showNotification("設問を反映しました。アンケート保存で確定します。", type = "message")
  })

  observeEvent(input$cancel_question_edit_for, {
    qid <- input$cancel_question_edit_for
    if (qid %in% new_question_ids()) {
      questions <- store$questions
      group_id <- questions[[qid]]$group_id
      questions[[qid]] <- NULL
      groups <- store$groups
      if (!any(vapply(questions, function(q) identical(q$group_id, group_id), logical(1)))) {
        groups[[group_id]] <- NULL
        store$group_order <- setdiff(store$group_order, group_id)
      }
      store$questions <- questions
      store$order <- setdiff(store$order, qid)
      store$groups <- groups
      new_question_ids(setdiff(new_question_ids(), qid))
    }
    if (identical(editor_question_id(), qid)) editor_question_id(NULL)
    if (identical(editing_question_options(), qid)) editing_question_options(NULL)
  })

  observeEvent(input$save_question_options_for, {
    qid <- input$save_question_options_for
    req(qid %in% names(store$questions))
    questions <- store$questions
    questions[[qid]]$settings <- list(
      randomize_choices = isTRUE(input[[paste0("question_randomize_choices_", qid)]]),
      required = isTRUE(input[[paste0("question_required_", qid)]])
    )
    if (identical(editor_question_id(), qid)) editor_question_id(NULL)
    store$questions <- questions
    showNotification("設問オプションを反映しました。アンケート保存で確定します。", type = "message")
  })

  # ---------- 削除 ----------
  # FIX: NULL代入ではなく取り出して差し替える
  observeEvent(input$delete_q, ignoreInit = TRUE, {
    id <- input$delete_q
    req(nzchar(id))
    
    group_id <- store$questions[[id]]$group_id
    qs <- store$questions
    qs[[id]] <- NULL
    store$questions <- qs
    store$order     <- store$order[store$order != id]
    groups <- store$groups
    group_order <- store$group_order
    if (!any(vapply(qs, function(question) identical(question$group_id, group_id), logical(1)))) {
      groups[[group_id]] <- NULL
      group_order <- setdiff(group_order, group_id)
      if (identical(editing_group(), group_id)) editing_group(NULL)
    }
    for (group_id in store$group_order) {
      condition <- groups[[group_id]]$settings$visibility
      if (!is.null(condition) && identical(condition$question_id, id)) {
        groups[[group_id]]$settings$visibility <- NULL
      }
    }
    store$groups <- groups
    store$group_order <- group_order
  })

  # ---------- 選択肢の改変 ----------
  observeEvent(input$edit_options_qid, {

    qid <- input$edit_options_qid
    req(nzchar(qid), qid %in% names(store$questions))

    edit_options_target(qid)
    showModal(modalDialog(
      title = paste0("選択肢を改変：", store$questions[[qid]]$title),
      textAreaInput(
        "edit_options_text",
        "※アンケート実施途中に選択肢を変更すると、変更前後の回答を正確に比較できず、統計分析に影響する場合があります。",
        value = paste(as.character(unlist(store$questions[[qid]]$options)), collapse = "\n"),
        placeholder = "例：とても満足\nやや満足",
        rows = 8
      ),
      footer = tagList(
        modalButton("キャンセル"),
        actionButton("edit_options_save", "この内容で上書き")
      ),
      easyClose = TRUE
    ))

  })

  observeEvent(input$edit_options_save, {

    qid <- edit_options_target()
    req(qid, qid %in% names(store$questions))

    edit_text <- if (is.null(input$edit_options_text)) "" else input$edit_options_text
    options <- strsplit(gsub("\r\n?", "\n", edit_text), "\n", fixed = TRUE)[[1]]
    options <- unique(trimws(options))
    options <- options[nzchar(options)]

    if (length(options) == 0) {
      showNotification("選択肢を1つ以上入力してください", type = "error")
      return()
    }

    q <- store$questions[[qid]]
    q$options <- options
    qs <- store$questions
    qs[[qid]] <- q
    store$questions <- qs

    showNotification("選択肢を変更しました。アンケート保存で確定します。", type = "message")

    removeModal()

  })
  
  # ---------- 設問一覧 UI ----------
  output$question_list_ui <- renderUI({
    question_group_board_ui(
      groups = store$groups,
      group_order = store$group_order,
      questions = store$questions,
      question_order = store$order,
      editing_group = editing_group(),
      editing_question_options = editing_question_options(),
      editing_question_tab = editing_question_tab()
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
              "multiple" = checkboxGroupInput(paste0("p_", id), label, choices = opts),
              "select"   = selectInput(paste0("p_", id), label, choices = opts),
              "numeric"  = numericInput(paste0("p_", id), label, value = NA),
              "text"     = textAreaInput(paste0("p_", id), label),
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

    sid <- trimws(input$survey_id)
    if (!nzchar(sid)) sid <- paste0("survey_", UUIDgenerate())
    previous_sid <- current_id()
    is_copy <- !is.null(previous_sid) && nzchar(previous_sid) && !identical(previous_sid, sid)

    if (is_copy && sid %in% names(load_db())) {
      showNotification("変更先のアンケートIDはすでに使われています。", type = "error")
      return()
    }

    random_start <- input$random_start
    if (is.null(random_start) || is.na(random_start) || random_start < 1 || random_start > length(store$order)) {
      random_start <- NULL
    } else {
      random_start <- as.integer(random_start)
    }

    tryCatch({
      saved_state <- if (is_copy) clone_survey_state() else list(
        questions = store$questions,
        order = store$order,
        groups = store$groups,
        group_order = store$group_order
      )

      save_db(
        survey_id = sid,
        password_hash = hash_pw(input$survey_pw),
        questions_list = saved_state$questions,
        question_ids = saved_state$order,
        random_start = random_start,
        groups_list = saved_state$groups,
        group_ids = saved_state$group_order
      )

      if (is_copy) {
        store$questions <- saved_state$questions
        store$order <- saved_state$order
        store$groups <- saved_state$groups
        store$group_order <- saved_state$group_order
        editor_question_id(NULL)
        editing_question_options(NULL)
        editing_group(NULL)
      }
      new_question_ids(character())
      current_id(sid)
      updateTextInput(session, "survey_id", value = sid)
      showNotification(paste("保存しました（ID:", sid, ")"), type = "message")
    }, error = function(e) {
      showNotification(paste("保存エラー:", e$message), type = "error")
    })
  })

})
