library(shiny)
source("../common/db.R")
source("../common/survey-ui.R")

likert_preset_questions <- function(items, max_score = 5) {
  middle <- ceiling(max_score / 2)
  lapply(items, function(item) {
    ends <- trimws(strsplit(item, "—", fixed = TRUE)[[1]])
    scale <- if (length(ends) > 1) paste0("1：", ends[1], "　", middle, "：どちらともいえない　", max_score, "：", tail(ends, 1)) else paste0("1：そう思わない　", middle, "：どちらともいえない　", max_score, "：そう思う")
    list(title = item, desc = paste0("1〜", max_score, "で回答してください。 ", scale), type = "single_numeric",
         options = as.character(seq_len(max_score)), min = 1, max = max_score)
  })
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
    ), 5)
  ),
  `UMUX-Lite` = list(
    title = "UMUX-Lite",
    questions = likert_preset_questions(c(
      "このシステムの機能は、私の要求を満たしている。",
      "このシステムは使いやすい。"
    ), 7)
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
    ), 7)
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
    ), 7)
  ),
  `NASA-TLX` = list(
    title = "NASA-TLX",
    questions = lapply(c("知的・知覚的要求", "身体的要求", "時間的切迫感", "作業達成度", "努力", "不満・ストレス"), function(item) list(
      title = paste0("NASA-TLX：", item),
      desc = "0（低い）〜100（高い）で回答してください。",
      type = "single_numeric", options = as.character(0:100), min = 0, max = 100,
      settings = list(display = "slider")
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
    ), 7)
  ),
  GEQ = list(
    title = "GEQ（ゲーム体験質問紙の一般的評価項目）",
    questions = likert_preset_questions(c(
      "この体験は楽しかった。",
      "この体験に夢中になった。",
      "この体験に満足した。",
      "もう一度この体験をしたい。",
      "この体験は緊張を感じさせた。"
    ))
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
    ), 7)
  )
)

shinyServer(function(input, output, session) {
  
  # 作業中アンケート（順序付き）
  store <- reactiveValues(groups = list())
  current_id <- reactiveVal(NULL)
  editing_question_options <- reactiveVal(NULL)
  active_group_settings <- reactiveVal(NULL)
  editor_mode <- reactiveVal(FALSE)
  home_survey_id <- reactiveVal("")
  survey_title_initial <- reactiveVal("")
  next_editor_key <- local({
    key <- 0L
    function(kind) { key <<- key + 1L; paste0("new_", kind, "_", key) }
  })

  observeEvent(getQueryString(), {
    survey_id <- getQueryString()$id
    if (!is.null(survey_id) && length(survey_id) && nzchar(trimws(survey_id))) {
      home_survey_id(trimws(survey_id))
    }
  }, ignoreInit = FALSE)

  output$survey_id_ui <- renderUI({
    if (editor_mode()) tagList(
      actionButton("return_home", "ホームに戻る", class = "btn-default"),
      tags$div(class = "form-group", style = "margin-top:14px;",
        tags$label("編集中のアンケートID"),
        if (is.null(current_id())) {
          tags$input(type = "text", class = "form-control", value = "保存時に自動生成", readonly = "readonly")
        } else {
          tags$div(style = "display:flex; gap:6px;",
            tags$input(id = "editing_survey_id", type = "text", class = "form-control",
                       value = current_id(), readonly = "readonly"),
            tags$button(type = "button", class = "btn btn-default",
                        onclick = "navigator.clipboard.writeText(document.getElementById('editing_survey_id').value)",
                        "コピー")
          )
        }
      )
    ) else textInput("survey_id", "読み込むアンケートID", value = home_survey_id())
  })

  output$survey_actions_ui <- renderUI({
    if (editor_mode()) return(NULL)
    tagList(actionButton("load_survey", "読み込む", width = "100%"),
            actionButton("new_survey", "新規作成", width = "100%"))
  })

  output$survey_editor_controls_ui <- renderUI({
    if (!editor_mode()) return(NULL)
    tagList(
      textInput("survey_title", "アンケート名", value = survey_title_initial(), placeholder = "例：新UIの使いやすさアンケート"),
      hr(), h4("標準化プリセット"),
      selectInput("preset_name", "プリセットを選択", choices = c(
        "選択してください" = "", "SUS（システムの使いやすさ）" = "SUS",
        "UMUX-Lite（使いやすさ・有用性）" = "UMUX-Lite", "UEQ（製品・サービスのユーザー体験）" = "UEQ",
        "UEQ-S（ユーザー体験の短縮版）" = "UEQ-S", "NASA-TLX（作業負荷）" = "NASA-TLX",
        "TAM（技術受容・導入意向）" = "TAM", "GEQ（ゲーム・体験の評価）" = "GEQ",
        "IMI（活動への内発的動機づけ）" = "IMI"
      )),
      actionButton("add_preset", "プリセットを設問に追加"),
      helpText("選択した質問群を現在の設問一覧の末尾へ追加します。"), hr(),
      actionButton("save_survey", "アンケートを保存", style = "color:white;background:#0072B2;width:100%;font-weight:bold;"),
      uiOutput("access_link_ui")
    )
  })

  output$admin_content_ui <- renderUI({
    if (!editor_mode()) return(NULL)
    tabsetPanel(
      tabPanel("設問一覧", uiOutput("question_list_ui")),
      tabPanel("回答画面プレビュー", uiOutput("answer_preview_ui"))
    )
  })

  output$access_link_ui <- renderUI({
    survey_id <- current_id()
    if (is.null(survey_id) || !nzchar(survey_id)) return(NULL)

    # The browser provides the public protocol, host, and port. Using these
    # values makes the displayed address match the URL from which admin is
    # currently being accessed (for example http://localhost:3838).
    protocol <- session$clientData$url_protocol
    hostname <- session$clientData$url_hostname
    port <- session$clientData$url_port
    admin_path <- session$clientData$url_pathname
    if (is.null(protocol) || !nzchar(protocol) || is.null(hostname) || !nzchar(hostname)) return(NULL)

    origin <- paste0(protocol, "//", hostname, if (!is.null(port) && nzchar(port)) paste0(":", port) else "")
    app_prefix <- sub("/(admin|analytics)/?$", "", admin_path)
    if (identical(app_prefix, admin_path)) app_prefix <- ""
    app_url <- function(path, with_id = FALSE) {
      paste0(origin, app_prefix, "/", path, "/", if (with_id) paste0("?id=", utils::URLencode(survey_id, reserved = TRUE)) else "")
    }
    answer_url <- app_url("answer", with_id = TRUE)
    qr_url <- app_url("access", with_id = TRUE)
    admin_url <- app_url("admin", with_id = TRUE)
    analysis_url <- app_url("analytics", with_id = TRUE)

    tagList(
      tags$div(
        style = "margin-top:12px;word-break:break-all;",
        tags$strong("URL集"),
        tags$div(
          style = "margin-top:10px;",
          tags$strong("アンケート回答画面（URL）"),
          tags$br(),
          tags$a(href = answer_url, target = "_blank", rel = "noopener", answer_url)
        )
      ),
      tags$a(
        href = qr_url,
        target = "_blank",
        rel = "noopener",
        class = "btn btn-default",
        style = "width:100%; margin-top:8px;",
        "QRコードを生成"
      ),
      tags$hr(),
      tags$div(
        style = "word-break:break-all;",
        tags$strong("保存推奨"),
        tags$div(
          style = "margin-top:10px;",
          tags$strong("アンケート作成画面（現在）"),
          tags$br(),
          tags$a(href = admin_url, target = "_blank", rel = "noopener", admin_url)
        ),
        tags$div(
          style = "margin-top:10px;",
          tags$strong("アンケート分析画面"),
          tags$br(),
          tags$a(href = analysis_url, target = "_blank", rel = "noopener", analysis_url)
        )
      )
    )
  })

  observeEvent(input$toggle_question, {
    question_id <- input$toggle_question
    req(question_id %in% names(flatten_survey_groups(store$groups)))
    capture_questions()
    current <- editing_question_options()
    editing_question_options(if (is.null(current)) question_id else NULL)
  })

  observeEvent(input$close_question, {
    if (is.null(editing_question_options())) return()
    capture_questions()
    editing_question_options(NULL)
  })

  reset_question_type <- function(question, type) {
    required <- if (is.null(question$settings$required)) TRUE else isTRUE(question$settings$required)
    question$type <- type
    question$options <- NULL
    question$min <- question$max <- NULL
    question$settings <- list(required = required)
    if (type %in% c("single_numeric", "single_text", "multiple")) {
      question$settings$display <- "radio"
      question$settings$randomize_choices <- FALSE
    }
    if (type == "text") question$settings$long_text <- FALSE
    if (type == "single_numeric") {
      question$min <- 1
      question$max <- 5
      question$options <- as.character(1:5)
    }
    question
  }

  capture_question_editor_inputs <- function(groups) {
    for (group_id in names(groups)) {
      questions <- groups[[group_id]]$questions
      for (question_id in names(questions)) {
        question <- questions[[question_id]]
        old_type <- question$type
        selected_type <- input[[paste0("question_edit_type_", question_id)]]
        type_changed <- !is.null(selected_type) && !identical(selected_type, old_type)
        for (field in c("title", "desc")) {
          value <- input[[paste0("question_edit_", field, "_", question_id)]]
          if (!is.null(value)) question[[field]] <- value
        }
        if (type_changed) question <- reset_question_type(question, selected_type)

        if (!type_changed && question$type %in% c("single_text", "multiple")) {
          options <- input[[paste0("question_edit_options_", question_id)]]
          if (!is.null(options)) {
            options <- unique(trimws(as.character(options)))
            options <- options[nzchar(options)]
            question$options <- if (length(options)) options else NULL
          }
        } else if (question$type != "single_numeric" || type_changed) question$options <- NULL

        if (!type_changed && question$type %in% c("single_numeric", "numeric")) {
          for (field in c("min", "max")) {
            input_id <- if (question$type == "numeric") paste0("question_edit_numeric_", field, "_", question_id) else paste0("question_edit_", field, "_", question_id)
            value <- input[[input_id]]
            if (!is.null(value)) question[[field]] <- if (!length(value) || is.na(value)) NULL else suppressWarnings(as.numeric(value))
          }
        } else if (!type_changed) question$min <- question$max <- NULL

        if (question$type == "single_numeric") {
          old_options <- suppressWarnings(as.numeric(question$options))
          if (is.null(input[[paste0("question_edit_min_", question_id)]]) && is.null(question$min) && length(old_options) && all(is.finite(old_options))) question$min <- min(old_options)
          if (is.null(input[[paste0("question_edit_max_", question_id)]]) && is.null(question$max) && length(old_options) && all(is.finite(old_options))) question$max <- max(old_options)
          if (!is.null(question$min) && !is.null(question$max) && question$min <= question$max &&
              question$min == floor(question$min) && question$max == floor(question$max)) question$options <- as.character(seq.int(question$min, question$max))
        }

        if (!type_changed) {
          setting_ids <- c(randomize_choices = "question_randomize_choices_", required = "question_required_",
                           long_text = "question_long_text_")
          for (field in names(setting_ids)) {
            value <- input[[paste0(setting_ids[[field]], question_id)]]
            if (!is.null(value)) {
              if (is.null(question$settings)) question$settings <- list()
              question$settings[[field]] <- isTRUE(value)
            }
          }
          display_id <- switch(question$type,
            single_numeric = paste0("question_edit_display_", question_id, "_numeric"),
            single_text = paste0("question_edit_display_", question_id, "_text"),
            multiple = paste0("question_edit_display_", question_id, "_multiple"), NULL)
          display <- if (is.null(display_id)) NULL else input[[display_id]]
          if (!is.null(display)) {
            if (is.null(question$settings)) question$settings <- list()
            question$settings$display <- display
          }
          if (question$type == "single_numeric") for (field in c("min_label", "max_label")) {
            value <- input[[paste0("question_", field, "_", question_id)]]
            if (!is.null(value)) {
              if (is.null(question$settings)) question$settings <- list()
              question$settings[[field]] <- value
            }
          }
        }
        questions[[question_id]] <- question
      }
      groups[[group_id]]$questions <- questions
    }
    groups
  }

  observe({
    groups <- store$groups
    changed <- FALSE
    for (group_id in names(groups)) for (question_id in names(groups[[group_id]]$questions)) {
      question <- groups[[group_id]]$questions[[question_id]]
      type <- input[[paste0("question_edit_type_", question_id)]]
      if (is.null(type) || identical(type, question$type)) next
      for (field in c("title", "desc")) {
        value <- input[[paste0("question_edit_", field, "_", question_id)]]
        if (!is.null(value)) question[[field]] <- value
      }
      groups[[group_id]]$questions[[question_id]] <- reset_question_type(question, type)
      changed <- TRUE
      if (type == "single_numeric") {
        updateNumericInput(session, paste0("question_edit_min_", question_id), value = 1)
        updateNumericInput(session, paste0("question_edit_max_", question_id), value = 5)
      } else {
        updateNumericInput(session, paste0("question_edit_min_", question_id), value = NA_real_)
        updateNumericInput(session, paste0("question_edit_max_", question_id), value = NA_real_)
      }
      updateNumericInput(session, paste0("question_edit_numeric_min_", question_id), value = NA_real_)
      updateNumericInput(session, paste0("question_edit_numeric_max_", question_id), value = NA_real_)
      updateSelectizeInput(session, paste0("question_edit_options_", question_id), choices = character(), selected = character())
    }
    if (changed) store$groups <- groups
  })

  capture_questions <- function() {
    store$groups <- capture_question_editor_inputs(store$groups)
    store$groups
  }


  # ---------- 既存アンケート読み込み ----------
  observeEvent(input$load_survey, {
    req(!is.null(input$survey_id), nzchar(trimws(input$survey_id)), nzchar(input$survey_pw))
    
    db  <- load_db()
    sid <- trimws(input$survey_id)
    
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
    structure <- load_survey_items(sid)
    groups <- structure$groups
    qlist <- flatten_survey_groups(groups)
    survey_info <- load_survey_info(sid)
    
    if (length(qlist) == 0) {
      showNotification("設問が見つかりません", type = "error")
      return()
    }
    
    store$groups <- groups
    editing_question_options(names(qlist)[1])
    current_id(survey_info$id)
    home_survey_id(survey_info$id)
    survey_title_initial(survey_info$title)
    editor_mode(TRUE)
    showNotification("アンケートを読み込みました", type = "message")
  })

  observeEvent(input$new_survey, {
    store$groups <- list()
    editing_question_options(NULL)
    active_group_settings(NULL)
    current_id(NULL)
    home_survey_id("")
    survey_title_initial("")
    editor_mode(TRUE)
  })

  observeEvent(input$return_home, {
    if (!is.null(current_id())) home_survey_id(current_id())
    editor_mode(FALSE)
  })

  # ---------- 標準化プリセット追加 ----------
  observeEvent(input$add_preset, {

    preset <- standard_presets[[input$preset_name]]
    req(preset)

    groups <- capture_questions()
    group_id <- next_editor_key("group")
    group <- list(id = NULL, title = "", settings = list(page = TRUE), questions = list())
    for (template in preset$questions) {
      template$id <- NULL
      template$settings <- modifyList(if (is.null(template$settings)) list() else template$settings,
                                      list(randomize_choices = FALSE, required = TRUE))
      group$questions[[next_editor_key("question")]] <- template
    }
    store$groups <- c(groups, setNames(list(group), group_id))
    showNotification(paste0(preset$title, "を追加しました。保存して反映してください。"), type = "message")

  })

  observeEvent(input$add_group, {
    capture_questions()
    groups <- store$groups
    group_id <- next_editor_key("group")
    groups[[group_id]] <- list(id = NULL, title = "", settings = list(page = TRUE))
    store$groups <- groups
  })

  observeEvent(input$toggle_group_settings, {
    group_id <- input$toggle_group_settings
    group_order <- names(store$groups)
    req(group_id %in% group_order)
    group_index <- match(group_id, group_order)
    previous_group_ids <- if (group_index > 1) group_order[seq_len(group_index - 1)] else character()
    previous_questions <- flatten_survey_groups(store$groups[previous_group_ids])
    active_group_settings(group_id)
    group <- store$groups[[group_id]]
    if (is.null(group) || !is.list(group)) group <- list()
    if (is.null(group$settings) || !is.list(group$settings)) group$settings <- list()
    if (is.null(group$questions) || !is.list(group$questions)) group$questions <- list()
    showModal(modalDialog(
      title = "グループ設定",
      question_group_settings_ui(group_id, group, previous_questions),
      footer = NULL,
      easyClose = TRUE
    ))
  })

  observe({
    group_id <- active_group_settings()
    req(!is.null(group_id), group_id %in% names(store$groups))
    title <- input[[paste0("group_title_", group_id)]]
    page <- input[[paste0("group_page_", group_id)]]
    randomize <- input[[paste0("group_randomize_questions_", group_id)]]
    has_visibility <- input[[paste0("group_has_visibility_", group_id)]]
    req(!is.null(title), !is.null(page), !is.null(randomize), !is.null(has_visibility))

    settings <- list(page = isTRUE(page), randomize_questions = isTRUE(randomize))
    condition_question <- input[[paste0("group_condition_question_", group_id)]]
    condition_value <- input[[paste0("group_condition_value_", group_id)]]
    if (isTRUE(has_visibility) && !is.null(condition_question) && nzchar(condition_question) &&
        !is.null(condition_value) && nzchar(condition_value)) {
      settings$visibility <- list(
        question_id = condition_question,
        operator = input[[paste0("group_condition_operator_", group_id)]],
        value = condition_value
      )
    }

    groups <- store$groups
    if (!identical(groups[[group_id]]$title, title) || !identical(groups[[group_id]]$settings, settings)) {
      groups[[group_id]]$title <- title
      groups[[group_id]]$settings <- settings
      store$groups <- groups
    }
  })

  observeEvent(input$move_question, {
    parts <- strsplit(input$move_question, "|", fixed = TRUE)[[1]]
    req(length(parts) == 2, parts[1] %in% names(flatten_survey_groups(store$groups)))
    qid <- parts[1]
    capture_questions()
    group_id <- names(store$groups)[which(vapply(store$groups, function(group) qid %in% names(group$questions), logical(1)))[1]]
    ids <- names(store$groups[[group_id]]$questions)
    store$groups[[group_id]]$questions <- store$groups[[group_id]]$questions[move_item(ids, qid, parts[2])]
  })

  observeEvent(input$group_action, {
    parts <- strsplit(input$group_action, "|", fixed = TRUE)[[1]]
    action <- parts[1]
    group_id <- parts[2]
    req(length(parts) >= 2, group_id %in% names(store$groups))
    capture_questions()
    if (action == "move") {
      req(length(parts) == 3)
      store$groups <- sanitize_group_visibility(store$groups[move_item(names(store$groups), group_id, parts[3])])
      return()
    }
    groups <- store$groups
    group_order <- names(store$groups)
    if (action == "copy") {
      new_id <- next_editor_key("group")
      copy <- groups[[group_id]]
      copy$id <- NULL
      copy$questions <- lapply(copy$questions, duplicate_question)
      names(copy$questions) <- vapply(copy$questions, function(question) next_editor_key("question"), character(1))
      groups[[new_id]] <- copy
      group_order <- append(group_order, new_id, after = match(group_id, group_order))
      if (length(copy$questions)) editing_question_options(names(copy$questions)[1])
    } else if (action == "delete") {
      removed_ids <- names(groups[[group_id]]$questions)
      groups[[group_id]] <- NULL
      group_order <- setdiff(group_order, group_id)
      if (!is.null(editing_question_options()) && editing_question_options() %in% removed_ids) {
        remaining <- flatten_survey_groups(groups[group_order])
        editing_question_options(if (length(remaining)) names(remaining)[1] else NULL)
      }
      if (identical(active_group_settings(), group_id)) { active_group_settings(NULL); removeModal() }
    } else return()
    store$groups <- sanitize_group_visibility(groups[group_order])
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
        length(question_ids) != length(flatten_survey_groups(store$groups)) ||
        !setequal(question_ids, names(flatten_survey_groups(store$groups)))) {
      showNotification("ドラッグ後の配置を反映できませんでした。", type = "error")
      return()
    }

    groups <- capture_questions()
    questions <- flatten_survey_groups(groups)
    question_drafts <- if (is.list(payload) && !is.null(payload$question_drafts)) payload$question_drafts else list()
    for (draft in question_drafts) {
      question_id <- as.character(draft$id)
      if (length(question_id) != 1 || is.na(question_id) || !question_id %in% names(questions)) next
      question <- questions[[question_id]]
      for (field in c("title", "desc", "type")) {
        if (!is.null(draft[[field]])) question[[field]] <- as.character(draft[[field]])
      }
      if (!is.null(draft$options)) {
        options <- unique(trimws(as.character(unlist(draft$options, use.names = FALSE))))
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
      if (!is.null(draft$display)) {
        if (is.null(question$settings)) question$settings <- list()
        question$settings$display <- as.character(draft$display)
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
    groups <- store$groups
    for (group_id in setdiff(group_ids, names(groups))) {
      groups[[group_id]] <- list(
        id = NULL,
        title = "",
        settings = list(page = TRUE)
      )
    }
    groups <- groups[group_ids]
    for (group in layout) {
      group_id <- as.character(group$group_id)
      ids <- unlist(group$question_ids, use.names = FALSE)
      groups[[group_id]]$questions <- lapply(questions[ids], function(question) { question$group_id <- NULL; question })
    }
    store$groups <- sanitize_group_visibility(groups)
  })
  
  # ---------- 設問の挿入・編集 ----------
  observeEvent(input$insert_question, {
    payload <- strsplit(input$insert_question, "|", fixed = TRUE)[[1]]
    if (!length(payload) || !nzchar(payload[1])) return()
    kind <- payload[1]
    capture_questions()
    groups <- store$groups
    group_order <- names(groups)
    if (!length(groups)) {
      group_id <- next_editor_key("group")
      groups[[group_id]] <- list(id = NULL, title = "", settings = list(page = TRUE))
    }
    group_order <- names(groups)
    qid <- next_editor_key("question")
    question <- list(id = NULL, title = "", desc = "", type = "text", options = NULL,
                     settings = list(randomize_choices = FALSE, required = TRUE), min = NULL, max = NULL)
    if (kind == "in") {
      if (length(payload) < 3 || !payload[2] %in% group_order || !payload[3] %in% names(groups[[payload[2]]]$questions)) return()
      group_id <- payload[2]
      items <- groups[[group_id]]$questions
      items <- append(items, setNames(list(question), qid), after = match(payload[3], names(items)) - 1L)
    } else {
      group_id <- if (kind == "after_group" && length(payload) >= 2 && payload[2] %in% group_order) payload[2] else tail(group_order, 1)
      items <- append(groups[[group_id]]$questions, setNames(list(question), qid))
    }
    groups[[group_id]]$questions <- items
    store$groups <- groups
    editing_question_options(qid)
  })

  # ---------- 削除 ----------
  observeEvent(input$question_action, {
    parts <- strsplit(input$question_action, "|", fixed = TRUE)[[1]]
    action <- parts[1]
    id <- parts[2]
    req(length(parts) == 2, id %in% names(flatten_survey_groups(store$groups)))
    capture_questions()
    group_id <- names(store$groups)[which(vapply(store$groups, function(group) id %in% names(group$questions), logical(1)))[1]]
    items <- store$groups[[group_id]]$questions
    if (action == "copy") {
      copy_id <- next_editor_key("question")
      items <- append(items, setNames(list(duplicate_question(items[[id]])), copy_id), after = match(id, names(items)))
      editing_question_options(copy_id)
    } else if (action == "delete") {
      items[[id]] <- NULL
      store$groups[[group_id]]$questions <- items
      remaining <- flatten_survey_groups(store$groups)
      if (identical(editing_question_options(), id)) editing_question_options(if (length(remaining)) names(remaining)[1] else NULL)
      store$groups <- sanitize_group_visibility(store$groups)
    } else return()
    store$groups[[group_id]]$questions <- items
  })

  # ---------- 設問一覧 UI ----------
  output$question_list_ui <- renderUI({
    question_group_board_ui(
      groups = store$groups,
      editing_question_options = editing_question_options()
    )
  })
  
  # ---------- プレビュー ----------
  output$answer_preview_ui <- renderUI({
    if (!length(flatten_survey_groups(store$groups)))
      return(p("設問が作成されるとプレビューが表示されます。"))
    
    tagList(
      lapply(names(store$groups), function(group_id) {
        tagList(
          if (!is.null(store$groups[[group_id]]$title) && !is.na(store$groups[[group_id]]$title[1]) &&
              nzchar(trimws(store$groups[[group_id]]$title[1]))) {
            h3(store$groups[[group_id]]$title)
          },
          lapply(names(store$groups[[group_id]]$questions), function(id) {
            q <- store$groups[[group_id]]$questions[[id]]
            label <- tags$div(
              strong(q$title),
              tags$div(style = "color:#666;font-size:0.9em;", q$desc)
            )
            question_response_input(paste0("p_", id), q, label = label)
          })
        )
      })
    )
  })
  
  # ---------- 保存処理 ----------
  observeEvent(input$save_survey, {
    if (!length(flatten_survey_groups(store$groups))) {
      showNotification("設問が1件もありません", type = "error")
      return()
    }
    if (is.null(input$survey_pw) || !nzchar(input$survey_pw)) {
      showNotification("パスワードを入力してください", type = "error")
      return()
    }

    entered_survey_id <- current_id()

    groups_to_save <- capture_questions()
    questions_to_save <- flatten_survey_groups(groups_to_save)
    for (qid in names(questions_to_save)) {
      question <- questions_to_save[[qid]]
      error <- if (is.null(question$title) || !nzchar(trimws(question$title))) "設問タイトルを入力してください。"
      else if (question$type == "single_numeric" && (is.null(question$min) || is.null(question$max) || question$min > question$max || question$min != floor(question$min) || question$max != floor(question$max))) "数字択一の範囲は整数で指定してください。"
      else if (question$type %in% c("single_text", "multiple") && !length(question$options)) "選択肢を1つ以上入力してください。"
      else if (question$type == "numeric" && !is.null(question$min) && !is.null(question$max) && question$min > question$max) "上限は下限以上にしてください。"
      else NULL
      if (!is.null(error)) { showNotification(error, type = "error"); return() }
    }

    survey_title <- trimws(if (is.null(input$survey_title)) "" else input$survey_title)

    tryCatch({
      id_map <- save_db(
        survey_id = entered_survey_id,
        password_hash = hash_pw(input$survey_pw),
        groups_list = groups_to_save,
        survey_title = survey_title
      )

      store$groups <- remember_saved_ids(groups_to_save, id_map)

      saved_id <- id_map$survey_id
      current_id(saved_id)
      home_survey_id(saved_id)
      showNotification(paste("保存しました（ID:", saved_id, ")"), type = "message")
    }, error = function(e) {
      showNotification(paste("保存エラー:", e$message), type = "error")
    })
  })

})

