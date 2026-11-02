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
  store      <- reactiveValues(order = character(0), questions = list())
  current_id <- reactiveVal(NULL)
  edit_options_target <- reactiveVal(NULL)
  
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
    qlist <- load_questions(sid)
    
    if (length(qlist) == 0) {
      showNotification("設問が見つかりません", type = "error")
      return()
    }
    
    # id をキーにした名前付きリストに変換
    ids <- names(qlist)
    
    store$order     <- ids
    store$questions <- qlist
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
      template$id <- qid
      qs[[qid]] <- template
      ids <- c(ids, qid)
    }

    store$questions <- qs
    store$order <- c(store$order, ids)
    showNotification(paste0(preset$title, "を追加しました。保存して反映してください。"), type = "message")

  })
  
  # ---------- 設問追加 ----------
  observeEvent(input$add_q, {
    req(nzchar(input$q_title))
    
    qid <- UUIDgenerate()
    
    # 選択肢チェック
    options <- NULL
    if (input$q_type %in% c("single", "multiple", "select")) {
      options <- strsplit(gsub("\r\n?", "\n", input$q_options), "\n", fixed = TRUE)[[1]]
      options <- trimws(options)
      options <- options[nzchar(options)]
      if (length(options) == 0) {
        showNotification("選択肢が必要です", type = "error")
        return()
      }
    }
    
    # スライダー範囲チェック
    minv <- NULL; maxv <- NULL
    if (input$q_type == "slider") {
      minv <- input$q_min
      maxv <- input$q_max
      if (maxv <= minv) {
        showNotification("上限は下限より大きくしてください", type = "error")
        return()
      }
    }
    
    # --- FIX: reactiveValues のリストはいったん取り出して差し替える ---
    qs <- store$questions
    qs[[qid]] <- list(
      id      = qid,
      title   = input$q_title,
      desc    = input$q_desc,
      type    = input$q_type,
      options = options,
      min     = minv,
      max     = maxv
    )
    store$questions <- qs
    store$order     <- c(store$order, qid)
    
    # 入力フィールドをリセット
    updateTextInput(    session, "q_title",   value = "")
    updateTextAreaInput(session, "q_desc",    value = "")
    updateTextAreaInput(session, "q_options", value = "")
    showNotification("設問を追加しました", type = "message")
  })
  
  # ---------- 削除 ----------
  # FIX: NULL代入ではなく取り出して差し替える
  observeEvent(input$delete_q, ignoreInit = TRUE, {
    id <- input$delete_q
    req(nzchar(id))
    
    qs <- store$questions
    qs[[id]] <- NULL
    store$questions <- qs
    store$order     <- store$order[store$order != id]

    if (!is.null(current_id())) {
      delete_question_and_answers(current_id(), id)
    }
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

    if (!is.null(current_id()) && nzchar(input$survey_pw)) {
      update_question_options(current_id(), qid, q$options)
      showNotification("選択肢を改変して保存しました", type = "message")
    } else {
      showNotification("選択肢を改変しました。アンケートを保存してください", type = "message")
    }

    removeModal()

  })
  
  # ---------- 上移動 ----------
  # FIX: ベクトルをいったんコピーして差し替える
  observeEvent(input$move_up, ignoreInit = TRUE, {
    id  <- input$move_up
    req(nzchar(id))
    
    ord <- store$order
    idx <- which(ord == id)
    if (length(idx) == 1 && idx > 1) {
      ord[c(idx - 1, idx)] <- ord[c(idx, idx - 1)]
      store$order <- ord
    }
  })
  
  # ---------- 下移動 ----------
  observeEvent(input$move_down, ignoreInit = TRUE, {
    id  <- input$move_down
    req(nzchar(id))
    
    ord <- store$order
    idx <- which(ord == id)
    if (length(idx) == 1 && idx < length(ord)) {
      ord[c(idx, idx + 1)] <- ord[c(idx + 1, idx)]
      store$order <- ord
    }
  })
  
  # ---------- 設問一覧 UI ----------
  output$question_list_ui <- renderUI({
    if (length(store$order) == 0) return(p("まだ設問がありません。"))
    
    tagList(
      lapply(store$order, function(id) {
        q <- store$questions[[id]]
        div(
          style = "padding:10px;border:1px solid #ccc;margin-bottom:10px;",
          strong(q$title), br(),
          span(style = "color:#666;", q$desc), br(),
          span(style = "color:#555;", paste("タイプ:", q$type)), br(),
          if (!is.null(q$options))
            div(paste("選択肢:", paste(unlist(q$options), collapse = ", "))),
          if (q$type %in% c("single", "multiple", "select"))
            actionButton(
              paste0("edit_", id), "選択肢を改変",
              onclick = sprintf(
                "Shiny.setInputValue('edit_options_qid','%s',{priority:'event'})", id)
            ),
          if (!is.null(q$min) && q$type == "slider")
            div(paste0("範囲: ", q$min, "〜", q$max)),
          div(
            actionButton(
              paste0("up_", id), "▲ 上",
              onclick = sprintf(
                "Shiny.setInputValue('move_up','%s',{priority:'event'})", id)
            ),
            actionButton(
              paste0("down_", id), "▼ 下",
              onclick = sprintf(
                "Shiny.setInputValue('move_down','%s',{priority:'event'})", id)
            ),
            actionButton(
              paste0("del_", id), "削除",
              onclick = sprintf(
                "Shiny.setInputValue('delete_q','%s',{priority:'event'})", id),
              style = "color:white;background:red;margin-left:10px;"
            )
          )
        )
      })
    )
  })
  
  # ---------- プレビュー ----------
  output$answer_preview_ui <- renderUI({
    if (length(store$order) == 0)
      return(p("設問が作成されるとプレビューが表示されます。"))
    
    tagList(
      lapply(store$order, function(id) {
        q     <- store$questions[[id]]
        label <- tags$div(
          strong(q$title),
          tags$div(style = "color:#666;font-size:0.9em;", q$desc)
        )
        opts <- unlist(q$options)   # JSONから戻るlistをcharacterベクトルに
        
        switch(
          q$type,
          "single"   = radioButtons(        paste0("p_", id), label, choices = opts),
          "multiple" = checkboxGroupInput(  paste0("p_", id), label, choices = opts),
          "select"   = selectInput(         paste0("p_", id), label, choices = opts),
          "numeric"  = numericInput(        paste0("p_", id), label, value = NA),
          "text"     = textAreaInput(       paste0("p_", id), label),
          "slider"   = sliderInput(         paste0("p_", id), label,
                                            min = q$min, max = q$max, value = q$min),
          "date"     = dateInput(           paste0("p_", id), label, value = Sys.Date())
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
    if (!nzchar(input$survey_pw)) {
      showNotification("パスワードを入力してください", type = "error")
      return()
    }
    
    sid <- input$survey_id
    if (!nzchar(sid)) sid <- paste0("survey_", UUIDgenerate())
    
    current_id(sid)

    random_start <- input$random_start
    if (is.null(random_start) || is.na(random_start) || random_start < 1 || random_start > length(store$order)) {
      random_start <- NULL
    } else {
      random_start <- as.integer(random_start)
    }
    
    tryCatch({
      # SQLiteに保存
      save_db(
        survey_id = sid,
        password_hash = hash_pw(input$survey_pw),
        questions_list = store$questions,
        question_ids = store$order,
        random_start = random_start
      )
      
      # FIX: 自動生成IDをUIに反映してユーザーが確認できるようにする
      updateTextInput(session, "survey_id", value = sid)
      
      showNotification(paste("保存しました（ID:", sid, ")"), type = "message")
    }, error = function(e) {
      showNotification(paste("保存エラー:", e$message), type = "error")
    })
  })
})
