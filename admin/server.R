library(shiny)
source("db.R")

# Define server logic required to draw a histogram
shinyServer(function(input, output, session) {
  
  # 作業中アンケート（順序付き）
  store      <- reactiveValues(order = character(0), questions = list())
  current_id <- reactiveVal(NULL)
  
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
    showNotification("アンケートを読み込みました", type = "message")
  })
  
  # ---------- 設問追加 ----------
  observeEvent(input$add_q, {
    req(nzchar(input$q_title))
    
    qid <- UUIDgenerate()
    
    # 選択肢チェック
    options <- NULL
    if (input$q_type %in% c("single", "multiple", "select")) {
      options <- strsplit(input$q_options, "[\r\n]+")[[1]]
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
    
    tryCatch({
      # SQLiteに保存
      save_db(
        survey_id = sid,
        password_hash = hash_pw(input$survey_pw),
        questions_list = store$questions,
        question_ids = store$order
      )
      
      # FIX: 自動生成IDをUIに反映してユーザーが確認できるようにする
      updateTextInput(session, "survey_id", value = sid)
      
      showNotification(paste("保存しました（ID:", sid, ")"), type = "message")
    }, error = function(e) {
      showNotification(paste("保存エラー:", e$message), type = "error")
    })
  })
})