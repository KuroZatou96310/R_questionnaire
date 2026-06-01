# admin.R --- 複数アンケート管理・ID/パスワード制アンケート生成アプリ
# 保存先: ../enquete_app_data/
library(shiny)
library(jsonlite)
library(uuid)
library(digest)   # パスワードハッシュ化用
library(DBI)
library(RSQLite)


# ---------- 設定 ----------

BASE_DIR <- getwd()

DATA_DIR <- file.path(
  BASE_DIR,
  "enquete_app_data"
)
DB_FILE  <- file.path(DATA_DIR, "questionnaire.db")

if (!dir.exists(DATA_DIR)) dir.create(DATA_DIR, recursive = TRUE)

# ---------- DB 操作 ----------
# SQLiteデータベース初期化
init_db <- function() {
  if (file.exists(DB_FILE)) return()
  
  con <- dbConnect(SQLite(), DB_FILE)
  
  # surveysテーブル
  dbExecute(con, "
    CREATE TABLE surveys (
      survey_id TEXT PRIMARY KEY,
      password_hash TEXT NOT NULL,
      title TEXT,
      description TEXT,
      created_at TEXT,
      updated_at TEXT
    )
  ")
  
  # questionsテーブル
  dbExecute(con, "
    CREATE TABLE questions (
      question_id TEXT PRIMARY KEY,
      survey_id TEXT NOT NULL,
      title TEXT,
      description TEXT,
      type TEXT,
      order_index INTEGER,
      options_json TEXT,
      min_value REAL,
      max_value REAL,
      FOREIGN KEY(survey_id) REFERENCES surveys(survey_id)
    )
  ")
  
  dbDisconnect(con)
}

# 既存アンケート一覧を取得（surveyオブジェクト形式で返す）
load_db <- function() {
  init_db()
  
  con <- dbConnect(SQLite(), DB_FILE)
  surveys <- dbReadTable(con, "surveys")
  dbDisconnect(con)
  
  if (nrow(surveys) == 0) return(list())
  
  # JSON互換形式に変換
  result <- list()
  for (i in 1:nrow(surveys)) {
    row <- surveys[i, ]
    result[[row$survey_id]] <- list(
      password = row$password_hash,
      created = row$created_at,
      qcount = NA_integer_
    )
  }
  return(result)
}

# 質問をデータベースから読み込む
load_questions <- function(survey_id) {
  con <- dbConnect(SQLite(), DB_FILE)
  
  qs_df <- dbGetQuery(con, 
                      "SELECT * FROM questions WHERE survey_id = ? ORDER BY order_index",
                      list(survey_id)
  )
  
  dbDisconnect(con)
  
  if (nrow(qs_df) == 0) return(list())
  
  # リスト形式に変換
  questions <- list()
  for (i in 1:nrow(qs_df)) {
    row <- qs_df[i, ]
    options <- NULL
    if (!is.na(row$options_json) && nzchar(row$options_json)) {
      options <- fromJSON(row$options_json, simplifyVector = FALSE)
    }
    
    questions[[row$question_id]] <- list(
      id = row$question_id,
      title = row$title,
      desc = row$description,
      type = row$type,
      options = options,
      min = if (is.na(row$min_value)) NULL else row$min_value,
      max = if (is.na(row$max_value)) NULL else row$max_value
    )
  }
  return(questions)
}

# アンケートと質問をデータベースに保存
save_db <- function(survey_id, password_hash, questions_list, question_ids) {
  init_db()
  con <- dbConnect(SQLite(), DB_FILE)
  
  tryCatch({
    created_at <- as.character(Sys.time())
    
    # surveyが既に存在するか確認
    existing <- dbGetQuery(con, 
                           "SELECT COUNT(*) as cnt FROM surveys WHERE survey_id = ?",
                           list(survey_id)
    )
    
    if (existing$cnt > 0) {
      # 既存: 質問を削除して再作成
      dbExecute(con, "DELETE FROM questions WHERE survey_id = ?", list(survey_id))
      dbExecute(con, 
                "UPDATE surveys SET password_hash = ?, updated_at = ? WHERE survey_id = ?",
                list(password_hash, created_at, survey_id)
      )
    } else {
      # 新規作成
      dbExecute(con,
                "INSERT INTO surveys (survey_id, password_hash, created_at, updated_at) VALUES (?, ?, ?, ?)",
                list(survey_id, password_hash, created_at, created_at)
      )
    }
    
    # 質問を保存
    for (i in seq_along(question_ids)) {
      qid <- question_ids[i]
      q <- questions_list[[qid]]
      
      options_json <- ifelse(
        is.null(q$options) || length(q$options) == 0,
        NA_character_,
        toJSON(q$options, auto_unbox = TRUE)
      )
      
      dbExecute(con,
                paste(
                  "INSERT INTO questions",
                  "(question_id, survey_id, title, description, type, order_index, options_json, min_value, max_value)",
                  "VALUES (?, ?, ?, ?, ?, ?, ?, ?, ?)"
                ),
                list(
                  qid,
                  survey_id,
                  q$title,
                  q$desc,
                  q$type,
                  i - 1,
                  options_json,
                  if (is.null(q$min)) NA_real_ else q$min,
                  if (is.null(q$max)) NA_real_ else q$max
                )
      )
    }
    
    dbDisconnect(con)
    return(TRUE)
  }, error = function(e) {
    dbDisconnect(con)
    stop(e)
  })
}

# パスワードをSHA-256でハッシュ化
hash_pw <- function(pw) {
  digest(pw, algo = "sha256", serialize = FALSE)
}

# fromJSON後のquestions正規化（optionsがlist()になるケース対策）
normalize_questions <- function(qlist) {
  lapply(qlist, function(q) {
    # options: 空リストはNULLに統一
    if (!is.null(q$options) && length(q$options) == 0) q$options <- NULL
    # min/max: NULLに統一
    if (!is.null(q$min) && length(q$min) == 0) q$min <- NULL
    if (!is.null(q$max) && length(q$max) == 0) q$max <- NULL
    q
  })
}

# ==============================
#           UI
# ==============================
ui <- fluidPage(
  titlePanel("アンケート生成アプリ（admin）"),
  
  sidebarLayout(
    sidebarPanel(
      h4("アンケート情報 (ID/PW)"),
      textInput("survey_id",  "アンケートID（空なら自動生成）"),
      passwordInput("survey_pw", "アンケートパスワード（必須）"),
      actionButton("load_survey", "既存アンケートを読み込む"),
      hr(),
      
      h4("設問を追加"),
      textInput("q_title", "設問タイトル"),
      textAreaInput("q_desc", "設問説明"),
      selectInput("q_type", "設問タイプ",
                  choices = c("single", "multiple", "numeric", "text",
                              "select", "slider", "date")),
      conditionalPanel(
        condition = "['single','multiple','select'].includes(input.q_type)",
        textAreaInput("q_options", "選択肢（改行区切り）",
                      placeholder = "例：はい\nいいえ\nわからない")
      ),
      conditionalPanel(
        condition = "input.q_type == 'slider'",
        numericInput("q_min", "下限値", 0),
        numericInput("q_max", "上限値", 100)
      ),
      actionButton("add_q", "設問を追加"),
      hr(),
      
      actionButton("save_survey", "アンケートを保存",
                   style = "color:white;background:#0072B2;width:100%;font-weight:bold;")
    ),
    
    mainPanel(
      tabsetPanel(
        tabPanel("設問一覧",        uiOutput("question_list_ui")),
        tabPanel("回答画面プレビュー", uiOutput("answer_preview_ui"))
      )
    )
  )
)

# ==============================
#         SERVER
# ==============================
server <- function(input, output, session) {
  
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
}

shinyApp(ui, server)