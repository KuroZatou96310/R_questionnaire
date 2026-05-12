# admin.R --- 複数アンケート管理・ID/パスワード制アンケート生成アプリ
# 保存先: C:/NIT_task/R_webapp/enquete_app_data/
library(shiny)
library(jsonlite)
library(uuid)

# ---------- 設定 ----------
DATA_DIR <- "C:/NIT_task/R_webapp/enquete_app_data"
DB_FILE  <- file.path(DATA_DIR, "surveys.json")

if (!dir.exists(DATA_DIR)) dir.create(DATA_DIR, recursive = TRUE)

# ---------- DB 操作 ----------
load_db <- function() {
  if (!file.exists(DB_FILE)) return(list())
  fromJSON(DB_FILE, simplifyVector = FALSE)
}
save_db <- function(db) {
  writeLines(toJSON(db, pretty = TRUE, auto_unbox = TRUE), con = DB_FILE)
}

# ==============================
#           UI
# ==============================
ui <- fluidPage(
  titlePanel("アンケート生成アプリ（admin）"),
  
  sidebarLayout(
    sidebarPanel(
      h4("アンケート情報 (ID/PW)"),
      textInput("survey_id", "アンケートID（任意、空なら自動生成）"),
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
        textAreaInput("q_options", "選択肢（改行区切り）", placeholder = "例：はい\nいいえ\nわからない")
      ),
      conditionalPanel(
        condition = "input.q_type == 'slider'",
        numericInput("q_min", "下限値", 0),
        numericInput("q_max", "上限値", 100)
      ),
      actionButton("add_q", "設問を追加"),
      hr(),
      
      actionButton("save_survey", "アンケートを保存",
                   style="color:white;background:#0072B2;width:100%;font-weight:bold;")
    ),
    
    mainPanel(
      tabsetPanel(
        tabPanel("設問一覧", uiOutput("question_list_ui")),
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
  store <- reactiveValues(order = character(0), questions = list())
  current_id <- reactiveVal(NULL)
  
  # ---------- 既存アンケート読み込み ----------
  observeEvent(input$load_survey, {
    req(nzchar(input$survey_id))
    db <- load_db()
    sid <- input$survey_id
    if (!(sid %in% names(db))) {
      showNotification("指定IDのアンケートは存在しません", type = "error")
      return()
    }
    s <- db[[sid]]
    # パスワードチェック
    if (is.null(s$password) || s$password != input$survey_pw) {
      showNotification("パスワードが正しくありません", type = "error")
      return()
    }
    qlist <- s$questions
    ids <- sapply(qlist, function(q) q$id)
    store$order <- ids
    store$questions <- qlist
    current_id(sid)
    showNotification("アンケートを読み込みました", type = "message")
  })
  
  # ---------- 設問追加 ----------
  observeEvent(input$add_q, {
    req(nzchar(input$q_title))
    qid <- UUIDgenerate()
    
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
    
    minv <- NULL; maxv <- NULL
    if (input$q_type == "slider") {
      minv <- input$q_min; maxv <- input$q_max
      if (maxv <= minv) {
        showNotification("上限は下限より大きくしてください", type = "error")
        return()
      }
    }
    
    store$questions[[qid]] <- list(
      id = qid,
      title = input$q_title,
      desc  = input$q_desc,
      type  = input$q_type,
      options = options,
      min = minv,
      max = maxv
    )
    store$order <- c(store$order, qid)
    
    updateTextInput(session, "q_title", value = "")
    updateTextAreaInput(session, "q_desc", value = "")
    updateTextAreaInput(session, "q_options", value = "")
    showNotification("設問を追加しました", type = "message")
  })
  
  # ---------- 並び替え・削除 ----------
  observeEvent(input$delete_q, {
    id <- input$delete_q
    store$questions[[id]] <- NULL
    store$order <- store$order[store$order != id]
  })
  observeEvent(input$move_up, {
    id <- input$move_up
    idx <- which(store$order == id)
    if (idx > 1) store$order[c(idx-1, idx)] <- store$order[c(idx, idx-1)]
  })
  observeEvent(input$move_down, {
    id <- input$move_down
    idx <- which(store$order == id)
    if (idx < length(store$order)) store$order[c(idx, idx+1)] <- store$order[c(idx+1, idx)]
  })
  
  # ---------- 設問一覧 UI ----------
  output$question_list_ui <- renderUI({
    if (length(store$order) == 0) return(p("まだ設問がありません。"))
    tagList(
      lapply(store$order, function(id) {
        q <- store$questions[[id]]
        div(style="padding:10px;border:1px solid #ccc;margin-bottom:10px;",
            strong(q$title), br(),
            span(style="color:#666;", q$desc), br(),
            span(style="color:#555;", paste("タイプ:", q$type)), br(),
            if (!is.null(q$options)) div(paste("選択肢:", paste(q$options, collapse=", "))),
            if (q$type == "slider") div(paste0("範囲: ", q$min, "〜", q$max)),
            div(
              actionButton(paste0("up_", id), "▲ 上",
                           onclick = sprintf("Shiny.setInputValue('move_up','%s',{priority:'event'})", id)),
              actionButton(paste0("down_", id), "▼ 下",
                           onclick = sprintf("Shiny.setInputValue('move_down','%s',{priority:'event'})", id)),
              actionButton(paste0("del_", id), "削除",
                           onclick = sprintf("Shiny.setInputValue('delete_q','%s',{priority:'event'})", id),
                           style="color:white;background:red;margin-left:10px;")
            )
        )
      })
    )
  })
  
  # ---------- プレビュー ----------
  output$answer_preview_ui <- renderUI({
    if (length(store$order) == 0) return(p("設問が作成されるとプレビューが表示されます。"))
    tagList(
      lapply(store$order, function(id) {
        q <- store$questions[[id]]
        label <- tags$div(strong(q$title), tags$div(style="color:#666;font-size:0.9em;", q$desc))
        switch(q$type,
               "single"   = radioButtons(paste0("p_",id), label, choices = q$options),
               "multiple" = checkboxGroupInput(paste0("p_",id), label, choices = q$options),
               "select"   = selectInput(paste0("p_",id), label, choices = q$options),
               "numeric"  = numericInput(paste0("p_",id), label, value = NA),
               "text"     = textAreaInput(paste0("p_",id), label),
               "slider"   = sliderInput(paste0("p_",id), label, min = q$min, max = q$max, value = q$min),
               "date"     = dateInput(paste0("p_",id), label, value = Sys.Date())
        )
      })
    )
  })
  
  # ---------- 保存処理（surveys.json と per-survey フォルダ） ----------
  observeEvent(input$save_survey, {
    req(length(store$order) > 0)
    req(nzchar(input$survey_pw))
    
    sid <- input$survey_id
    if (!nzchar(sid)) sid <- paste0("survey_", UUIDgenerate())
    current_id(sid)
    
    # load index DB
    db <- load_db()
    
    # prepare data
    survey_entry <- list(
      password = input$survey_pw,
      created = as.character(Sys.time()),
      questions = store$questions[store$order]
    )
    
    # update index
    db[[sid]] <- list(
      password = input$survey_pw,
      created = survey_entry$created,
      qcount = length(store$order)
    )
    save_db(db)
    
    # per-survey folder
    enq_dir <- file.path(DATA_DIR, sid)
    if (!dir.exists(enq_dir)) dir.create(enq_dir, recursive = TRUE)
    
    # meta.json
    meta <- list(id = sid, password = input$survey_pw, created = survey_entry$created)
    writeLines(toJSON(meta, pretty = TRUE, auto_unbox = TRUE), con = file.path(enq_dir, "meta.json"))
    
    # questions.json (ordered)
    writeLines(toJSON(survey_entry$questions, pretty = TRUE, auto_unbox = TRUE), con = file.path(enq_dir, "questions.json"))
    
    showNotification(paste("保存しました（ID:", sid, ")"), type = "message")
  })
  
}

shinyApp(ui, server)
