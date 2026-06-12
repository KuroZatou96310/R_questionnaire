library(jsonlite)
library(uuid)
library(digest)   # パスワードハッシュ化用
library(DBI)
library(RSQLite)
BASE_DIR <- getwd()

DATA_DIR <- file.path(
  BASE_DIR,
  "enquete_app_data"
)
DB_FILE  <- file.path(DATA_DIR, "questionnaire.db")
if (!dir.exists(DATA_DIR)) dir.create(DATA_DIR, recursive = TRUE)

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