library(jsonlite)
library(uuid)
library(digest)
library(DBI)
library(RSQLite)

# =========================
# DB設定
# =========================

DATA_DIR <- "/srv/shiny-server/data"
DB_FILE <- file.path(
  DATA_DIR,
  "questionnaire.db"
)

# =========================
# DB初期化
# =========================

init_db <- function() {

  if (!dir.exists(DATA_DIR)) {
    dir.create(
      DATA_DIR,
      recursive = TRUE,
      showWarnings = FALSE
    )
  }

  con <- dbConnect(SQLite(), DB_FILE)
  on.exit(dbDisconnect(con), add = TRUE)

  # surveys
  dbExecute(con, "
    CREATE TABLE IF NOT EXISTS surveys (
      survey_id TEXT PRIMARY KEY,
      password_hash TEXT NOT NULL,
      title TEXT,
      description TEXT,
      created_at TEXT,
      updated_at TEXT
    )
  ")

  # questions
  dbExecute(con, "
    CREATE TABLE IF NOT EXISTS questions (
      question_id TEXT PRIMARY KEY,
      survey_id TEXT NOT NULL,
      title TEXT,
      description TEXT,
      type TEXT,
      order_index INTEGER,
      options_json TEXT,
      min_value REAL,
      max_value REAL,
      FOREIGN KEY(survey_id)
        REFERENCES surveys(survey_id)
    )
  ")

  # responses
  dbExecute(con, "
    CREATE TABLE IF NOT EXISTS responses (
      response_id TEXT PRIMARY KEY,
      survey_id TEXT NOT NULL,
      submitted_at TEXT,
      FOREIGN KEY(survey_id)
        REFERENCES surveys(survey_id)
    )
  ")

  # answers
  dbExecute(con, "
    CREATE TABLE IF NOT EXISTS answers (
      answer_id TEXT PRIMARY KEY,
      response_id TEXT NOT NULL,
      question_id TEXT NOT NULL,
      answer_text TEXT,
      FOREIGN KEY(response_id)
        REFERENCES responses(response_id)
    )
  ")
}

# =========================
# ハッシュ
# =========================

hash_pw <- function(pw) {
  digest(pw, algo = "sha256", serialize = FALSE)
}

# =========================
# アンケート一覧
# =========================

load_db <- function() {

  init_db()

  con <- dbConnect(SQLite(), DB_FILE)
  on.exit(dbDisconnect(con), add = TRUE)

  surveys <- dbReadTable(con, "surveys")

  if (nrow(surveys) == 0) {
    return(list())
  }

  res <- list()

  for (i in seq_len(nrow(surveys))) {

    row <- surveys[i, ]

    res[[row$survey_id]] <- list(
      password = row$password_hash,
      created  = row$created_at,
      qcount   = NA_integer_
    )
  }

  res
}

# =========================
# 質問読み込み
# =========================

load_questions <- function(survey_id) {

  init_db()

  con <- dbConnect(SQLite(), DB_FILE)
  on.exit(dbDisconnect(con), add = TRUE)

  qs_df <- dbGetQuery(
    con,
    "
      SELECT *
      FROM questions
      WHERE survey_id = ?
      ORDER BY order_index
    ",
    params = list(survey_id)
  )

  if (nrow(qs_df) == 0) {
    return(list())
  }

  qs <- list()

  for (i in seq_len(nrow(qs_df))) {

    row <- qs_df[i, ]

    options <- NULL

    if (!is.na(row$options_json) && nzchar(row$options_json)) {
      options <- fromJSON(row$options_json)
    }

    qs[[row$question_id]] <- list(
      id      = row$question_id,
      title   = row$title,
      desc    = row$description,
      type    = row$type,
      options = options,
      min     = if (is.na(row$min_value)) NULL else row$min_value,
      max     = if (is.na(row$max_value)) NULL else row$max_value
    )
  }

  qs
}

# =========================
# 回答保存
# =========================

save_response <- function(survey_id, answers) {

  init_db()

  con <- dbConnect(SQLite(), DB_FILE)
  on.exit(dbDisconnect(con), add = TRUE)

  response_id <- UUIDgenerate()

  dbExecute(
    con,
    "
      INSERT INTO responses
      (response_id, survey_id, submitted_at)
      VALUES (?, ?, ?)
    ",
    list(
      response_id,
      survey_id,
      as.character(Sys.time())
    )
  )

  for (qid in names(answers)) {

    val <- answers[[qid]]

    if (length(val) > 1) {
      val <- paste(val, collapse = ",")
    }

    dbExecute(
      con,
      "
        INSERT INTO answers
        (answer_id, response_id, question_id, answer_text)
        VALUES (?, ?, ?, ?)
      ",
      list(
        UUIDgenerate(),
        response_id,
        qid,
        as.character(val)
      )
    )
  }

  response_id
}

# =========================
# ★分析用：回答取得（追加部分）
# =========================

load_answers <- function(survey_id) {

  init_db()

  con <- dbConnect(SQLite(), DB_FILE)
  on.exit(dbDisconnect(con), add = TRUE)

  dbGetQuery(
    con,
    "
      SELECT
        r.response_id,
        r.submitted_at,
        a.question_id,
        a.answer_text
      FROM responses r
      JOIN answers a
        ON r.response_id = a.response_id
      WHERE r.survey_id = ?
      ORDER BY r.submitted_at
    ",
    params = list(survey_id)
  )
}

# =========================
# 正規化
# =========================

normalize_questions <- function(qlist) {

  lapply(qlist, function(q) {

    if (!is.null(q$options) && length(q$options) == 0) {
      q$options <- NULL
    }

    if (!is.null(q$min) && length(q$min) == 0) {
      q$min <- NULL
    }

    if (!is.null(q$max) && length(q$max) == 0) {
      q$max <- NULL
    }

    q
  })
}