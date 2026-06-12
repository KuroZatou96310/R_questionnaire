library(jsonlite)
library(uuid)
library(digest)
library(DBI)
library(RSQLite)

# =========================
# DB設定
# =========================

DATA_DIR <- "/srv/shiny-server/enquete_app_data"
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

  con <- dbConnect(
    SQLite(),
    DB_FILE
  )

  on.exit(
    dbDisconnect(con),
    add = TRUE
  )

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
# パスワードハッシュ
# =========================

hash_pw <- function(pw) {
  digest(
    pw,
    algo = "sha256",
    serialize = FALSE
  )
}

# =========================
# アンケート一覧取得
# =========================

load_db <- function() {

  init_db()

  con <- dbConnect(
    SQLite(),
    DB_FILE
  )

  on.exit(
    dbDisconnect(con),
    add = TRUE
  )

  surveys <- dbReadTable(
    con,
    "surveys"
  )

  if (nrow(surveys) == 0) {
    return(list())
  }

  result <- list()

  for (i in seq_len(nrow(surveys))) {

    row <- surveys[i, ]

    result[[row$survey_id]] <- list(
      password = row$password_hash,
      created = row$created_at,
      qcount = NA_integer_
    )

  }

  result
}

# =========================
# 質問読み込み
# =========================

load_questions <- function(
  survey_id
) {

  init_db()

  con <- dbConnect(
    SQLite(),
    DB_FILE
  )

  on.exit(
    dbDisconnect(con),
    add = TRUE
  )

  qs_df <- dbGetQuery(
    con,
    "
      SELECT *
      FROM questions
      WHERE survey_id = ?
      ORDER BY order_index
    ",
    list(survey_id)
  )

  if (nrow(qs_df) == 0) {
    return(list())
  }

  questions <- list()

  for (i in seq_len(nrow(qs_df))) {

    row <- qs_df[i, ]

    options <- NULL

    if (
      !is.na(row$options_json) &&
      nzchar(row$options_json)
    ) {
      options <- fromJSON(
        row$options_json,
        simplifyVector = FALSE
      )
    }

    questions[[row$question_id]] <- list(
      id = row$question_id,
      title = row$title,
      desc = row$description,
      type = row$type,
      options = options,
      min = if (is.na(row$min_value))
        NULL else row$min_value,
      max = if (is.na(row$max_value))
        NULL else row$max_value
    )
  }

  questions
}

# =========================
# アンケート保存
# =========================

save_db <- function(
  survey_id,
  password_hash,
  questions_list,
  question_ids
) {

  init_db()

  con <- dbConnect(
    SQLite(),
    DB_FILE
  )

  on.exit(
    dbDisconnect(con),
    add = TRUE
  )

  created_at <- as.character(
    Sys.time()
  )

  existing <- dbGetQuery(
    con,
    "
      SELECT COUNT(*) AS cnt
      FROM surveys
      WHERE survey_id = ?
    ",
    list(survey_id)
  )

  if (existing$cnt > 0) {

    dbExecute(
      con,
      "
      DELETE FROM questions
      WHERE survey_id = ?
      ",
      list(survey_id)
    )

    dbExecute(
      con,
      "
      UPDATE surveys
      SET password_hash = ?,
          updated_at = ?
      WHERE survey_id = ?
      ",
      list(
        password_hash,
        created_at,
        survey_id
      )
    )

  } else {

    dbExecute(
      con,
      "
      INSERT INTO surveys
      (
        survey_id,
        password_hash,
        created_at,
        updated_at
      )
      VALUES
      (
        ?, ?, ?, ?
      )
      ",
      list(
        survey_id,
        password_hash,
        created_at,
        created_at
      )
    )

  }

  for (i in seq_along(question_ids)) {

    qid <- question_ids[i]
    q <- questions_list[[qid]]

    options_json <- if (
      is.null(q$options) ||
      length(q$options) == 0
    ) {
      NA_character_
    } else {
      toJSON(
        q$options,
        auto_unbox = TRUE
      )
    }

    dbExecute(
      con,
      "
      INSERT INTO questions
      (
        question_id,
        survey_id,
        title,
        description,
        type,
        order_index,
        options_json,
        min_value,
        max_value
      )
      VALUES
      (
        ?, ?, ?, ?, ?, ?, ?, ?, ?
      )
      ",
      list(
        qid,
        survey_id,
        q$title,
        q$desc,
        q$type,
        i - 1,
        options_json,
        if (is.null(q$min))
          NA_real_ else q$min,
        if (is.null(q$max))
          NA_real_ else q$max
      )
    )

  }

  TRUE
}

# =========================
# 回答保存
# =========================

save_response <- function(
  survey_id,
  answers
) {

  init_db()

  con <- dbConnect(
    SQLite(),
    DB_FILE
  )

  on.exit(
    dbDisconnect(con),
    add = TRUE
  )

  response_id <- UUIDgenerate()

  dbExecute(
    con,
    "
    INSERT INTO responses
    (
      response_id,
      survey_id,
      submitted_at
    )
    VALUES
    (
      ?, ?, ?
    )
    ",
    list(
      response_id,
      survey_id,
      as.character(Sys.time())
    )
  )

  for (qid in names(answers)) {

    answer_value <- answers[[qid]]

    if (length(answer_value) > 1) {
      answer_value <- paste(
        answer_value,
        collapse = ","
      )
    }

    dbExecute(
      con,
      "
      INSERT INTO answers
      (
        answer_id,
        response_id,
        question_id,
        answer_text
      )
      VALUES
      (
        ?, ?, ?, ?
      )
      ",
      list(
        UUIDgenerate(),
        response_id,
        qid,
        as.character(answer_value)
      )
    )
  }

  response_id
}

# =========================
# 正規化
# =========================

normalize_questions <- function(
  qlist
) {

  lapply(
    qlist,
    function(q) {

      if (
        !is.null(q$options) &&
        length(q$options) == 0
      ) {
        q$options <- NULL
      }

      if (
        !is.null(q$min) &&
        length(q$min) == 0
      ) {
        q$min <- NULL
      }

      if (
        !is.null(q$max) &&
        length(q$max) == 0
      ) {
        q$max <- NULL
      }

      q
    }
  )
}