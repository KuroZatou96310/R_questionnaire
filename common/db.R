library(jsonlite)
library(uuid)
library(digest)
library(DBI)
library(RSQLite)

DATA_DIR <- "/srv/shiny-server/data"
DB_FILE <- file.path(DATA_DIR, "questionnaire.db")

init_db <- function() {
  dir.create(DATA_DIR, recursive = TRUE, showWarnings = FALSE)

  con <- dbConnect(SQLite(), DB_FILE)
  on.exit(dbDisconnect(con), add = TRUE)
  dbExecute(con, "PRAGMA foreign_keys = ON")

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
  survey_columns <- dbGetQuery(con, "PRAGMA table_info(surveys)")$name
  if (!("random_start" %in% survey_columns)) {
    dbExecute(con, "ALTER TABLE surveys ADD COLUMN random_start INTEGER")
  }

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
      FOREIGN KEY(survey_id) REFERENCES surveys(survey_id) ON DELETE CASCADE
    )
  ")
  dbExecute(con, "
    CREATE TABLE IF NOT EXISTS responses (
      response_id TEXT PRIMARY KEY,
      survey_id TEXT NOT NULL,
      submitted_at TEXT,
      FOREIGN KEY(survey_id) REFERENCES surveys(survey_id) ON DELETE CASCADE
    )
  ")
  dbExecute(con, "
    CREATE TABLE IF NOT EXISTS answers (
      answer_id TEXT PRIMARY KEY,
      response_id TEXT NOT NULL,
      question_id TEXT NOT NULL,
      answer_text TEXT,
      FOREIGN KEY(response_id) REFERENCES responses(response_id) ON DELETE CASCADE,
      FOREIGN KEY(question_id) REFERENCES questions(question_id) ON DELETE CASCADE
    )
  ")
  invisible(TRUE)
}

.db_initialized <- FALSE

ensure_db <- function() {
  if (!.db_initialized) {
    init_db()
    .db_initialized <<- TRUE
  }
  invisible(TRUE)
}

connect_db <- function() {
  con <- dbConnect(SQLite(), DB_FILE)
  dbExecute(con, "PRAGMA foreign_keys = ON")
  con
}

with_db <- function(code) {
  ensure_db()
  con <- connect_db()
  on.exit(dbDisconnect(con), add = TRUE)
  eval(substitute(code), envir = environment())
}

hash_pw <- function(pw) digest(pw, algo = "sha256", serialize = FALSE)

load_db <- function() with_db({
  surveys <- dbReadTable(con, "surveys")
  if (nrow(surveys) == 0) return(list())

  setNames(lapply(seq_len(nrow(surveys)), function(i) {
    row <- surveys[i, ]
    list(password = row$password_hash, created = row$created_at, qcount = NA_integer_)
  }), surveys$survey_id)
})

load_survey_info <- function(survey_id) with_db({
  survey <- dbGetQuery(con, "
    SELECT survey_id, title, description FROM surveys WHERE survey_id = ?
  ", params = list(survey_id))
  if (nrow(survey) == 0) return(list(id = survey_id, title = "", description = ""))
  list(
    id = enc2utf8(as.character(survey$survey_id[1])),
    title = enc2utf8(as.character(survey$title[1])),
    description = enc2utf8(as.character(survey$description[1]))
  )
})

load_random_start <- function(survey_id) with_db({
  value <- dbGetQuery(con, "SELECT random_start FROM surveys WHERE survey_id = ?", list(survey_id))
  if (nrow(value) == 0 || is.na(value$random_start[1])) NULL else as.integer(value$random_start[1])
})

load_questions <- function(survey_id) with_db({
  rows <- dbGetQuery(con, "
    SELECT * FROM questions WHERE survey_id = ? ORDER BY order_index
  ", list(survey_id))
  if (nrow(rows) == 0) return(list())

  questions <- lapply(seq_len(nrow(rows)), function(i) {
    row <- rows[i, ]
    options <- NULL
    if (!is.na(row$options_json) && nzchar(row$options_json)) {
      options <- fromJSON(enc2utf8(row$options_json), simplifyVector = FALSE)
      options <- enc2utf8(as.character(unlist(options, use.names = FALSE)))
    }
    list(
      id = enc2utf8(as.character(row$question_id)),
      title = enc2utf8(as.character(row$title)),
      desc = enc2utf8(as.character(row$description)),
      type = as.character(row$type),
      options = options,
      min = if (is.na(row$min_value)) NULL else row$min_value,
      max = if (is.na(row$max_value)) NULL else row$max_value
    )
  })
  names(questions) <- rows$question_id
  questions
})

save_db <- function(survey_id, password_hash, questions_list, question_ids, random_start = NULL) with_db({
  now <- as.character(Sys.time())
  existing <- dbGetQuery(con, "SELECT COUNT(*) AS cnt FROM surveys WHERE survey_id = ?", list(survey_id))$cnt > 0

  if (existing) {
    old_ids <- dbGetQuery(con, "SELECT question_id FROM questions WHERE survey_id = ?", list(survey_id))$question_id
    removed_ids <- setdiff(old_ids, question_ids)
    for (id in removed_ids) {
      dbExecute(con, "DELETE FROM answers WHERE question_id = ?", list(id))
      dbExecute(con, "DELETE FROM questions WHERE survey_id = ? AND question_id = ?", list(survey_id, id))
    }
    dbExecute(con, "
      UPDATE surveys SET password_hash = ?, updated_at = ?, random_start = ? WHERE survey_id = ?
    ", list(password_hash, now, if (is.null(random_start)) NA_integer_ else as.integer(random_start), survey_id))
  } else {
    old_ids <- character()
    dbExecute(con, "
      INSERT INTO surveys (survey_id, password_hash, created_at, updated_at, random_start)
      VALUES (?, ?, ?, ?, ?)
    ", list(survey_id, password_hash, now, now, if (is.null(random_start)) NA_integer_ else as.integer(random_start)))
  }

  for (i in seq_along(question_ids)) {
    id <- question_ids[i]
    q <- questions_list[[id]]
    options_json <- if (is.null(q$options) || length(q$options) == 0) NA_character_ else toJSON(q$options, auto_unbox = TRUE)
    values <- list(q$title, q$desc, q$type, i - 1, options_json,
                   if (is.null(q$min)) NA_real_ else q$min,
                   if (is.null(q$max)) NA_real_ else q$max)
    if (id %in% old_ids) {
      dbExecute(con, "
        UPDATE questions SET title = ?, description = ?, type = ?, order_index = ?,
          options_json = ?, min_value = ?, max_value = ?
        WHERE survey_id = ? AND question_id = ?
      ", c(values, list(survey_id, id)))
    } else {
      dbExecute(con, "
        INSERT INTO questions
          (question_id, survey_id, title, description, type, order_index, options_json, min_value, max_value)
        VALUES (?, ?, ?, ?, ?, ?, ?, ?, ?)
      ", c(list(id, survey_id), values))
    }
  }
  TRUE
})

update_question_options <- function(survey_id, question_id, options) with_db({
  dbExecute(con, "
    UPDATE questions SET options_json = ? WHERE survey_id = ? AND question_id = ?
  ", list(toJSON(unique(as.character(options)), auto_unbox = TRUE), survey_id, question_id))
})

delete_question_and_answers <- function(survey_id, question_id) with_db({
  dbBegin(con)
  tryCatch({
    dbExecute(con, "
      DELETE FROM answers WHERE question_id = ?
        AND response_id IN (SELECT response_id FROM responses WHERE survey_id = ?)
    ", list(question_id, survey_id))
    dbExecute(con, "DELETE FROM questions WHERE survey_id = ? AND question_id = ?", list(survey_id, question_id))
    dbCommit(con)
    invisible(TRUE)
  }, error = function(error) {
    dbRollback(con)
    stop(error)
  })
})

save_response <- function(survey_id, answers) with_db({
  response_id <- UUIDgenerate()
  dbExecute(con, "INSERT INTO responses (response_id, survey_id, submitted_at) VALUES (?, ?, ?)",
            list(response_id, survey_id, as.character(Sys.time())))
  for (qid in names(answers)) {
    value <- answers[[qid]]
    if (length(value) > 1) value <- paste(value, collapse = ",")
    dbExecute(con, "INSERT INTO answers (answer_id, response_id, question_id, answer_text) VALUES (?, ?, ?, ?)",
              list(UUIDgenerate(), response_id, qid, as.character(value)))
  }
  response_id
})

load_answers <- function(survey_id) with_db({
  answers <- dbGetQuery(con, "
    SELECT r.response_id, r.submitted_at, a.question_id, a.answer_text
    FROM responses r JOIN answers a ON r.response_id = a.response_id
    WHERE r.survey_id = ? ORDER BY r.submitted_at
  ", list(survey_id))
  answers$answer_text <- enc2utf8(as.character(answers$answer_text))
  answers
})

normalize_questions <- function(qlist) {
  lapply(qlist, function(q) {
    if (!is.null(q$options) && length(q$options) == 0) q$options <- NULL
    if (!is.null(q$min) && length(q$min) == 0) q$min <- NULL
    if (!is.null(q$max) && length(q$max) == 0) q$max <- NULL
    q
  })
}
