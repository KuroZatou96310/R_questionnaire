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
    try(dbExecute(con, "ALTER TABLE surveys ADD COLUMN random_start INTEGER"), silent = TRUE)
  }

  dbExecute(con, "
    CREATE TABLE IF NOT EXISTS question_groups (
      group_id TEXT PRIMARY KEY,
      survey_id TEXT NOT NULL,
      title TEXT NOT NULL,
      order_index INTEGER NOT NULL,
      settings_json TEXT NOT NULL DEFAULT '{}',
      FOREIGN KEY(survey_id) REFERENCES surveys(survey_id) ON DELETE CASCADE
    )
  ")
  dbExecute(con, "
    CREATE TABLE IF NOT EXISTS questions (
      question_id TEXT PRIMARY KEY,
      survey_id TEXT NOT NULL,
      group_id TEXT,
      title TEXT,
      description TEXT,
      type TEXT,
      order_index INTEGER,
      options_json TEXT,
      settings_json TEXT NOT NULL DEFAULT '{}',
      min_value REAL,
      max_value REAL,
      FOREIGN KEY(survey_id) REFERENCES surveys(survey_id) ON DELETE CASCADE
    )
  ")
  question_columns <- dbGetQuery(con, "PRAGMA table_info(questions)")$name
  if (!("group_id" %in% question_columns)) {
    try(dbExecute(con, "ALTER TABLE questions ADD COLUMN group_id TEXT"), silent = TRUE)
  }
  if (!("settings_json" %in% question_columns)) {
    try(dbExecute(con, "ALTER TABLE questions ADD COLUMN settings_json TEXT NOT NULL DEFAULT '{}'"), silent = TRUE)
  }
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
  eval(substitute(code), envir = list2env(list(con = con), parent = parent.frame()))
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

load_groups <- function(survey_id) with_db({
  groups <- dbGetQuery(con, "
    SELECT group_id, title, order_index, settings_json
    FROM question_groups WHERE survey_id = ? ORDER BY order_index
  ", list(survey_id))

  questions <- dbGetQuery(con, "SELECT question_id FROM questions WHERE survey_id = ? ORDER BY order_index",
                          list(survey_id))$question_id
  split_legacy_group <- nrow(groups) == 0 ||
    (nrow(groups) == 1 && identical(as.character(groups$title[1]), "グループ1") && length(questions) > 1)

  if (split_legacy_group) {
    inherited_settings <- if (nrow(groups) == 1) groups$settings_json[1] else "{}"
    if (nrow(groups) == 1) {
      dbExecute(con, "DELETE FROM question_groups WHERE survey_id = ?", list(survey_id))
    }
    for (i in seq_along(questions)) {
      group_id <- UUIDgenerate()
      dbExecute(con, "
        INSERT INTO question_groups (group_id, survey_id, title, order_index, settings_json)
        VALUES (?, ?, ?, ?, ?)
      ", list(group_id, survey_id, paste0("設問グループ", i), i - 1, inherited_settings))
      dbExecute(con, "UPDATE questions SET group_id = ? WHERE question_id = ?",
                list(group_id, questions[i]))
    }
  } else {
    unassigned <- dbGetQuery(con, "SELECT question_id FROM questions WHERE survey_id = ? AND group_id IS NULL",
                             list(survey_id))$question_id
    for (question_id in unassigned) {
      group_id <- UUIDgenerate()
      order_index <- nrow(groups)
      dbExecute(con, "
        INSERT INTO question_groups (group_id, survey_id, title, order_index, settings_json)
        VALUES (?, ?, ?, ?, '{}')
      ", list(group_id, survey_id, paste0("設問グループ", order_index + 1), order_index))
      dbExecute(con, "UPDATE questions SET group_id = ? WHERE question_id = ?",
                list(group_id, question_id))
    }
  }

  groups <- dbGetQuery(con, "
    SELECT group_id, title, order_index, settings_json
    FROM question_groups WHERE survey_id = ? ORDER BY order_index
  ", list(survey_id))

  result <- lapply(seq_len(nrow(groups)), function(i) {
    settings <- tryCatch(fromJSON(groups$settings_json[i], simplifyVector = FALSE), error = function(e) list())
    if (is.null(settings$page)) settings$page <- "group"
    list(id = groups$group_id[i], title = enc2utf8(groups$title[i]), settings = settings)
  })
  names(result) <- groups$group_id
  result
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
    settings <- tryCatch(fromJSON(row$settings_json, simplifyVector = FALSE), error = function(e) list())
    list(
      id = enc2utf8(as.character(row$question_id)),
      group_id = if (is.na(row$group_id) || !nzchar(row$group_id)) NA_character_ else as.character(row$group_id),
      title = enc2utf8(as.character(row$title)),
      desc = enc2utf8(as.character(row$description)),
      type = as.character(row$type),
      options = options,
      settings = settings,
      min = if (is.na(row$min_value)) NULL else row$min_value,
      max = if (is.na(row$max_value)) NULL else row$max_value
    )
  })
  names(questions) <- rows$question_id
  questions
})

save_db <- function(survey_id, password_hash, questions_list, question_ids, random_start = NULL,
                    groups_list = list(), group_ids = character()) with_db({
  now <- as.character(Sys.time())
  existing <- dbGetQuery(con, "SELECT COUNT(*) AS cnt FROM surveys WHERE survey_id = ?", list(survey_id))$cnt > 0

  if (!length(group_ids)) {
    group_id <- UUIDgenerate()
    group_ids <- group_id
    groups_list[[group_id]] <- list(id = group_id, title = "グループ1", settings = list(page = "group"))
    for (qid in question_ids) questions_list[[qid]]$group_id <- group_id
  }

  question_group_ids <- vapply(question_ids, function(id) {
    group_id <- questions_list[[id]]$group_id
    if (is.null(group_id) || !length(group_id)) NA_character_ else as.character(group_id[1])
  }, character(1))
  if (anyNA(question_group_ids) || any(!question_group_ids %in% group_ids)) {
    stop("すべての設問を有効なグループに所属させてください。")
  }

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

  old_group_ids <- dbGetQuery(con, "SELECT group_id FROM question_groups WHERE survey_id = ?", list(survey_id))$group_id
  for (i in seq_along(group_ids)) {
    id <- group_ids[i]
    group <- groups_list[[id]]
    settings <- if (is.null(group$settings)) list() else group$settings
    settings_json <- toJSON(settings, auto_unbox = TRUE, null = "null")
    if (id %in% old_group_ids) {
      dbExecute(con, "UPDATE question_groups SET title = ?, order_index = ?, settings_json = ? WHERE survey_id = ? AND group_id = ?",
                list(group$title, i - 1, settings_json, survey_id, id))
    } else {
      dbExecute(con, "INSERT INTO question_groups (group_id, survey_id, title, order_index, settings_json) VALUES (?, ?, ?, ?, ?)",
                list(id, survey_id, group$title, i - 1, settings_json))
    }
  }

  for (i in seq_along(question_ids)) {
    id <- question_ids[i]
    q <- questions_list[[id]]
    options_json <- if (is.null(q$options) || length(q$options) == 0) NA_character_ else toJSON(q$options, auto_unbox = TRUE)
    settings_json <- toJSON(if (is.null(q$settings)) list() else q$settings, auto_unbox = TRUE, null = "null")
    values <- list(q$group_id, q$title, q$desc, q$type, i - 1, options_json, settings_json,
                   if (is.null(q$min)) NA_real_ else q$min,
                   if (is.null(q$max)) NA_real_ else q$max)
    if (id %in% old_ids) {
      dbExecute(con, "
        UPDATE questions SET group_id = ?, title = ?, description = ?, type = ?, order_index = ?,
          options_json = ?, settings_json = ?, min_value = ?, max_value = ?
        WHERE survey_id = ? AND question_id = ?
      ", c(values, list(survey_id, id)))
    } else {
      dbExecute(con, "
        INSERT INTO questions
          (question_id, survey_id, group_id, title, description, type, order_index, options_json, settings_json, min_value, max_value)
        VALUES (?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?)
      ", c(list(id, survey_id), values))
    }
  }
  removed_group_ids <- setdiff(old_group_ids, group_ids)
  if (length(removed_group_ids)) {
    dbExecute(con, paste0("DELETE FROM question_groups WHERE survey_id = ? AND group_id IN (",
                          paste(rep("?", length(removed_group_ids)), collapse = ","), ")"),
              c(list(survey_id), as.list(removed_group_ids)))
  }
  TRUE
})

update_question_options <- function(survey_id, question_id, options) with_db({
  dbExecute(con, "
    UPDATE questions SET options_json = ? WHERE survey_id = ? AND question_id = ?
  ", list(toJSON(unique(as.character(options)), auto_unbox = TRUE), survey_id, question_id))
})

update_group_settings <- function(survey_id, group_id, settings) with_db({
  dbExecute(con, "
    UPDATE question_groups SET settings_json = ? WHERE survey_id = ? AND group_id = ?
  ", list(toJSON(settings, auto_unbox = TRUE, null = "null"), survey_id, group_id))
})

delete_question_and_answers <- function(survey_id, question_id) with_db({
  dbBegin(con)
  tryCatch({
    group_id <- dbGetQuery(con, "SELECT group_id FROM questions WHERE survey_id = ? AND question_id = ?",
                           list(survey_id, question_id))$group_id
    dbExecute(con, "
      DELETE FROM answers WHERE question_id = ?
        AND response_id IN (SELECT response_id FROM responses WHERE survey_id = ?)
    ", list(question_id, survey_id))
    dbExecute(con, "DELETE FROM questions WHERE survey_id = ? AND question_id = ?", list(survey_id, question_id))
    if (length(group_id) && !is.na(group_id)) {
      dbExecute(con, "DELETE FROM question_groups WHERE survey_id = ? AND group_id = ? AND NOT EXISTS (SELECT 1 FROM questions WHERE group_id = ?)",
                list(survey_id, group_id, group_id))
    }
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
