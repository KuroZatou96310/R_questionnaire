library(jsonlite)
library(digest)
library(DBI)
library(RSQLite)

DATA_DIR <- Sys.getenv("QUESTIONNAIRE_DATA_DIR", "/srv/shiny-server/data")
DB_FILE <- file.path(DATA_DIR, "questionnaire.db")
db_last_id <- function(con) as.character(dbGetQuery(con, "SELECT last_insert_rowid() AS id")$id[1])

flatten_survey_groups <- function(groups) {
  questions <- list()
  for (group_id in names(groups)) for (id in names(groups[[group_id]]$questions)) {
    question <- groups[[group_id]]$questions[[id]]
    question$group_id <- group_id
    questions[[id]] <- question
  }
  questions
}

create_app_schema <- function(con) {
  dbExecute(con, "CREATE TABLE surveys (survey_id INTEGER PRIMARY KEY, legacy_id TEXT UNIQUE, password_hash TEXT NOT NULL, title TEXT, description TEXT, created_at TEXT, updated_at TEXT)")
  dbExecute(con, "CREATE TABLE question_groups (group_id INTEGER PRIMARY KEY, survey_id INTEGER NOT NULL REFERENCES surveys(survey_id) ON DELETE CASCADE, title TEXT NOT NULL, order_index INTEGER NOT NULL, settings_json TEXT NOT NULL DEFAULT '{}')")
  dbExecute(con, "CREATE TABLE questions (question_id INTEGER PRIMARY KEY, survey_id INTEGER NOT NULL REFERENCES surveys(survey_id) ON DELETE CASCADE, group_id INTEGER NOT NULL REFERENCES question_groups(group_id) ON DELETE CASCADE, title TEXT, description TEXT, type TEXT, order_index INTEGER NOT NULL, options_json TEXT, settings_json TEXT NOT NULL DEFAULT '{}', min_value REAL, max_value REAL)")
  dbExecute(con, "CREATE INDEX questions_group_order ON questions(group_id, order_index)")
  dbExecute(con, "CREATE TABLE responses (response_id INTEGER PRIMARY KEY, survey_id INTEGER NOT NULL REFERENCES surveys(survey_id) ON DELETE CASCADE, submitted_at TEXT)")
  dbExecute(con, "CREATE TABLE answers (answer_id INTEGER PRIMARY KEY,response_id INTEGER NOT NULL REFERENCES responses(response_id) ON DELETE CASCADE,question_id INTEGER NOT NULL REFERENCES questions(question_id) ON DELETE CASCADE,answer_text TEXT)")
}

migrate_items_schema <- function(con) {
  surveys <- dbReadTable(con, "surveys")
  items <- dbReadTable(con, "survey_items")
  responses <- dbReadTable(con, "responses")
  answers <- dbReadTable(con, "answers")
  dbExecute(con, "PRAGMA foreign_keys = OFF")
  dbBegin(con)
  tryCatch({
    for (table in c("answers", "responses", "survey_items", "surveys")) dbExecute(con, paste("DROP TABLE", table))
    create_app_schema(con)
    for (i in seq_len(nrow(surveys))) {
      row <- surveys[i, ]
      dbExecute(con, "INSERT INTO surveys (survey_id,legacy_id,password_hash,title,description,created_at,updated_at) VALUES (?,?,?,?,?,?,?)",
                unname(as.list(row[c("survey_id", "legacy_id", "password_hash", "title", "description", "created_at", "updated_at")])))
    }
    groups <- items[items$kind == "group", , drop = FALSE]
    for (i in seq_len(nrow(groups))) {
      row <- groups[i, ]
      dbExecute(con, "INSERT INTO question_groups (group_id,survey_id,title,order_index,settings_json) VALUES (?,?,?,?,?)",
                list(row$item_id, row$survey_id, row$title, row$order_index, row$settings_json))
    }
    questions <- items[items$kind == "question", , drop = FALSE]
    for (i in seq_len(nrow(questions))) {
      row <- questions[i, ]
      dbExecute(con, "INSERT INTO questions (question_id,survey_id,group_id,title,description,type,order_index,options_json,settings_json,min_value,max_value) VALUES (?,?,?,?,?,?,?,?,?,?,?)",
                list(row$item_id, row$survey_id, row$parent_id, row$title, row$description, row$type, row$order_index,
                     row$options_json, row$settings_json, row$min_value, row$max_value))
    }
    for (i in seq_len(nrow(responses))) {
      row <- responses[i, ]
      dbExecute(con, "INSERT INTO responses (response_id,survey_id,submitted_at) VALUES (?,?,?)",
                list(row$response_id, row$survey_id, row$submitted_at))
    }
    for (i in seq_len(nrow(answers))) {
      row <- answers[i, ]
      dbExecute(con, "INSERT INTO answers (answer_id,response_id,question_id,answer_text) VALUES (?,?,?,?)",
                list(row$answer_id, row$response_id, row$question_id, row$answer_text))
    }
    violations <- dbGetQuery(con, "PRAGMA foreign_key_check")
    if (nrow(violations)) stop("DB移行後に外部キー参照切れがあります。")
    dbCommit(con)
  }, error = function(e) { dbRollback(con); dbExecute(con,"PRAGMA foreign_keys = ON"); stop(e) })
  dbExecute(con,"PRAGMA foreign_keys = ON")
  invisible(TRUE)
}

init_db <- function() {
  dir.create(DATA_DIR, recursive = TRUE, showWarnings = FALSE)

  con <- dbConnect(SQLite(), DB_FILE)
  on.exit(dbDisconnect(con), add = TRUE)
  dbExecute(con, "PRAGMA foreign_keys = ON")

  tables <- dbListTables(con)
  if ("survey_items" %in% tables) {
    migrate_items_schema(con)
    return(invisible(TRUE))
  }
  if (!length(tables)) {
    create_app_schema(con)
    return(invisible(TRUE))
  }
  required <- list(surveys = c("survey_id", "legacy_id", "password_hash"),
    question_groups = c("group_id", "survey_id", "title", "order_index", "settings_json"),
    questions = c("question_id", "survey_id", "group_id", "settings_json", "min_value", "max_value"),
    responses = c("response_id", "survey_id"), answers = c("answer_id", "response_id", "question_id"))
  has_columns <- all(names(required) %in% tables) && all(vapply(names(required), function(table) {
    all(required[[table]] %in% dbGetQuery(con, paste0("PRAGMA table_info(", table, ")"))$name)
  }, logical(1)))
  if (!has_columns) stop("未対応のDB構造です。データを変更せずに停止しました。")
  integer_keys <- c(surveys = "survey_id", question_groups = "group_id", questions = "question_id", responses = "response_id", answers = "answer_id")
  has_integer_keys <- all(vapply(names(integer_keys), function(table) {
    info <- dbGetQuery(con, paste0("PRAGMA table_info(", table, ")"))
    row <- info[info$name == integer_keys[[table]], ]
    nrow(row) == 1 && toupper(row$type[1]) == "INTEGER" && row$pk[1] == 1
  }, logical(1)))
  if (!has_integer_keys) stop("未対応のDB構造です。データを変更せずに停止しました。")
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

resolve_survey_id <- function(con, id) {
  if (is.null(id) || !length(id) || is.na(id[1]) || !nzchar(as.character(id[1]))) return(NA_character_)
  row <- dbGetQuery(con, "SELECT survey_id FROM surveys WHERE CAST(survey_id AS TEXT) = ? OR legacy_id = ? ORDER BY CAST(survey_id AS TEXT) = ? DESC LIMIT 1",
                    list(as.character(id[1]), as.character(id[1]), as.character(id[1])))
  if (nrow(row)) as.character(row$survey_id[1]) else NA_character_
}

load_db <- function() with_db({
  surveys <- dbGetQuery(con, "SELECT survey_id, legacy_id, password_hash FROM surveys")
  if (nrow(surveys) == 0) return(list())
  result <- list()
  for (i in seq_len(nrow(surveys))) if (!is.na(surveys$legacy_id[i])) result[[surveys$legacy_id[i]]] <- list(password = surveys$password_hash[i])
  for (i in seq_len(nrow(surveys))) result[[as.character(surveys$survey_id[i])]] <- list(password = surveys$password_hash[i])
  result
})

load_survey_info <- function(survey_id) with_db({
  survey_id <- as.integer(resolve_survey_id(con, survey_id))
  if (is.na(survey_id)) return(list(id = "", title = "", description = ""))
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

load_survey_items <- function(survey_id) with_db({
  survey_id <- as.integer(resolve_survey_id(con, survey_id))
  if (is.na(survey_id)) return(list(groups = list()))
  groups_rows <- dbGetQuery(con, "SELECT * FROM question_groups WHERE survey_id = ? ORDER BY order_index", list(survey_id))
  groups <- list()
  for (i in seq_len(nrow(groups_rows))) {
    row <- groups_rows[i, ]
    settings <- tryCatch(fromJSON(row$settings_json, simplifyVector = FALSE), error = function(e) list())
    if (is.null(settings$page) || identical(settings$page, "group")) settings$page <- FALSE
    if (!is.null(settings$visibility$question_id)) settings$visibility$question_id <- as.character(settings$visibility$question_id)
    groups[[as.character(row$group_id)]] <- list(id = as.character(row$group_id), title = enc2utf8(row$title), settings = settings, questions = list())
  }
  question_rows <- dbGetQuery(con, "SELECT * FROM questions WHERE survey_id = ? ORDER BY group_id, order_index", list(survey_id))
  for (i in seq_len(nrow(question_rows))) {
    row <- question_rows[i, ]
    settings <- tryCatch(fromJSON(row$settings_json, simplifyVector = FALSE), error = function(e) list())
    options <- if (is.na(row$options_json) || !nzchar(row$options_json)) NULL else enc2utf8(as.character(unlist(fromJSON(row$options_json, simplifyVector = FALSE), use.names = FALSE)))
    type <- as.character(row$type)
    if (type == "slider") {
      settings$display <- "slider"
      if (!is.na(row$min_value) && !is.na(row$max_value) && row$min_value <= row$max_value &&
          row$min_value == floor(row$min_value) && row$max_value == floor(row$max_value)) {
        type <- "single_numeric"
        options <- as.character(seq.int(row$min_value, row$max_value))
      } else { type <- "numeric"; settings$display <- NULL }
    }
    if (type == "single") type <- if (length(options) && all(is.finite(suppressWarnings(as.numeric(options))))) "single_numeric" else "single_text"
    if (type == "select") { type <- "single_text"; settings$display <- "dropdown" }
    question_id <- as.character(row$question_id)
    question <- list(id = question_id, group_id = as.character(row$group_id),
      title = enc2utf8(row$title), desc = enc2utf8(row$description), type = type, options = options, settings = settings,
      min = if (is.na(row$min_value)) NULL else row$min_value, max = if (is.na(row$max_value)) NULL else row$max_value)
    question$group_id <- NULL
    groups[[as.character(row$group_id)]]$questions[[question_id]] <- question
  }
  list(groups = groups)
})

save_db <- function(survey_id = NULL, password_hash, groups_list, survey_title = NULL) with_db({
  group_ids <- names(groups_list)
  questions_list <- flatten_survey_groups(groups_list)
  question_ids <- names(questions_list)
  survey_title <- if (is.null(survey_title) || !length(survey_title)) "" else trimws(as.character(survey_title[1]))
  if (!length(question_ids)) stop("アンケートには設問が必要です。")
  if (!length(group_ids)) stop("設問グループがありません。")

  question_group_ids <- vapply(question_ids, function(id) {
    group_id <- questions_list[[id]]$group_id
    if (is.null(group_id) || !length(group_id)) NA_character_ else as.character(group_id[1])
  }, character(1))
  if (anyNA(question_group_ids) || any(!question_group_ids %in% group_ids)) {
    stop("すべての設問を有効なグループに所属させてください。")
  }
  group_positions <- setNames(seq_along(group_ids), group_ids)
  for (group_id in group_ids) {
    condition <- groups_list[[group_id]]$settings$visibility
    if (is.null(condition)) next
    source_id <- condition$question_id
    source <- if (is.null(source_id) || !source_id %in% question_ids) NULL else questions_list[[source_id]]
    source_position <- if (is.null(source)) NA_integer_ else group_positions[[source$group_id]]
    if (is.null(source_position) || is.na(source_position) || source_position >= group_positions[[group_id]]) {
      stop("グループの表示条件が有効な設問を参照していません。")
    }
  }
  dbBegin(con)
  tryCatch({
    now <- as.character(Sys.time())
    survey_id <- as.integer(resolve_survey_id(con, survey_id))
    existing <- !is.na(survey_id)
    old_groups <- if (existing) dbGetQuery(con, "SELECT group_id FROM question_groups WHERE survey_id = ?", list(survey_id))$group_id else integer()
    old_questions <- if (existing) dbGetQuery(con, "SELECT question_id FROM questions WHERE survey_id = ?", list(survey_id))$question_id else integer()

    if (!existing) {
      dbExecute(con, "INSERT INTO surveys (password_hash, title, created_at, updated_at) VALUES (?, ?, ?, ?)",
                list(password_hash, survey_title, now, now))
      survey_id <- db_last_id(con)
    }

    group_map <- setNames(character(length(group_ids)), group_ids)
    for (i in seq_along(group_ids)) {
      id <- group_ids[i]
      group <- groups_list[[id]]
      db_id <- group$id
      if (!is.null(db_id) && as.character(db_id) %in% as.character(old_groups)) {
        group_map[[id]] <- as.character(db_id)
        dbExecute(con, "UPDATE question_groups SET title = ?, order_index = ? WHERE survey_id = ? AND group_id = ?",
                  list(if (is.null(group$title)) paste0("設問グループ", i) else group$title, i - 1, survey_id, as.integer(db_id)))
      } else {
        dbExecute(con, "INSERT INTO question_groups (survey_id, title, order_index) VALUES (?, ?, ?)", list(survey_id, if (is.null(group$title)) paste0("設問グループ", i) else group$title, i - 1))
        group_map[[id]] <- db_last_id(con)
      }
    }

    question_map <- setNames(character(length(question_ids)), question_ids)
    for (i in seq_along(question_ids)) {
      id <- question_ids[i]
      q <- questions_list[[id]]
      options_json <- if (is.null(q$options) || !length(q$options)) NA_character_ else toJSON(q$options, auto_unbox = TRUE)
      settings_json <- toJSON(if (is.null(q$settings)) list() else q$settings, auto_unbox = TRUE, null = "null")
      values <- list(as.integer(survey_id), as.integer(group_map[[as.character(q$group_id)]]), q$title, q$desc, q$type, i - 1, options_json, settings_json,
                     if (is.null(q$min)) NA_real_ else q$min,
                     if (is.null(q$max)) NA_real_ else q$max)
      db_id <- q$id
      if (!is.null(db_id) && as.character(db_id) %in% as.character(old_questions)) {
        dbExecute(con, "UPDATE questions SET survey_id = ?, group_id = ?, title = ?, description = ?, type = ?, order_index = ?,
          options_json = ?, settings_json = ?, min_value = ?, max_value = ? WHERE survey_id = ? AND question_id = ?",
          c(values, list(survey_id, as.integer(db_id))))
        question_map[[id]] <- as.character(db_id)
      } else {
        dbExecute(con, "INSERT INTO questions (survey_id, group_id, title, description, type, order_index, options_json, settings_json, min_value, max_value) VALUES (?, ?, ?, ?, ?, ?, ?, ?, ?, ?)", values)
        question_map[[id]] <- db_last_id(con)
      }
    }

    for (i in seq_along(group_ids)) {
      id <- group_ids[i]
      settings <- groups_list[[id]]$settings
      condition_id <- settings$visibility$question_id
      if (!is.null(condition_id) && as.character(condition_id) %in% names(question_map)) {
        settings$visibility$question_id <- as.integer(question_map[[as.character(condition_id)]])
      }
      dbExecute(con, "UPDATE question_groups SET settings_json = ? WHERE group_id = ?",
                list(toJSON(if (is.null(settings)) list() else settings, auto_unbox = TRUE, null = "null"),
                     as.integer(group_map[[id]])))
    }

    removed_questions <- setdiff(old_questions, as.integer(unname(question_map)))
    for (id in removed_questions) dbExecute(con, "DELETE FROM questions WHERE question_id = ?", list(as.integer(id)))
    removed_groups <- setdiff(old_groups, as.integer(unname(group_map)))
    for (id in removed_groups) dbExecute(con, "DELETE FROM question_groups WHERE group_id = ?", list(as.integer(id)))

    if (existing) {
      dbExecute(con, "UPDATE surveys SET password_hash = ?, title = ?, updated_at = ? WHERE survey_id = ?",
                list(password_hash, survey_title, now, survey_id))
    }
    dbCommit(con)
    list(survey_id = survey_id, groups = group_map, questions = question_map)
  }, error = function(error) {
    dbRollback(con)
    stop(error)
  })
})

save_response <- function(survey_id, answers) with_db({
  survey_id <- as.integer(resolve_survey_id(con, survey_id))
  if (is.na(survey_id)) stop("アンケートが見つかりません。")
  dbBegin(con)
  tryCatch({
    dbExecute(con, "INSERT INTO responses (survey_id, submitted_at) VALUES (?, ?)",
              list(survey_id, as.character(Sys.time())))
    response_id <- db_last_id(con)
    for (qid in names(answers)) {
      value <- answers[[qid]]
      if (length(value) > 1) value <- paste(value, collapse = ",")
      dbExecute(con, "INSERT INTO answers (response_id, question_id, answer_text) VALUES (?, ?, ?)",
                list(as.integer(response_id), as.integer(qid), as.character(value)))
    }
    dbCommit(con)
    response_id
  }, error = function(error) {
    dbRollback(con)
    stop(error)
  })
})

load_response_count <- function(survey_id) with_db({
  survey_id <- as.integer(resolve_survey_id(con, survey_id))
  if (is.na(survey_id)) return(0L)
  as.integer(dbGetQuery(con, "SELECT COUNT(*) AS count FROM responses WHERE survey_id = ?", list(survey_id))$count[1])
})

load_answers <- function(survey_id) with_db({
  survey_id <- as.integer(resolve_survey_id(con, survey_id))
  if (is.na(survey_id)) return(data.frame(response_id = character(), submitted_at = character(), question_id = character(), answer_text = character()))
  answers <- dbGetQuery(con, "
    SELECT r.response_id, r.submitted_at, a.question_id, a.answer_text
    FROM responses r JOIN answers a ON r.response_id = a.response_id
    WHERE r.survey_id = ? ORDER BY r.submitted_at
  ", list(survey_id))
  answers$answer_text <- enc2utf8(as.character(answers$answer_text))
  answers$response_id <- as.character(answers$response_id)
  answers$question_id <- as.character(answers$question_id)
  answers
})
