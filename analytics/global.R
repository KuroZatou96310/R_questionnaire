analysis_theme <- function() {
  ggplot2::theme_minimal(base_family = "jp", base_size = 13) +
    ggplot2::theme(
      plot.title = ggplot2::element_text(face = "bold", margin = ggplot2::margin(b = 14)),
      axis.title = ggplot2::element_text(face = "bold"),
      panel.grid.major.y = ggplot2::element_blank(),
      panel.grid.minor = ggplot2::element_blank()
    )
}

frequency_plot <- function(values, title, fill = "#2C7FB8", categories = NULL) {
  values <- as.character(values)
  if (!is.null(categories)) values <- factor(values, levels = unique(as.character(categories)))
  counts <- as.data.frame(table(values, useNA = "no"), stringsAsFactors = FALSE)
  names(counts) <- c("answer", "count")
  counts <- counts[counts$answer != "" & !is.na(counts$answer), , drop = FALSE]

  if (!nrow(counts)) {
    return(ggplot2::ggplot() + ggplot2::theme_void() +
      ggplot2::annotate("text", x = 0, y = 0, label = "回答なし", family = "jp"))
  }

  ggplot2::ggplot(counts, ggplot2::aes(x = reorder(answer, count), y = count)) +
    ggplot2::geom_col(fill = fill, width = 0.68) +
    ggplot2::geom_text(ggplot2::aes(label = count), hjust = -0.2, family = "jp") +
    ggplot2::coord_flip(clip = "off") +
    ggplot2::labs(title = title, x = NULL, y = "回答数") +
    ggplot2::expand_limits(y = max(counts$count) * 1.15) + analysis_theme()
}

word_tokens <- function(values) {
  text <- paste(as.character(values), collapse = " ")
  if (!nzchar(trimws(text))) return(character())
  tokens <- if (requireNamespace("stringi", quietly = TRUE)) {
    unlist(stringi::stri_split_boundaries(text, type = "word"))
  } else {
    chunks <- unlist(strsplit(gsub("[^[:alnum:]ぁ-んァ-ヶ一-龠]+", " ", text), "\\s+"))
    unlist(lapply(chunks, function(chunk) {
      chars <- strsplit(chunk, "", fixed = TRUE)[[1]]
      if (grepl("[ぁ-んァ-ヶ一-龠]", chunk) && length(chars) >= 3) {
        paste(chars[-length(chars)], chars[-1], sep = "")
      } else chunk
    }))
  }
  stopwords <- c("これ", "それ", "ため", "こと", "よう", "さん", "する", "なる", "ある", "いる", "この", "その", "から", "まで")
  tokens <- trimws(tokens)
  tokens[nzchar(tokens) & nchar(tokens) >= 2 & !tokens %in% stopwords]
}

question_values <- function(data, question) {
  values <- as.character(data$answer_text)
  if (question$type == "multiple") values <- trimws(unlist(strsplit(values, ",", fixed = TRUE)))
  values[!is.na(values) & nzchar(trimws(values))]
}

question_plot <- function(data, question) {
  if (!nrow(data)) {
    plot.new(); text(0.5, 0.5, "回答なし"); return(invisible(NULL))
  }
  values <- question_values(data, question)
  switch(question$type,
    single = frequency_plot(values, question$title, categories = question$options),
    select = frequency_plot(values, question$title, categories = question$options),
    date = frequency_plot(values, question$title, "#AF7AA1", question$options),
    multiple = frequency_plot(values, question$title, "#F28E2B", question$options),
    numeric = , slider = {
      data <- data.frame(value = suppressWarnings(as.numeric(values)))
      ggplot2::ggplot(data, ggplot2::aes(x = value)) +
        ggplot2::geom_histogram(bins = 12, fill = "#59A14F", color = "white") +
        ggplot2::stat_bin(bins = 12, geom = "text", ggplot2::aes(label = ggplot2::after_stat(count)),
                          vjust = -0.4, family = "jp") +
        ggplot2::labs(title = question$title, x = "値", y = "回答数") +
        ggplot2::scale_y_continuous(expand = ggplot2::expansion(mult = c(0, 0.16))) + analysis_theme()
    },
    text = { plot.new(); text(0.5, 0.5, "自由記述は下表を参照"); invisible(NULL) }
  )
}

question_summary <- function(data, question) {
  if (question$type %in% c("single", "select", "date", "multiple")) {
    result <- as.data.frame(table(question_values(data, question)), stringsAsFactors = FALSE)
    names(result) <- if (question$type == "date") c("日付", "件数") else c("回答", "件数")
    return(result)
  }
  if (question$type %in% c("numeric", "slider")) {
    values <- suppressWarnings(as.numeric(as.character(data$answer_text)))
    return(data.frame(
      key = c("平均", "中央値", "最小", "最大", "標準偏差"),
      value = c(mean(values, na.rm = TRUE), median(values, na.rm = TRUE), min(values, na.rm = TRUE),
                max(values, na.rm = TRUE), sd(values, na.rm = TRUE))
    ))
  }
  if (question$type == "text") return(data.frame(answer_text_ = data$answer_text, stringsAsFactors = FALSE))
  data.frame()
}

question_wordcloud <- function(data) {
  tokens <- word_tokens(data$answer_text)
  if (!length(tokens)) {
    plot.new(); text(0.5, 0.5, "ワードクラウドに表示できる回答がありません。"); return(invisible(NULL))
  }
  counts <- sort(table(tokens), decreasing = TRUE)
  par(family = "jp", mar = c(0, 0, 2, 0))
  wordcloud::wordcloud(names(counts), as.numeric(counts), min.freq = 1, random.order = FALSE,
                       rot.per = 0.12, colors = c("#287565", "#2C7FB8", "#D95F0E", "#7570B3", "#59A14F"),
                       family = "jp")
  title("自由記述のワードクラウド", family = "jp")
}
