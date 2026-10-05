library(shiny)
library(showtext)
library(ggplot2)

# Shiny Server の既定ロケールが C の場合でも、日本語をUTF-8として表示する
try(Sys.setlocale("LC_CTYPE", "C.UTF-8"), silent = TRUE)
font_add("jp", "/usr/share/fonts/opentype/noto/NotoSerifCJK-Bold.ttc")
showtext_auto()

source("db.R")

analysis_theme <- function() {
  theme_minimal(base_family = "jp", base_size = 13) +
    theme(
      plot.title = element_text(face = "bold", margin = margin(b = 14)),
      axis.title = element_text(face = "bold"),
      panel.grid.major.y = element_blank(),
      panel.grid.minor = element_blank()
    )
}

frequency_plot <- function(values, title, fill = "#2C7FB8", categories = NULL) {
  values <- as.character(values)
  if (!is.null(categories)) {
    categories <- unique(as.character(categories))
    values <- factor(values, levels = categories)
  }
  counts <- as.data.frame(table(values, useNA = "no"), stringsAsFactors = FALSE)
  names(counts) <- c("answer", "count")
  counts <- counts[counts$answer != "" & !is.na(counts$answer), , drop = FALSE]

  if (nrow(counts) == 0) {
    return(ggplot() + theme_void() +
      annotate("text", x = 0, y = 0, label = "回答なし", family = "jp"))
  }

  ggplot(counts, aes(x = reorder(answer, count), y = count)) +
    geom_col(fill = fill, width = 0.68) +
    geom_text(aes(label = count), hjust = -0.2, family = "jp") +
    coord_flip(clip = "off") +
    labs(title = title, x = NULL, y = "回答数") +
    expand_limits(y = max(counts$count, 0) * 1.15) +
    analysis_theme()
}

shinyServer(function(input, output, session){

  survey_questions <- reactiveVal(NULL)
  survey_answers   <- reactiveVal(NULL)


  #-------------------------
  # アンケート読込
  #-------------------------
  observeEvent(input$load,{

    req(
      nzchar(input$survey_id),
      nzchar(input$survey_pw)
    )

    db <- load_db()

    if(!(input$survey_id %in% names(db))){
      showNotification(
        "アンケートが見つかりません",
        type = "error"
      )
      return()
    }

    if(db[[input$survey_id]]$password !=
      hash_pw(input$survey_pw)){

      showNotification(
        "パスワードが違います",
        type = "error"
      )
      return()
    }

    survey_questions(
      load_questions(input$survey_id)
    )

    survey_answers(
      load_answers(input$survey_id)
    )

    showNotification(
      "読み込みました"
    )

  })

  #-------------------------
  # 回答数
  #-------------------------
  output$response_count <- renderText({

    req(survey_answers(), filtered_answers())

    total <- length(unique(survey_answers()$response_id))
    filtered <- length(unique(filtered_answers()$response_id))

    paste0("対象回答数：", filtered, " / ", total)

  })

  #-------------------------
  # UI生成
  #-------------------------
  output$analysis_ui <- renderUI({

    req(
      survey_questions(),
      survey_answers()
    )

    qs <- survey_questions()

    tagList(

      lapply(names(qs),function(id){

        q <- qs[[id]]

        tagList(

          hr(),

          h3(q$title),

          p(q$desc),

          plotOutput(
            paste0("plot_",id),
            height = "350px"
          ),

          tags$details(
            style = "margin:12px 0;border:1px solid #cbd5e1;border-radius:6px;overflow:hidden;",
            tags$summary(
              "▼ 回答件数一覧・統計値",
              style = "cursor:pointer;padding:10px 12px;background:#f1f5f9;font-weight:600;color:#1e293b;"
            ),
            tableOutput(paste0("table_", id))
          )

        )

      })

    )

  })

  #-------------------------
  # クロス集計
  #-------------------------
  categorical_question_ids <- reactive({

    req(survey_questions())

    ids <- names(survey_questions())
    ids[vapply(
      survey_questions()[ids],
      function(q) q$type %in% c("single", "select", "multiple", "date", "numeric", "slider"),
      logical(1)
    )]

  })

  output$crosstab_ui <- renderUI({

    ids <- categorical_question_ids()

    if (length(ids) < 2) {
      return(helpText("クロス集計には、選択式・複数選択式・日付の設問が2つ以上必要です。"))
    }

    qs <- survey_questions()
    choices <- setNames(ids, vapply(qs[ids], function(q) q$title, character(1)))

    tagList(
      selectInput("cross_row", "行にする設問", choices = choices, selected = ids[1]),
      selectInput("cross_col", "列にする設問", choices = choices, selected = ids[2]),

    )

  })

  expand_categorical_answers <- function(data, question_type) {

    if (is.null(question_type) || question_type != "multiple" || nrow(data) == 0) {
      return(data)
    }

    expanded <- lapply(seq_len(nrow(data)), function(i) {

      choices <- trimws(unlist(strsplit(as.character(data$answer_text[i]), ",", fixed = TRUE)))
      choices <- choices[nzchar(choices) & !is.na(choices)]

      if (length(choices) == 0) {
        return(NULL)
      }

      data.frame(
        response_id = rep(data$response_id[i], length(choices)),
        answer_text = choices,
        stringsAsFactors = FALSE
      )

    })

    expanded <- Filter(Negate(is.null), expanded)

    if (length(expanded) == 0) {
      return(data.frame(response_id = character(), answer_text = character()))
    }

    do.call(rbind, expanded)

  }

  # 回答していない設問（NULL、空文字）を除外して、必要な2列を必ず持つ形にする
  answered_question_data <- function(answers, question_id) {

    is_target <- answers$question_id == question_id
    answer_text <- as.character(answers$answer_text)
    has_answer <- !is.na(answer_text) & nzchar(trimws(answer_text))
    rows <- which(is_target & has_answer)

    data.frame(
      response_id = as.character(answers$response_id[rows]),
      answer_text = answer_text[rows],
      stringsAsFactors = FALSE
    )

  }

  question_categories <- function(question, observed_answers) {

    configured <- character()

    if (!is.null(question$options)) {
      configured <- enc2utf8(as.character(unlist(question$options, use.names = FALSE)))
    }

    observed <- enc2utf8(as.character(observed_answers))
    observed <- observed[!is.na(observed) & nzchar(trimws(observed))]

    unique(c(configured, observed))

  }

  #-------------------------
  # 共通フィルター
  #-------------------------
  filter_question_ids <- reactive({

    req(survey_questions())

    ids <- names(survey_questions())
    ids[vapply(
      survey_questions()[ids],
      function(q) q$type %in% c("single", "select", "multiple", "date"),
      logical(1)
    )]

  })

  output$filter_ui <- renderUI({

    ids <- filter_question_ids()

    if (length(ids) == 0) {
      return(helpText("絞り込みに利用できる設問がありません。"))
    }

    qs <- survey_questions()
    choices <- c("絞り込まない" = "", setNames(ids, vapply(qs[ids], function(q) q$title, character(1))))

    tagList(
      selectInput("filter_question", "設問", choices = choices, selected = ""),
      uiOutput("filter_value_ui"),
      actionButton("filter_clear", "条件をクリア", style = "width:100%;")
    )

  })

  output$filter_value_ui <- renderUI({

    req(survey_answers(), survey_questions())

    if (is.null(input$filter_question) || !nzchar(input$filter_question)) {
      return(NULL)
    }

    qs <- survey_questions()

    if (!(input$filter_question %in% names(qs))) {
      return(NULL)
    }

    question <- qs[[input$filter_question]]

    if (question$type %in% c("numeric", "slider")) {
      return(tagList(
        numericInput("filter_min", "以上", value = NA, step = "any"),
        numericInput("filter_max", "以下", value = NA, step = "any"),
        helpText("片方だけ指定することもできます。")
      ))
    }

    values <- answered_question_data(survey_answers(), input$filter_question)
    values <- expand_categorical_answers(values, question$type)
    choices <- question_categories(question, values$answer_text)

    selectInput("filter_values", "含める回答", choices = choices, multiple = TRUE)

  })

  observeEvent(input$filter_clear, {
    updateSelectInput(session, "filter_question", selected = "")
    updateNumericInput(session, "filter_min", value = NA)
    updateNumericInput(session, "filter_max", value = NA)
  })

  filtered_answers <- reactive({

    req(survey_answers())

    if (is.null(input$filter_question) || !nzchar(input$filter_question)) {
      return(survey_answers())
    }

    qs <- survey_questions()

    if (!(input$filter_question %in% names(qs))) {
      return(survey_answers())
    }

    question <- qs[[input$filter_question]]

    if (question$type %in% c("numeric", "slider")) {
      values <- answered_question_data(survey_answers(), input$filter_question)
      values$numeric_value <- suppressWarnings(as.numeric(values$answer_text))
      keep <- !is.na(values$numeric_value)

      if (!is.null(input$filter_min) && !is.na(input$filter_min)) {
        keep <- keep & values$numeric_value >= input$filter_min
      }
      if (!is.null(input$filter_max) && !is.na(input$filter_max)) {
        keep <- keep & values$numeric_value <= input$filter_max
      }

      response_ids <- unique(values$response_id[keep])
      return(survey_answers()[survey_answers()$response_id %in% response_ids, , drop = FALSE])
    }

    if (is.null(input$filter_values) || length(input$filter_values) == 0) {
      return(survey_answers())
    }

    filter_data <- answered_question_data(survey_answers(), input$filter_question)
    filter_data <- expand_categorical_answers(filter_data, qs[[input$filter_question]]$type)
    response_ids <- unique(filter_data$response_id[filter_data$answer_text %in% input$filter_values])

    survey_answers()[survey_answers()$response_id %in% response_ids, , drop = FALSE]

  })

  crosstab_data <- reactive({

    req(filtered_answers(), survey_questions(), input$cross_row, input$cross_col)

    qs <- survey_questions()

    empty_result <- function(message) {
      result <- data.frame(
        response_id = character(),
        answer_text_row = character(),
        answer_text_col = character(),
        stringsAsFactors = FALSE
      )
      attr(result, "message") <- message
      result
    }

    if (!isTRUE(input$cross_row != input$cross_col)) {
      return(empty_result("行と列には異なる設問を選んでください。"))
    }

    if (!(input$cross_row %in% names(qs) && input$cross_col %in% names(qs))) {
      return(empty_result("設問を選択してください。"))
    }

    ans <- filtered_answers()
    row_data <- answered_question_data(ans, input$cross_row)
    col_data <- answered_question_data(ans, input$cross_col)

    row_data <- expand_categorical_answers(row_data, qs[[input$cross_row]]$type)
    col_data <- expand_categorical_answers(col_data, qs[[input$cross_col]]$type)

    row_categories <- question_categories(qs[[input$cross_row]], row_data$answer_text)
    col_categories <- question_categories(qs[[input$cross_col]], col_data$answer_text)

    result <- merge(row_data, col_data, by = "response_id", suffixes = c("_row", "_col"))

    attr(result, "row_title") <- qs[[input$cross_row]]$title
    attr(result, "col_title") <- qs[[input$cross_col]]$title
    attr(result, "row_categories") <- row_categories
    attr(result, "col_categories") <- col_categories

    if (nrow(result) == 0) {
      attr(result, "message") <- "両方の設問に回答した人がいません。"
    }

    result

  })

  crosstab_counts <- reactive({

    d <- crosstab_data()

    if (!is.null(attr(d, "message"))) {
      return(NULL)
    }

    table(
      factor(d$answer_text_row, levels = attr(d, "row_categories")),
      factor(d$answer_text_col, levels = attr(d, "col_categories"))
    )

  })

  output$crosstab_table <- renderTable({

    d <- crosstab_data()

    message <- attr(d, "message")
    if (!is.null(message)) {
      result <- data.frame(message = message, stringsAsFactors = FALSE)
      names(result) <- "案内"
      return(result)
    }

    counts <- crosstab_counts()

    if (identical(input$cross_display, "percent")) {
      total <- sum(counts)
      values <- counts / total * 100
      values <- matrix(
        paste0(format(round(values, 1), nsmall = 1, trim = TRUE), "%"),
        nrow = nrow(counts),
        dimnames = dimnames(counts)
      )
      row_totals <- paste0(format(round(rowSums(counts) / total * 100, 1), nsmall = 1, trim = TRUE), "%")
      col_totals <- paste0(format(round(colSums(counts) / total * 100, 1), nsmall = 1, trim = TRUE), "%")
      grand_total <- "100.0%"
    } else {
      values <- counts
      row_totals <- rowSums(counts)
      col_totals <- colSums(counts)
      grand_total <- sum(counts)
    }

    values <- cbind(values, "合計" = row_totals)
    values <- rbind(values, "合計" = c(col_totals, grand_total))

    result <- data.frame(
      row_label = rownames(values),
      values,
      check.names = FALSE,
      stringsAsFactors = FALSE
    )
    names(result)[1] <- paste0(
      attr(d, "row_title"),
      "＼",
      attr(d, "col_title")
    )
    result

  }, rownames = FALSE)

  output$crosstab_heading <- renderText({
    if (identical(input$cross_display, "percent")) {
      "クロス集計表（構成比）"
    } else {
      "クロス集計表（件数）"
    }
  })

  output$crosstab_heatmap_heading <- renderText({
    if (identical(input$cross_display, "percent")) {
      "クロス集計ヒートマップ（構成比）"
    } else {
      "クロス集計ヒートマップ（件数）"
    }
  })

  output$crosstab_heatmap <- renderPlot({

    d <- crosstab_data()
    message <- attr(d, "message")

    if (!is.null(message)) {
      plot.new()
      text(0.5, 0.5, message)
      return()
    }

    counts <- crosstab_counts()
    cells <- as.data.frame(counts, stringsAsFactors = FALSE)
    names(cells) <- c("row", "column", "count")

    total <- sum(cells$count)
    is_percent <- identical(input$cross_display, "percent")

    if (is_percent) {
      cells$value <- cells$count / total * 100
      cells$label <- paste0(format(round(cells$value, 1), nsmall = 1, trim = TRUE), "%")
      fill_label <- "構成比（％）"
    } else {
      cells$value <- cells$count
      cells$label <- as.character(cells$count)
      fill_label <- "件数"
    }

    cells$row <- factor(cells$row, levels = rev(attr(d, "row_categories")))
    cells$column <- factor(cells$column, levels = attr(d, "col_categories"))

    ggplot(cells, aes(x = column, y = row, fill = value)) +
      geom_tile(color = "white", linewidth = 1) +
      geom_text(aes(label = label), family = "jp", size = 4.5) +
      scale_fill_gradient(
        low = "#F7FBFF",
        high = "#08519C",
        name = fill_label
      ) +
      labs(
        x = attr(d, "col_title"),
        y = attr(d, "row_title")
      ) +
      analysis_theme() +
      theme(
        axis.text.x = element_text(angle = 25, hjust = 1),
        panel.grid = element_blank()
      )

  })

  output$crosstab_stacked_plot <- renderPlot({

    d <- crosstab_data()
    message <- attr(d, "message")

    if (!is.null(message)) {
      plot.new()
      text(0.5, 0.5, message)
      return()
    }

    counts <- crosstab_counts()
    cells <- as.data.frame(counts, stringsAsFactors = FALSE)
    names(cells) <- c("row", "column", "count")

    if (identical(input$cross_stack_basis, "column")) {
      plot_data <- transform(cells, group = column, category = row)
      x_label <- attr(d, "col_title")
      fill_label <- attr(d, "row_title")
    } else {
      plot_data <- transform(cells, group = row, category = column)
      x_label <- attr(d, "row_title")
      fill_label <- attr(d, "col_title")
    }

    plot_data$group <- factor(plot_data$group)
    plot_data$category <- factor(plot_data$category)

    ggplot(plot_data, aes(x = group, y = count, fill = category)) +
      geom_col(position = "fill", width = 0.72, color = "white") +
      scale_y_continuous(labels = function(x) paste0(round(x * 100), "%")) +
      scale_fill_brewer(palette = "Set2", name = fill_label) +
      labs(x = x_label, y = "構成比") +
      analysis_theme() +
      theme(axis.text.x = element_text(angle = 20, hjust = 1))

  })

  crosstab_statistics <- reactive({

    d <- crosstab_data()
    counts <- crosstab_counts()

    if (!is.null(attr(d, "message")) || is.null(counts)) {
      return(list(message = attr(d, "message")))
    }

    active_counts <- counts[rowSums(counts) > 0, colSums(counts) > 0, drop = FALSE]

    if (nrow(active_counts) < 2 || ncol(active_counts) < 2) {
      return(list(message = "検定には、回答がある行・列がそれぞれ2項目以上必要です。"))
    }

    test <- tryCatch(
      suppressWarnings(chisq.test(active_counts, correct = FALSE)),
      error = function(e) e
    )

    if (inherits(test, "error")) {
      return(list(message = "カイ二乗検定を計算できませんでした。"))
    }

    n <- sum(active_counts)
    cramers_v <- sqrt(as.numeric(test$statistic) / (n * min(nrow(active_counts) - 1, ncol(active_counts) - 1)))
    small_expected <- sum(test$expected < 5)

    list(
      counts = active_counts,
      expected = test$expected,
      statistic = as.numeric(test$statistic),
      df = as.numeric(test$parameter),
      p_value = test$p.value,
      cramers_v = cramers_v,
      small_expected = small_expected,
      expected_cells = length(test$expected)
    )

  })

  output$crosstab_stats_table <- renderTable({

    stats <- crosstab_statistics()

    if (!is.null(stats$message)) {
      result <- data.frame(message = stats$message, check.names = FALSE)
      names(result) <- "案内"
      return(result)
    }

    result <- data.frame(
      metric = c("カイ二乗値", "自由度", "p値", "Cramér's V"),
      value = c(
        sprintf("%.3f", stats$statistic),
        stats$df,
        format.pval(stats$p_value, digits = 3, eps = 0.001),
        sprintf("%.3f", stats$cramers_v)
      ),
      check.names = FALSE
    )
    names(result) <- c("指標", "値")
    result

  }, rownames = FALSE)

  output$crosstab_stats_note <- renderText({

    stats <- crosstab_statistics()

    if (!is.null(stats$message)) return(stats$message)

    if (stats$small_expected > 0) {
      paste0(
        "注意：期待度数が5未満のセルが ", stats$small_expected, " / ",
        stats$expected_cells, " あります。カイ二乗検定の結果は慎重に解釈してください。"
      )
    } else {
      "すべての期待度数が5以上です。"
    }

  })

  output$crosstab_expected_table <- renderTable({

    stats <- crosstab_statistics()

    if (!is.null(stats$message)) {
      result <- data.frame(message = stats$message, check.names = FALSE)
      names(result) <- "案内"
      return(result)
    }

    expected <- round(stats$expected, 2)
    result <- data.frame(row = rownames(expected), expected, check.names = FALSE)
    names(result)[1] <- "行の選択肢"
    result

  }, rownames = FALSE)

  output$crosstab_note <- renderText({

    d <- crosstab_data()
    message <- attr(d, "message")
    if (!is.null(message)) return(message)

    paste0(
      "行：", attr(d, "row_title"), " ＼ 列：", attr(d, "col_title"),
      "。両方の設問に回答した ", length(unique(d$response_id)),
      " 人を集計しています。複数選択式は選択肢ごとに集計されます。"
    )

  })

  #-------------------------
  # 数値同士の分析
  #-------------------------
  numeric_question_ids <- reactive({

    req(survey_questions())

    ids <- names(survey_questions())
    ids[vapply(
      survey_questions()[ids],
      function(q) q$type %in% c("numeric", "slider"),
      logical(1)
    )]

  })

  output$numeric_analysis_ui <- renderUI({

    ids <- numeric_question_ids()

    if (length(ids) < 2) {
      return(helpText("数値同士の分析には、数値入力またはスライダーの設問が2つ以上必要です。"))
    }

    qs <- survey_questions()
    choices <- setNames(ids, vapply(qs[ids], function(q) q$title, character(1)))

    tagList(
      selectInput("numeric_x", "横軸の設問", choices = choices, selected = ids[1]),
      selectInput("numeric_y", "縦軸の設問", choices = choices, selected = ids[2])
    )

  })

  numeric_relation_data <- reactive({

    req(filtered_answers(), survey_questions(), input$numeric_x, input$numeric_y)

    empty_result <- function(message) {
      result <- data.frame(response_id = character(), x = numeric(), y = numeric())
      attr(result, "message") <- message
      result
    }

    if (!isTRUE(input$numeric_x != input$numeric_y)) {
      return(empty_result("横軸と縦軸には異なる設問を選んでください。"))
    }

    ans <- filtered_answers()
    x_answers <- answered_question_data(ans, input$numeric_x)
    y_answers <- answered_question_data(ans, input$numeric_y)
    x <- data.frame(response_id = x_answers$response_id, x = x_answers$answer_text)
    y <- data.frame(response_id = y_answers$response_id, y = y_answers$answer_text)

    d <- merge(x, y, by = "response_id")
    d$x <- suppressWarnings(as.numeric(d$x))
    d$y <- suppressWarnings(as.numeric(d$y))
    d <- d[complete.cases(d$x, d$y), , drop = FALSE]

    if (nrow(d) == 0) {
      attr(d, "message") <- "両方の設問に数値で回答した人がいません。"
    }

    d

  })

  output$numeric_relation_plot <- renderPlot({

    d <- numeric_relation_data()
    message <- attr(d, "message")

    if (!is.null(message)) {
      plot.new()
      text(0.5, 0.5, message)
      return()
    }

    qs <- survey_questions()
    plot_data <- data.frame(x = d$x, y = d$y)
    p <- ggplot(plot_data, aes(x = x, y = y)) +
      geom_point(size = 3, alpha = 0.72, color = "#2C7FB8") +
      labs(
        title = "数値同士の関係",
        x = qs[[input$numeric_x]]$title,
        y = qs[[input$numeric_y]]$title
      ) +
      analysis_theme()

    if (nrow(d) >= 2 && length(unique(d$x)) > 1) {
      p <- p + geom_smooth(method = "lm", se = TRUE, color = "#D95F0E", fill = "#FDD49E")
    }

    print(p)

  })

  numeric_relation_statistics <- reactive({

    d <- numeric_relation_data()
    message <- attr(d, "message")

    if (!is.null(message)) {
      return(list(message = message))
    }

    if (nrow(d) < 3 || length(unique(d$x)) < 2 || length(unique(d$y)) < 2) {
      return(list(message = "相関・回帰分析には、変動のある3件以上の回答が必要です。"))
    }

    pearson <- cor.test(d$x, d$y, method = "pearson")
    spearman <- suppressWarnings(cor.test(d$x, d$y, method = "spearman", exact = FALSE))
    model <- lm(y ~ x, data = d)
    standard_residuals <- rstandard(model)

    list(
      data = d,
      pearson = pearson,
      spearman = spearman,
      model = model,
      standard_residuals = standard_residuals,
      outlier_count = sum(abs(standard_residuals) > 2, na.rm = TRUE)
    )

  })

  output$numeric_relation_table <- renderTable({

    stats <- numeric_relation_statistics()
    message <- stats$message

    if (!is.null(message)) {
      result <- data.frame(message = message, stringsAsFactors = FALSE)
      names(result) <- "案内"
      return(result)
    }

    coefficients <- coef(stats$model)
    ci <- stats$pearson$conf.int

    result <- data.frame(
      metric = c(
        "有効回答数",
        "Pearson相関係数",
        "Pearson p値",
        "Pearson 95%信頼区間",
        "Spearman相関係数",
        "Spearman p値",
        "回帰式",
        "外れ値（標準化残差が±2超）"
      ),
      value = c(
        nrow(stats$data),
        sprintf("%.3f", unname(stats$pearson$estimate)),
        format.pval(stats$pearson$p.value, digits = 3, eps = 0.001),
        paste0("[", sprintf("%.3f", ci[1]), ", ", sprintf("%.3f", ci[2]), "]"),
        sprintf("%.3f", unname(stats$spearman$estimate)),
        format.pval(stats$spearman$p.value, digits = 3, eps = 0.001),
        paste0("y = ", sprintf("%.3f", coefficients[1]), " + ", sprintf("%.3f", coefficients[2]), " × x"),
        stats$outlier_count
      ),
      check.names = FALSE
    )

    names(result) <- c("指標", "値")
    result

  }, rownames = FALSE)

  output$numeric_residual_plot <- renderPlot({

    stats <- numeric_relation_statistics()

    if (!is.null(stats$message)) {
      plot.new()
      text(0.5, 0.5, stats$message)
      return()
    }

    residual_data <- data.frame(
      fitted = fitted(stats$model),
      residual = stats$standard_residuals,
      outlier = abs(stats$standard_residuals) > 2
    )

    ggplot(residual_data, aes(x = fitted, y = residual, color = outlier)) +
      geom_hline(yintercept = 0, color = "#666666", linewidth = 0.5) +
      geom_hline(yintercept = c(-2, 2), color = "#D95F0E", linetype = "dashed") +
      geom_point(size = 3, alpha = 0.8) +
      scale_color_manual(values = c("FALSE" = "#2C7FB8", "TRUE" = "#D95F0E"), labels = c("通常", "外れ値候補"), name = NULL) +
      labs(title = "標準化残差", x = "予測値", y = "標準化残差") +
      analysis_theme()

  })

  #-------------------------
  # グラフ・表生成
  #-------------------------
  observe({

    req(
      survey_questions(),
      filtered_answers()
    )

    qs <- survey_questions()
    ans <- filtered_answers()

    for(id in names(qs)){

      local({

        qid <- id
        q <- qs[[qid]]

        #---------------------
        # グラフ
        #---------------------

        output[[paste0("plot_",qid)]] <- renderPlot({

          d <- subset(
            ans,
            question_id == qid
          )

          if(nrow(d)==0){

            plot.new()

            text(
              0.5,
              0.5,
              "回答なし"
            )

            return()

          }

          switch(

            q$type,

            single = {
              print(frequency_plot(d$answer_text, q$title, categories = unlist(q$options)))

            },

            select = {
              print(frequency_plot(d$answer_text, q$title, categories = unlist(q$options)))

            },

            multiple = {

              x <- unlist(
                strsplit(
                  d$answer_text,
                  ","
                )
              )

              print(frequency_plot(trimws(x), q$title, "#F28E2B", unlist(q$options)))

            },

            numeric = {

              values <- suppressWarnings(as.numeric(d$answer_text))
              print(
                ggplot(data.frame(value = values), aes(x = value)) +
                  geom_histogram(bins = 12, fill = "#59A14F", color = "white") +
                  stat_bin(bins = 12, geom = "text", aes(label = after_stat(count)), vjust = -0.4, family = "jp") +
                  labs(title = q$title, x = "値", y = "回答数") +
                  scale_y_continuous(expand = expansion(mult = c(0, 0.16))) +
                  analysis_theme()
              )

            },

            slider = {

              values <- suppressWarnings(as.numeric(d$answer_text))
              print(
                ggplot(data.frame(value = values), aes(x = value)) +
                  geom_histogram(bins = 12, fill = "#59A14F", color = "white") +
                  stat_bin(bins = 12, geom = "text", aes(label = after_stat(count)), vjust = -0.4, family = "jp") +
                  labs(title = q$title, x = "値", y = "回答数") +
                  scale_y_continuous(expand = expansion(mult = c(0, 0.16))) +
                  analysis_theme()
              )

            },

            date = {

              print(frequency_plot(d$answer_text, q$title, "#AF7AA1", unlist(q$options)))

            },

            text = {

              plot.new()

              text(
                0.5,
                0.5,
                "自由記述は下表を参照"
              )

            }

          )

        })

        #---------------------
        # 表
        #---------------------

        output[[paste0("table_",qid)]] <- renderTable({

          d <- subset(
            ans,
            question_id == qid
          )

          switch(

            q$type,

            single = {

              tb <- as.data.frame(
                table(d$answer_text)
              )

              names(tb) <- c(
                "回答",
                "件数"
              )

              tb

            },

            select = {

              tb <- as.data.frame(
                table(d$answer_text)
              )

              names(tb) <- c(
                "回答",
                "件数"
              )

              tb

            },

            multiple = {

              x <- unlist(
                strsplit(
                  d$answer_text,
                  ","
                )
              )

              tb <- as.data.frame(
                table(trimws(x))
              )

              names(tb) <- c(
                "回答",
                "件数"
              )

              tb

            },

            numeric = {

              x <- as.numeric(
                d$answer_text
              )

              data.frame(

                key=c(
                  "平均",
                  "中央値",
                  "最小",
                  "最大",
                  "標準偏差"
                ),

                value=c(
                  mean(x,na.rm=TRUE),
                  median(x,na.rm=TRUE),
                  min(x,na.rm=TRUE),
                  max(x,na.rm=TRUE),
                  sd(x,na.rm=TRUE)
                )

              )

            },

            slider = {

              x <- as.numeric(
                d$answer_text
              )

              data.frame(

                key=c(
                  "平均",
                  "中央値",
                  "最小",
                  "最大",
                  "標準偏差"
                ),

                value=c(
                  mean(x,na.rm=TRUE),
                  median(x,na.rm=TRUE),
                  min(x,na.rm=TRUE),
                  max(x,na.rm=TRUE),
                  sd(x,na.rm=TRUE)
                )

              )

            },

            date = {

              tb <- as.data.frame(
                table(d$answer_text)
              )

              names(tb) <- c(
                "日付",
                "件数"
              )

              tb

            },

            text = {

              data.frame(

                answer_text_=d$answer_text,

                stringsAsFactors=FALSE

              )

            }

          )

        })

      })

    }

  })

  #-------------------------
  # CSVダウンロード
  #-------------------------
  output$download_csv <- downloadHandler(

    filename=function(){

      paste0(
        input$survey_id,
        "_answers.csv"
      )

    },

    content=function(file){

      req(survey_questions(), survey_answers())

      survey <- load_survey_info(input$survey_id)
      questions <- survey_questions()
      answers <- survey_answers()

      empty_text <- function(value) {
        if (length(value) == 0 || is.na(value)) "" else enc2utf8(as.character(value))
      }

      exported <- lapply(seq_len(nrow(answers)), function(i) {

        answer <- answers[i, , drop = FALSE]
        question <- questions[[answer$question_id]]

        choices <- if (is.null(question$options)) {
          ""
        } else {
          paste(enc2utf8(as.character(unlist(question$options, use.names = FALSE))), collapse = " | ")
        }

        data.frame(
          "アンケートID" = survey$id,
          "アンケートタイトル" = empty_text(survey$title),
          "アンケート説明" = empty_text(survey$description),
          "設問ID" = empty_text(question$id),
          "設問" = empty_text(question$title),
          "設問説明" = empty_text(question$desc),
          "種類" = empty_text(question$type),
          "選択肢" = choices,
          "最小値" = empty_text(question$min),
          "最大値" = empty_text(question$max),
          "回答ID" = empty_text(answer$response_id),
          "回答日時" = empty_text(answer$submitted_at),
          "回答" = empty_text(answer$answer_text),
          check.names = FALSE,
          stringsAsFactors = FALSE
        )

      })

      exported <- do.call(rbind, exported)

      con <- file(file, open = "wb")
      on.exit(close(con), add = TRUE)

      # Excelでも日本語を正しく表示できるUTF-8 BOM付きCSV
      writeBin(charToRaw("\ufeff"), con)
      write.csv(exported, con, row.names = FALSE, fileEncoding = "UTF-8", na = "")

    }

  )

})


