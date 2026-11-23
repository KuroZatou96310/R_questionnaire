question_response_input <- function(id, question, value = NULL, label = NULL) {
  settings <- if (is.null(question$settings)) list() else question$settings
  options <- unlist(question$options, use.names = FALSE)
  selected <- if (is.null(value)) character() else as.character(value)
  display <- if (is.null(settings$display) || !length(settings$display)) "radio" else settings$display[1]

  switch(question$type,
    single_numeric = switch(display,
      slider = sliderInput(id, label, min = question$min, max = question$max,
                           value = if (length(selected)) as.numeric(selected[1]) else question$min, step = 1),
      dropdown = selectInput(id, label, c("選択してください" = "", options), selected = selected),
      horizontal = div(class = "numeric-scale",
        radioButtons(id, label, options, selected = selected, inline = TRUE),
        div(class = "numeric-scale-ends",
          span(if (is.null(settings$min_label)) "" else settings$min_label),
          span(if (is.null(settings$max_label)) "" else settings$max_label)
        )
      ),
      radioButtons(id, label, options, selected = selected)
    ),
    single_text = switch(display,
      dropdown = selectInput(id, label, c("選択してください" = "", options), selected = selected),
      horizontal = radioButtons(id, label, options, selected = selected, inline = TRUE),
      radioButtons(id, label, options, selected = selected)
    ),
    multiple = checkboxGroupInput(id, label, options, selected = selected, inline = identical(display, "horizontal")),
    numeric = {
      numeric_min <- if (!is.null(question$min) && length(question$min) && is.finite(suppressWarnings(as.numeric(question$min[1])))) as.numeric(question$min[1]) else NULL
      numeric_max <- if (!is.null(question$max) && length(question$max) && is.finite(suppressWarnings(as.numeric(question$max[1])))) as.numeric(question$max[1]) else NULL
      numeric_value <- if (length(selected)) suppressWarnings(as.numeric(selected[1])) else NA_real_
      if (is.null(numeric_min) && is.null(numeric_max)) {
        numericInput(id, label, value = numeric_value, step = "any")
      } else if (is.null(numeric_min)) {
        numericInput(id, label, value = numeric_value, max = numeric_max, step = "any")
      } else if (is.null(numeric_max)) {
        numericInput(id, label, value = numeric_value, min = numeric_min, step = "any")
      } else {
        numericInput(id, label, value = numeric_value, min = numeric_min, max = numeric_max, step = "any")
      }
    },
    text = if (isTRUE(settings$long_text)) textAreaInput(id, label, value = if (length(selected)) selected[1] else "", rows = 3) else textInput(id, label, value = if (length(selected)) selected[1] else ""),
    date = {
      control <- dateInput(id, label, value = if (length(selected)) selected[1] else Sys.Date())
      control$children[[2]]$attribs[["data-initial-date"]] <- ""
      control$children[[2]]$attribs$placeholder <- "日付を選択"
      control
    },
    NULL
  )
}
