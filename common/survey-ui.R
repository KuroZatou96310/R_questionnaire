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
      horizontal = radioButtons(id, label, options, selected = selected, inline = TRUE),
      radioButtons(id, label, options, selected = selected)
    ),
    single_text = switch(display,
      dropdown = selectInput(id, label, c("選択してください" = "", options), selected = selected),
      horizontal = radioButtons(id, label, options, selected = selected, inline = TRUE),
      radioButtons(id, label, options, selected = selected)
    ),
    multiple = checkboxGroupInput(id, label, options, selected = selected, inline = identical(display, "horizontal")),
    numeric = numericInput(id, label, value = if (length(selected)) as.numeric(selected[1]) else NA, min = question$min, max = question$max, step = "any"),
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
