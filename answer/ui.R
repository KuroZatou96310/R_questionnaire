library(shiny)
library(bslib)

shinyUI(
  page_fluid(
    theme = bs_theme(
      version = 5,
      bootswatch = "minty",
      primary = "#287565",
      bg = "#D5E6D9",
      fg = "#20243A"
    ),
    tags$head(tags$link(rel = "stylesheet", type = "text/css", href = "styles.css?v=20261116-2")),
    div(
      class = "answer-wrap",
      h1(class = "answer-heading", "アンケートに回答"),
      uiOutput("load_panel_ui"),
      uiOutput("question_ui")
    )
  )
)
