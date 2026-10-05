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
    tags$head(tags$link(rel = "stylesheet", type = "text/css", href = "styles.css")),
    div(
      class = "answer-wrap",
      h1(class = "answer-heading", "アンケートに回答"),
      div(
        class = "load-card",
        textInput("survey_id", "アンケートID", placeholder = "例：survey-001"),
        actionButton("load", "アンケートを読み込む", class = "btn-primary")
      ),
      uiOutput("question_ui")
    )
  )
)
