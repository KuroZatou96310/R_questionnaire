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
    tags$head(
      tags$title("アンケート回答"),
      tags$link(rel = "stylesheet", type = "text/css", href = "styles.css?v=20261123-10"),
      tags$script(HTML("Shiny.addCustomMessageHandler('page-title', function(title) { document.title = title; });"))
    ),
    div(
      class = "answer-wrap",
      uiOutput("load_panel_ui"),
      uiOutput("question_ui")
    )
  )
)
