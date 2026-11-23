library(shiny)

shinyUI(fluidPage(
  tags$head(
    tags$title("アンケート作成画面"),
    tags$link(rel = "stylesheet", type = "text/css", href = "questionnaire-admin.css?v=20261123-4"),
    tags$script(src = "questionnaire-dnd.js?v=20261122-2")
  ),
  titlePanel("アンケート作成"),
  
  sidebarLayout(
    sidebarPanel(
      uiOutput("survey_id_ui"),
      passwordInput("survey_pw", "アンケートパスワード（必須）"),
      uiOutput("survey_actions_ui"),
      uiOutput("survey_editor_controls_ui")
    ),
    mainPanel(
      uiOutput("admin_content_ui")
    )
  )
))
