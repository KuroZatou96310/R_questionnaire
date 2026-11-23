library(shiny)

shinyUI(fluidPage(
  tags$head(
    tags$title("アンケート作成画面"),
    tags$link(rel = "stylesheet", type = "text/css", href = "questionnaire-admin.css?v=20261122-1"),
    tags$script(src = "questionnaire-dnd.js?v=20261122-2")
  ),
  titlePanel("アンケート生成アプリ（admin）"),
  
  sidebarLayout(
    sidebarPanel(
      h4("アンケート情報"),
      textInput("survey_id",  "読み込むアンケートID"),
      passwordInput("survey_pw", "アンケートパスワード（必須）"),
      actionButton("load_survey", "既存アンケートを読み込む"),
      tags$div(
        style = "margin-top:14px;",
        textInput("survey_title", "アンケート名", placeholder = "例：新UIの使いやすさアンケート",
                  width = "100%")
      ),
      hr(),

      h4("標準化プリセット"),
      selectInput(
        "preset_name",
        "プリセットを選択",
        choices = c("選択してください" = "", "SUS", "UMUX-Lite", "UEQ", "UEQ-S", "NASA-TLX", "TAM", "GEQ", "IMI")
      ),
      actionButton("add_preset", "プリセットを設問に追加"),
      helpText("選択した質問群を現在の設問一覧の末尾へ追加します。"),
      hr(),

      actionButton("save_survey", "アンケートを保存",
                   style = "color:white;background:#0072B2;width:100%;font-weight:bold;"),
      uiOutput("access_link_ui")
    ),
    
    mainPanel(
      tabsetPanel(
        tabPanel("設問一覧", uiOutput("question_list_ui")),
        tabPanel("回答画面プレビュー", uiOutput("answer_preview_ui"))
      )
    )
  )
))
