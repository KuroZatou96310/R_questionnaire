library(shiny)

# Define UI for application that plots random distributions 
shinyUI(fluidPage(
  tags$head(
    tags$link(rel = "stylesheet", type = "text/css", href = "questionnaire-admin.css"),
    tags$script(src = "questionnaire-dnd.js")
  ),
  titlePanel("アンケート生成アプリ（admin）"),
  
  sidebarLayout(
    sidebarPanel(
      h4("アンケート情報 (ID/PW)"),
      textInput("survey_id",  "アンケートID（空なら自動生成）"),
      passwordInput("survey_pw", "アンケートパスワード（必須）"),
      actionButton("load_survey", "既存アンケートを読み込む"),
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

      h4("質問順序のランダム化"),
      numericInput(
        "random_start",
        "X番目以降の設問をランダム化",
        value = NA,
        min = 1,
        step = 1
      ),
      helpText("単一グループでは指定番号より前を固定し、それ以降を並べ替えます。複数グループでは各グループの設定を使います。"),
      hr(),
      
      actionButton("save_survey", "アンケートを保存",
                   style = "color:white;background:#0072B2;width:100%;font-weight:bold;")
    ),
    
    mainPanel(
      tabsetPanel(
        tabPanel("設問一覧", uiOutput("question_list_ui")),
        tabPanel("回答画面プレビュー", uiOutput("answer_preview_ui"))
      )
    )
  )
))
