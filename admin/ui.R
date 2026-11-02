library(shiny)

# Define UI for application that plots random distributions 
shinyUI(fluidPage(
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
      helpText("指定した番号より前の設問は固定し、それ以降を回答者ごとに並べ替えます。"),
      hr(),
      
      h4("設問を追加"),
      textInput("q_title", "設問タイトル"),
      textAreaInput("q_desc", "設問説明"),
      selectInput("q_type", "設問タイプ",
                  choices = c("single", "multiple", "numeric", "text",
                              "select", "slider", "date")),
      conditionalPanel(
        condition = "['single','multiple','select'].includes(input.q_type)",
        textAreaInput("q_options", "選択肢（改行区切り）",
                      placeholder = "例：はい\nいいえ\nわからない")
      ),
      conditionalPanel(
        condition = "input.q_type == 'slider'",
        numericInput("q_min", "下限値", 0),
        numericInput("q_max", "上限値", 100)
      ),
      actionButton("add_q", "設問を追加"),
      hr(),
      
      actionButton("save_survey", "アンケートを保存",
                   style = "color:white;background:#0072B2;width:100%;font-weight:bold;")
    ),
    
    mainPanel(
      tabsetPanel(
        tabPanel("設問一覧",        uiOutput("question_list_ui")),
        tabPanel("回答画面プレビュー", uiOutput("answer_preview_ui"))
      )
    )
  )
))
