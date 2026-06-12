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
