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
        choices = c(
          "選択してください" = "",
          "SUS：システムの使いやすさ（10項目）" = "SUS",
          "UMUX-Lite：有用性・使いやすさ（2項目）" = "UMUX-Lite",
          "UEQ：体験品質の総合評価（26項目）" = "UEQ",
          "UEQ-S：体験品質の短縮版（8項目）" = "UEQ-S",
          "NASA-TLX：作業負荷の評価（6項目）" = "NASA-TLX",
          "TAM：技術受容性の評価（6項目）" = "TAM",
          "GEQ：ゲーム・体験の評価（5項目）" = "GEQ",
          "IMI：活動への動機・興味の評価（7項目）" = "IMI"
        )
      ),
      uiOutput("preset_description"),
      actionButton("add_preset", "プリセットを設問に追加"),
      helpText("設問タイトルにはプリセット名を付けず、各設問の説明に回答尺度の意味を表示します。"),
      hr(),

      h4("質問順序のランダム化"),
      checkboxInput(
        "enable_random",
        "質問順序をランダム化する",
        value = FALSE
      ),
      conditionalPanel(
        condition = "input.enable_random === true",
        numericInput(
          "random_start",
          "何番目以降をランダム化するか",
          value = 1,
          min = 1,
          step = 1
        ),
        helpText("この番号より前は固定し、指定位置以降だけを回答者ごとに並べ替えます。")
      ),
      helpText("不要な場合はチェックを外したまま保存してください。"),
      hr(),
      
      h4("設問を追加"),
      textInput("q_title", "設問タイトル"),
      textAreaInput("q_desc", "設問説明"),
      selectInput("q_type", "設問タイプ",
                  choices = c("single", "multiple", "numeric", "text",
                              "select", "slider", "date", "info")),
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
