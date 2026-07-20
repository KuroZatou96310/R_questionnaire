library(shiny)

# Shiny Server の既定ロケールが C の場合でも、日本語をUTF-8として表示する
try(Sys.setlocale("LC_CTYPE", "C.UTF-8"), silent = TRUE)

shinyUI(

  fluidPage(

    titlePanel("アンケート分析"),

    sidebarLayout(

      sidebarPanel(

        h4("アンケート読込"),

        textInput(
          "survey_id",
          "アンケートID"
        ),

        passwordInput(
          "survey_pw",
          "パスワード"
        ),

        actionButton(
          "load",
          "読み込み",
          style = "width:100%;"
        ),

        hr(),

        h4("回答数"),

        textOutput(
          "response_count"
        ),

        hr(),

        downloadButton(
          "download_csv",
          "CSVダウンロード"
        )

      ),

      mainPanel(

        tabsetPanel(
          id = "analysis_tab",

          tabPanel(
            "単純集計",
            uiOutput("analysis_ui")
          ),

          tabPanel(
            "クロス集計",
            br(),
            uiOutput("crosstab_ui"),
            hr(),
            h4(textOutput("crosstab_heading", inline = TRUE)),
            tableOutput("crosstab_table"),
            br(),
            textOutput("crosstab_note")
          ),

          tabPanel(
            "数値同士の分析",
            br(),
            uiOutput("numeric_analysis_ui"),
            hr(),
            plotOutput("numeric_relation_plot", height = "420px"),
            h4("相関の要約"),
            tableOutput("numeric_relation_table")
          )
        )

      )

    )

  )

)
