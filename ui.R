library(shiny)

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

        uiOutput(
          "analysis_ui"
        )

      )

    )

  )

)