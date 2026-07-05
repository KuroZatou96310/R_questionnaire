library(shiny)
library(bslib)
shinyUI(
  page_fluid(

    uiOutput("question_ui")
,
    sidebarLayout(

      sidebarPanel(

        textInput(
          "survey_id",
          "アンケートID"
        ),

        actionButton(
          "load",
          "読み込み"
        )

      ),

      mainPanel(


        br(),

        actionButton(
          "submit",
          "回答を送信",
          style="color:white;background:#28a745;"
        )

      )

    )
  )
)