library(shiny)

shinyUI(
  fluidPage(

    titlePanel("アンケート回答"),

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

        uiOutput("question_ui"),

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