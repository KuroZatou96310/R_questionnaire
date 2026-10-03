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

        h4("回答条件の絞り込み"),

        uiOutput("filter_ui"),

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
            h4(textOutput("crosstab_heatmap_heading", inline = TRUE)),
            plotOutput("crosstab_heatmap", height = "460px"),
            h4("100%積み上げ棒グラフ"),
            radioButtons(
              "cross_stack_basis",
              "割合の基準",
              choices = c("行％（列の内訳）" = "row", "列％（行の内訳）" = "column"),
              selected = "row",
              inline = TRUE
            ),
            plotOutput("crosstab_stacked_plot", height = "430px"),
            h4("統計的な差の検定"),
            tableOutput("crosstab_stats_table"),
            textOutput("crosstab_stats_note"),
            h4("期待度数"),
            tableOutput("crosstab_expected_table"),
            textOutput("crosstab_note")
          ),

          tabPanel(
            "数値同士の分析",
            br(),
            uiOutput("numeric_analysis_ui"),
            hr(),
            plotOutput("numeric_relation_plot", height = "420px"),
            h4("相関の要約"),
            tableOutput("numeric_relation_table"),
            h4("残差プロット"),
            plotOutput("numeric_residual_plot", height = "360px")
          )
        )

      )

    )

  )

)
