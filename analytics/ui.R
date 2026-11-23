library(shiny)

# Shiny Server の既定ロケールが C の場合でも、日本語をUTF-8として表示する
try(Sys.setlocale("LC_CTYPE", "C.UTF-8"), silent = TRUE)

shinyUI(

  fluidPage(

    tags$head(
      tags$title("アンケート分析画面"),
    ),

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

        downloadButton("download_csv_bundle", "CSVを一括ダウンロード"),
        tags$div(
          style = "display:none;",
          downloadButton("download_csv", "回答データCSV"),
          downloadButton("download_questions_csv", "設問・設定CSV")
        ),

        hr(),

        uiOutput("access_link_ui")

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
            textOutput("crosstab_note"),
            hr(),
            h4(textOutput("crosstab_heading")),

            radioButtons(
              "cross_display",
              "表示方法",
              choices = c(
                "件数" = "count",
                "構成比（％）" = "percent"
              ),
              selected = "count",
              inline = TRUE
            ),

            tableOutput("crosstab_table"),
            br(),
            h4(textOutput("crosstab_heatmap_heading", inline = TRUE)),
            plotOutput("crosstab_heatmap", height = "460px"),
            hr(),
            h4("100%積み上げ棒グラフ"),

            radioButtons(
              "cross_stack_basis",
              "割合の基準",
              choices = c("行％（列の内訳）" = "row", "列％（行の内訳）" = "column"),
              selected = "row",
              inline = TRUE
            ),
            
            plotOutput("crosstab_stacked_plot", height = "430px"),
            hr(),
            h4("統計的な差の検定"),
            tableOutput("crosstab_stats_table"),
            tags$div(
              style = "margin:12px 0;padding:10px 14px;border:1px solid #cbd5e1;border-radius:6px;color:#334155;",
              tags$strong("クロス集計の見方"),
              tags$h5("表とグラフ", style = "margin:12px 0 6px;"),
              tags$ul(
                tags$li(tags$b("クロス集計表："), "行と列の回答の組み合わせごとの人数です。「構成比(%)」を選ぶと、全体に対する割合で表示されます。"),
                tags$li(tags$b("ヒートマップ："), "色が濃いほど、人数の多い組み合わせです。ただし、人数の多い選択肢は、関連がなくても濃く見えます。"),
                tags$li(tags$b("100%積み上げ棒グラフ："), "行（または列）ごとに回答の内訳を割合で比べます。内訳が棒によって大きく違うほど、2つの設問に関連がある可能性があります。偏りを見るときは、こちらも確認してください。")
              ),
              tags$h5("統計的な差の検定", style = "margin:12px 0 6px;"),
              tags$ul(
                tags$li("設問同士の回答の偏りが、偶然だけで起きた可能性や、関連の強さを確認します。"),
                tags$li(tags$b("期待度数が5未満のセル："), "検定が信頼できるかの目安です。20%を超えると、p値とVは参考値になります。選択肢をまとめると改善します。"),
                tags$li(tags$b("p値（カイ二乗検定）："), "小さいほど、回答の偏りが偶然とは考えにくいことを示します（0.05未満が目安）。0.05以上なら、関連があるとは言い切れません。"),
                tags$li(tags$b("クラメールのV："), "関連の強さを0〜1で表します。人数が少ない、または選択肢が多いと大きく出やすいため、p値とあわせて参考程度にしてください。"),
                tags$li(tags$b("カイ二乗値・自由度："), "p値とクラメールのVを計算するための値です。論文での報告や結果の確認に使えます。読み方は気にしなくて構いません。")
              )
            ),

          ),

          tabPanel(
            "数値分析",
            br(),
            uiOutput("numeric_analysis_ui"),
            hr(),
            plotOutput("numeric_relation_plot", height = "420px"),
            h4("相関の要約"),
            tableOutput("numeric_relation_table"),
            tags$div(
              style = "margin:12px 0;padding:10px 14px;border:1px solid #cbd5e1;border-radius:6px;color:#334155;",
              tags$strong("指標の見方（統計初心者向け）"),
              tags$ul(
                tags$li(tags$b("有効回答数 n："), "2つの設問の両方に数値で答えた人の数です。少ないほど、結果は偶然に左右されやすくなります。"),
                tags$li(tags$b("相関係数（Spearman ρ）："), "2つの値の「順位」がどれだけ連動しているかを表します（−1〜+1）。+1に近いほど「一方が高い人は他方も高い」、−1に近いほど「一方が高い人は他方が低い」関係で、0に近いと連動は見られません。"),
                tags$li(tags$b("p値："), "「本当は関係がない」と仮定したとき、今回と同じかそれ以上の関係が出る確率です。小さいほど「偶然では起こりにくい」と言え、0.05未満が一般的な目安です。関係の強さや重要さを表すものではありません。"),
                tags$li(tags$b("相関の95%信頼区間："), "今回のデータから推定した、本当の相関係数の範囲です。幅が広いほど推定が不確かで、人数が少ないと広くなります。範囲が0をまたぐ場合は、関係があるとは言い切れません。"),
                tags$li(tags$b("回帰の傾き："), "横軸の値が1大きい人は、縦軸の値が平均してこれだけ大きい（小さい）という傾向を表します。原因と結果を示すものではありません。"),
                tags$li(tags$b("外れ値："), "全体の傾向から大きく離れた回答の件数です。回帰モデルの標準化残差の絶対値が2を超えた回答を外れ値候補として数えています（概ね平均から2標準偏差以上離れた目安です）。")
              )
            )
          ),

        )

      )

    )

  )

)
