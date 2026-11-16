library(shiny)
library(bslib)

shinyUI(page_fillable(
  title = "Access",
  theme = bs_theme(
    version = 5,
    bootswatch = "minty",
    primary = "#287565",
    bg = "#f3f6fb",
    fg = "#20243A"
  ),
  div(class = "row min-vh-100 align-items-center justify-content-center",
    column(width = 12, class = "col-md-8 col-lg-6 col-xl-4",
      card(class = "shadow-sm",
        card_body(
          h4(textOutput("survey_id", inline = TRUE), class = "text-center mb-4"),
          div(class = "d-flex justify-content-center", uiOutput("qr_code")),
          div(class = "d-grid mt-3",
            downloadButton("download_qr", "画像をダウンロード", class = "btn-primary")
          )
        )
      )
    )
  )
))
