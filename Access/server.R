library(shiny)
library(qrcode)
source("../common/db.R")

shinyServer(function(input, output, session) {
  access_data <- reactive({
    query <- parseQueryString(session$clientData$url_search)
    survey_id <- query[["id"]]
    req(!is.null(survey_id), nzchar(survey_id))
    survey_id <- load_survey_info(survey_id)$id
    req(nzchar(survey_id))

    protocol <- sub(":$", "", session$clientData$url_protocol)
    if (!nzchar(protocol)) protocol <- "http"
    hostname <- session$clientData$url_hostname
    port <- as.character(session$clientData$url_port)
    default_port <- (protocol == "http" && port == "80") || (protocol == "https" && port == "443")
    origin <- paste0(protocol, "://", hostname, if (nzchar(port) && !default_port) paste0(":", port) else "")

    access_path <- session$clientData$url_pathname
    answer_path <- sub("/access/?$", "/answer/", access_path)
    if (identical(answer_path, access_path)) {
      prefix <- sub("/[^/]*$", "", access_path)
      answer_path <- paste0(prefix, "/answer/")
    }
    if (!startsWith(answer_path, "/")) answer_path <- paste0("/", answer_path)

    list(
      id = survey_id,
      url = paste0(origin, answer_path, "?id=", utils::URLencode(survey_id, reserved = TRUE))
    )
  })

  output$survey_id <- renderText({
    paste0("ID: ", access_data()$id)
  })

  output$qr_code <- renderUI({
    info <- access_data()
    svg_file <- tempfile(fileext = ".svg")
    on.exit(unlink(svg_file), add = TRUE)
    generate_svg(qr_code(info$url), filename = svg_file, size = 360, show = FALSE)
    svg <- readLines(svg_file, warn = FALSE)
    svg[[2]] <- sub(' height="360" width="360"', ' viewBox="0 0 360 360" width="100%" height="auto"', svg[[2]], fixed = TRUE)
    HTML(paste(svg[-1L], collapse = "\n"))
  })

  output$download_qr <- downloadHandler(
    filename = function() {
      id <- gsub("[^A-Za-z0-9._-]", "_", access_data()$id)
      paste0("QR_", id, ".svg")
    },
    content = function(file) {
      info <- access_data()
      generate_svg(qr_code(info$url), filename = file, size = 360, show = FALSE)
    },
    contentType = "image/svg+xml"
  )
})

