# Module: PDF upload + extraction confirmation (FR-01..FR-05, FR-04 stage 1).
# The server returns a reactive holding the extraction result so downstream
# modules (statcheck, p-curve, ...) can consume the extracted text.

#' Upload module UI
#' @noRd
mod_upload_ui <- function(id) {
  ns <- NS(id)
  tagList(
    fileInput(
      ns("pdf"),
      label = "Upload manuscript (PDF, max 20 MB)",
      accept = ".pdf",
      buttonLabel = "Browse…",
      placeholder = "No file selected"
    ),
    shinycssloaders::withSpinner(
      uiOutput(ns("status")),
      proxy.height = "80px"
    ),
    uiOutput(ns("applicability"))
  )
}

#' Build the success confirmation UI (FR-04 stage 1: page + word counts only,
#' no statistic count - that belongs to the statcheck scan stage).
#' @noRd
upload_success_ui <- function(res) {
  div(
    class = "alert alert-success",
    role = "alert",
    tags$strong("Extraction succeeded. "),
    sprintf(
      "%s %s, %s words detected.",
      format(res$n_pages, big.mark = ","),
      if (res$n_pages == 1) "page" else "pages",
      format(res$n_words, big.mark = ",")
    )
  )
}

#' Build the failure UI from an extraction result.
#' @noRd
upload_error_ui <- function(res) {
  div(class = "alert alert-danger", role = "alert", res$message)
}

#' Upload module server
#' @return A reactive returning the `pdf_result()` for the current upload.
#' @noRd
mod_upload_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    extraction <- reactive({
      req(input$pdf)
      res <- extract_pdf_text(input$pdf$datapath)
      res$name <- input$pdf$name  # filename only (no content) for the report (§9.7)
      res
    })

    output$status <- renderUI({
      res <- extraction()
      if (isTRUE(res$ok)) upload_success_ui(res) else upload_error_ui(res)
    })

    # UX-08: advisory scope banner (never gates anything).
    output$applicability <- renderUI({
      res <- extraction()
      req(isTRUE(res$ok))
      applicability_banner(applicability_check(res$text))
    })

    extraction
  })
}
