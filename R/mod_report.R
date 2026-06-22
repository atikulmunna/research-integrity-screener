# Module: Unified Report (FR-31..FR-35). Reads the five module result reactives
# (any may be unresolved), builds the report, renders it, and exports it as a
# self-contained HTML file.

#' Resolve a reactive that may not be ready yet, returning NULL instead of
#' raising the silent req() error.
#' @noRd
resolve_or_null <- function(r) tryCatch(r(), error = function(e) NULL)

#' Report module UI
#' @noRd
mod_report_ui <- function(id) {
  ns <- NS(id)
  bslib::card(
    bslib::card_header("Unified Report"),
    bslib::card_body(
      uiOutput(ns("report")),
      tags$hr(),
      tags$p(class = "text-muted small fst-italic", DISCLAIMER),
      downloadButton(ns("download"), "Download report (HTML)", class = "btn-primary mt-2")
    )
  )
}

#' Report module server
#' @param extraction,statcheck_res,pcurve_res,zcurve_res,grim_res,sprite_res
#'   reactives from the other modules.
#' @return reactive returning the assembled report structure.
#' @noRd
mod_report_server <- function(id, extraction, statcheck_res, pcurve_res,
                              zcurve_res, grim_res, sprite_res) {
  moduleServer(id, function(input, output, session) {
    report <- reactive({
      build_report(
        resolve_or_null(statcheck_res), resolve_or_null(pcurve_res),
        resolve_or_null(zcurve_res), resolve_or_null(grim_res),
        resolve_or_null(sprite_res)
      )
    })

    file_name <- reactive({
      ext <- resolve_or_null(extraction)
      if (!is.null(ext)) ext$name else NULL
    })

    output$report <- renderUI({
      rep <- report()
      secs <- report_sections(rep)
      tagList(
        report_summary_card(rep),
        lapply(secs, function(s) {
          tags$details(
            class = "mb-2", open = NA,
            tags$summary(class = "fw-semibold", s$title),
            div(class = "mt-2", s$body)
          )
        })
      )
    })

    output$download <- downloadHandler(
      filename = function() {
        paste0("RIS_report_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".html")
      },
      content = function(file) {
        html <- build_report_html(report(), file_name())
        writeLines(c("<!DOCTYPE html>", as.character(html)), file)
      }
    )

    report
  })
}
