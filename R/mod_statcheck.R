# Module: statcheck (Core tier). Consumes the upload module's extraction
# reactive, runs statcheck, and returns its result reactive for the report.

#' statcheck module UI
#' @noRd
mod_statcheck_ui <- function(id) {
  ns <- NS(id)
  bslib::card(
    bslib::card_header(
      "statcheck - APA statistic consistency  ",
      tier_badge("Core")
    ),
    bslib::card_body(
      explainer_panel(
        "What is this test?",
        tags$p(
          "statcheck recomputes the p-value from each reported test statistic ",
          "(t, F, χ², z, r) and its degrees of freedom, then compares it with ",
          "the p-value printed in the paper."
        ),
        tags$p(
          "An inconsistency means the reported and recomputed values do not ",
          "line up. A “decision error” is a mismatch that crosses the .05 ",
          "significance threshold. These are not evidence of misconduct - they ",
          "are often simple typos or transcription slips."
        )
      ),
      uiOutput(ns("summary")),
      shinycssloaders::withSpinner(tableOutput(ns("table")), proxy.height = "120px")
    )
  )
}

#' statcheck module server
#' @param extraction A reactive returning a `pdf_result()` from mod_upload.
#' @return A reactive returning the `run_statcheck()` result.
#' @noRd
mod_statcheck_server <- function(id, extraction) {
  moduleServer(id, function(input, output, session) {
    result <- reactive({
      ext <- extraction()
      req(isTRUE(ext$ok))
      run_statcheck(ext$text)
    })

    output$summary <- renderUI({
      r <- result()
      if (!isTRUE(r$ok)) {
        return(div(class = "alert alert-secondary", role = "alert", r$message))
      }
      # FR-04 stage 2: report how many APA statistics were detected/checked.
      div(
        class = "mb-2",
        sprintf(
          "%d APA-formatted statistic%s checked - %d inconsistent (%d decision error%s).",
          r$n_checked, if (r$n_checked == 1) "" else "s",
          r$n_inconsistent,
          r$n_decision_errors, if (r$n_decision_errors == 1) "" else "s"
        )
      )
    })

    output$table <- renderTable(
      {
        r <- result()
        req(isTRUE(r$ok))
        display_statcheck_table(r$table)
      },
      striped = TRUE, spacing = "xs", width = "100%", na = "-"
    )

    result
  })
}
