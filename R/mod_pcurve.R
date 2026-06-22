# Module: p-curve (EXPLORATORY tier). Consumes the canonical statcheck result
# (FR-10a) - it uses the parsed test statistics, which the noncentral method
# requires. The focal-test caveat (FR-11a) is shown inline and always visible.

#' p-curve module UI
#' @noRd
mod_pcurve_ui <- function(id) {
  ns <- NS(id)
  bslib::card(
    bslib::card_header("p-curve - evidential value (screening)  ", tier_badge("Exploratory")),
    bslib::card_body(
      # FR-11a: mandatory, always-visible caveat (not behind the explainer).
      div(
        class = "alert alert-warning", role = "alert",
        tags$strong("Screening heuristic - not a methodological p-curve. "),
        "These p-values are auto-extracted from every detected test, not the ",
        "manually chosen focal tests a valid p-curve requires. Read the result ",
        "as a prompt for closer manual review, not as a finding."
      ),
      explainer_panel(
        "What is this test?",
        tags$p(
          "p-curve looks at the shape of the significant p-values. A right-skewed ",
          "curve (many very small p-values) is what you expect when real effects ",
          "are present; a flat or left-leaning curve (values bunched near .05) is ",
          "what you expect when there is little underlying signal."
        ),
        tags$p(
          "Because RIS cannot separate focal tests from incidental ones, this is ",
          "only a rough screen of the manuscript's significant statistics."
        )
      ),
      uiOutput(ns("summary")),
      shinycssloaders::withSpinner(plotOutput(ns("plot"), height = "260px"),
                                   proxy.height = "260px")
    )
  )
}

#' p-curve module server
#' @param statcheck_res reactive returning a run_statcheck() result.
#' @return reactive returning the run_pcurve() result.
#' @noRd
mod_pcurve_server <- function(id, statcheck_res) {
  moduleServer(id, function(input, output, session) {
    result <- reactive({
      sc <- statcheck_res()
      req(isTRUE(sc$ok))
      run_pcurve(pcurve_inputs_from_statcheck(sc))
    })

    output$summary <- renderUI({
      r <- result()
      if (!isTRUE(r$ok)) {
        return(div(class = "alert alert-secondary", role = "alert", r$message))
      }
      fp <- function(p) if (is.na(p)) "-" else fmt_p(p)
      pwr <- if (is.na(r$power)) "-" else paste0(round(100 * r$power), "%")
      tagList(
        div(class = "fw-semibold mb-1",
            sprintf("Based on %d significant test%s.", r$k, if (r$k == 1) "" else "s")),
        tags$ul(
          tags$li(sprintf("Right-skew test (evidential value): Z = %.2f, p = %s",
                          r$rightskew$z_full, fp(r$rightskew$p_full))),
          tags$li(sprintf("Flatness test (vs 33%% power): Z = %.2f, p = %s",
                          r$flatness$z_full, fp(r$flatness$p_full))),
          tags$li(sprintf("Estimated average power: %s", pwr))
        ),
        div(class = "mt-2", r$interpretation)
      )
    })

    output$plot <- renderPlot({
      r <- result()
      req(isTRUE(r$ok), length(r$p_values) > 0)
      pcurve_plot(r$p_values)
    })

    result
  })
}
