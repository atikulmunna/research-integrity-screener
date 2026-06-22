# Module: z-curve (META-ANALYSIS tier). Consumes the canonical statcheck result
# (FR-10a). Generally disabled for single manuscripts (< 20 significant tests),
# with the scope caveat always visible (FR-18 / DR-02).

#' z-curve module UI
#' @noRd
mod_zcurve_ui <- function(id) {
  ns <- NS(id)
  bslib::card(
    bslib::card_header("z-curve — average power of a literature  ", tier_badge("Meta-analysis")),
    bslib::card_body(
      # FR-18 / DR-02: always-visible scope caveat.
      div(
        class = "alert alert-secondary", role = "alert",
        tags$strong("For literatures and meta-analyses. "),
        "z-curve estimates the average power across a body of studies and needs ",
        "at least 20 significant tests. A single manuscript usually does not ",
        "qualify, in which case this analysis is shown as not applicable."
      ),
      explainer_panel(
        "What is this test?",
        tags$p(
          "z-curve fits a model to the significant test statistics and estimates ",
          "two things: the Expected Discovery Rate (how many of the tested ",
          "hypotheses likely reflect true effects) and the Expected Replication ",
          "Rate (how often a significant finding would be expected to replicate)."
        ),
        tags$p(
          "These are properties of a whole literature, not of one study, which is ",
          "why z-curve belongs to the meta-analysis tier."
        )
      ),
      uiOutput(ns("summary")),
      shinycssloaders::withSpinner(plotOutput(ns("plot"), height = "300px"),
                                   proxy.height = "300px")
    )
  )
}

#' z-curve module server
#' @param statcheck_res reactive returning a run_statcheck() result.
#' @return reactive returning the run_zcurve() result.
#' @noRd
mod_zcurve_server <- function(id, statcheck_res) {
  moduleServer(id, function(input, output, session) {
    result <- reactive({
      sc <- statcheck_res()
      req(isTRUE(sc$ok))
      run_zcurve(sc$computed_p)
    })

    output$summary <- renderUI({
      r <- result()
      if (!isTRUE(r$ok)) {
        return(div(class = "alert alert-secondary", role = "alert", r$message))
      }
      pct <- function(x) sprintf("%d%%", round(100 * x))
      tagList(
        div(class = "fw-semibold mb-1", sprintf("Fitted on %d significant tests.", r$n)),
        tags$ul(
          tags$li(sprintf("EDR (Expected Discovery Rate): %s, 95%% CI [%s, %s]",
                          pct(r$edr$est), pct(r$edr$lo), pct(r$edr$hi))),
          tags$li(sprintf("ERR (Expected Replication Rate): %s, 95%% CI [%s, %s]",
                          pct(r$err$est), pct(r$err$lo), pct(r$err$hi)))
        ),
        div(class = "mt-2", r$interpretation)
      )
    })

    output$plot <- renderPlot({
      r <- result()
      req(isTRUE(r$ok))
      plot(r$fit)
    })

    result
  })
}
