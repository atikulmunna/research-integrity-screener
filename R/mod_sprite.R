# Module: SPRITE (Core tier, manual entry). One entry at a time (per FR-26;
# SPRITE is run per row and is the most expensive Core check). Independent of
# upload (UX-03).

#' SPRITE module UI
#' @noRd
mod_sprite_ui <- function(id) {
  ns <- NS(id)
  bslib::card(
    bslib::card_header("SPRITE — distribution feasibility  ", tier_badge("Core")),
    bslib::card_body(
      fillable = FALSE,  # form inputs flow naturally, not squeezed by flex-fill
      explainer_panel(
        "What is this test?",
        tags$p(
          "SPRITE asks whether any set of whole-number responses could produce ",
          "the reported mean and standard deviation, given the sample size and ",
          "the scale's range. If at least one such set exists, it shows an example."
        ),
        tags$p(
          "If no set of responses can yield the reported mean and SD, the ",
          "combination is mathematically impossible. As with GRIM, the reported ",
          "decimal places matter — enter the mean and SD exactly as printed."
        )
      ),
      bslib::layout_columns(
        col_widths = c(4, 4, 4),
        textInput(ns("mean"), "Mean (M)", value = "4.00"),
        textInput(ns("sd"), "SD", value = "1.95"),
        numericInput(ns("n"), "Sample size (n)", value = 20, min = 2, max = 200, step = 1)
      ),
      bslib::layout_columns(
        col_widths = c(4, 4, 4),
        numericInput(ns("scale_min"), "Scale min", value = 1, step = 1),
        numericInput(ns("scale_max"), "Scale max", value = 7, step = 1),
        numericInput(ns("items"), "Items", value = 1, min = 1, step = 1)
      ),
      actionButton(ns("run"), "Run SPRITE check", class = "btn-primary"),
      tags$hr(),
      uiOutput(ns("status")),
      shinycssloaders::withSpinner(plotOutput(ns("plot"), height = "260px"),
                                   proxy.height = "260px")
    )
  )
}

#' SPRITE module server
#' @return reactive returning the `run_sprite()` result for the report.
#' @noRd
mod_sprite_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    result <- eventReactive(input$run, {
      run_sprite(
        mean = input$mean, sd = input$sd, n = input$n,
        scale_min = input$scale_min, scale_max = input$scale_max,
        items = input$items
      )
    })

    output$status <- renderUI({
      r <- result()
      cls <- switch(r$status,
                    feasible = "alert alert-success",
                    impossible = "alert alert-danger",
                    inconclusive = "alert alert-warning",
                    "alert alert-secondary")
      div(class = cls, role = "alert", r$message)
    })

    output$plot <- renderPlot({
      r <- result()
      req(!is.null(r$example))
      sprite_plot(r$example, r$scale_min, r$scale_max)
    })

    result
  })
}
