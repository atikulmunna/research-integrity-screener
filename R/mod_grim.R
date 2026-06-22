# Module: GRIM (Core tier, manual entry). Independent of PDF upload (UX-03).

GRIM_PLACEHOLDER <- paste(
  "5.19, 28, 1, 7, 1, Extraversion",
  "3.45, 40, 1, 5, 1, Anxiety",
  sep = "\n"
)

#' GRIM module UI
#' @noRd
mod_grim_ui <- function(id) {
  ns <- NS(id)
  bslib::card(
    bslib::card_header("GRIM - granularity check  ", tier_badge("Core")),
    bslib::card_body(
      fillable = FALSE,  # form inputs flow naturally, not squeezed by flex-fill
      explainer_panel(
        "What is this test?",
        tags$p(
          "GRIM checks whether a reported mean is mathematically achievable. ",
          "A mean of integer responses must equal some whole-number total ",
          "divided by the sample size, so only certain decimals are possible ",
          "for a given n."
        ),
        tags$p(
          "A mean that no integer total can produce is GRIM-inconsistent. This ",
          "is usually a reporting or rounding slip, not evidence of misconduct. ",
          "The reported decimal places matter, so enter the mean exactly as ",
          "printed (e.g. 5.20, not 5.2)."
        )
      ),
      tags$p(
        class = "text-muted small",
        "One row per line: ", tags$code("mean, n, scale_min, scale_max[, items][, label]"),
        ". Up to 50 rows."
      ),
      textAreaInput(ns("entries"), label = NULL, value = GRIM_PLACEHOLDER,
                    rows = 6, width = "100%"),
      actionButton(ns("run"), "Run GRIM check", class = "btn-primary"),
      tags$hr(),
      uiOutput(ns("messages")),
      uiOutput(ns("summary")),
      tableOutput(ns("table"))
    )
  )
}

#' GRIM module server
#' @return reactive returning list(ok, errors, message, grim) for the report.
#' @noRd
mod_grim_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    analysis <- eventReactive(input$run, {
      p <- parse_grim_input(input$entries)
      if (is.null(p$data) || nrow(p$data) == 0) {
        return(list(ok = FALSE, errors = p$errors,
                    message = "No valid rows to check.", grim = NULL))
      }
      if (nrow(p$data) > 50) {
        return(list(ok = FALSE, errors = p$errors,
                    message = "GRIM is limited to 50 rows in v1. Please reduce the number of entries.",
                    grim = NULL))
      }
      list(ok = TRUE, errors = p$errors, message = NULL, grim = run_grim(p$data))
    })

    output$messages <- renderUI({
      a <- analysis()
      tags <- list()
      if (length(a$errors)) {
        tags <- c(tags, list(div(
          class = "alert alert-warning", role = "alert",
          tags$strong("Some rows were skipped:"),
          tags$ul(lapply(a$errors, tags$li))
        )))
      }
      if (!isTRUE(a$ok) && !is.null(a$message)) {
        tags <- c(tags, list(div(class = "alert alert-secondary", role = "alert", a$message)))
      }
      tagList(tags)
    })

    output$summary <- renderUI({
      a <- analysis()
      req(isTRUE(a$ok))
      div(class = "mb-2 fw-semibold", a$grim$summary)
    })

    output$table <- renderTable(
      {
        a <- analysis()
        req(isTRUE(a$ok))
        display_grim_table(a$grim$table)
      },
      striped = TRUE, spacing = "xs", width = "100%"
    )

    analysis
  })
}
