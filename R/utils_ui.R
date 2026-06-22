# Small shared UI/formatting helpers used across modules.

#' Reliability-tier badge (DR-01). Core = green, Exploratory = amber,
#' Meta-analysis = grey.
#' @noRd
tier_badge <- function(tier) {
  cls <- switch(
    tier,
    "Core" = "text-bg-success",
    "Exploratory" = "text-bg-warning",
    "Meta-analysis" = "text-bg-secondary",
    "text-bg-secondary"
  )
  span(class = paste("badge", cls), paste(tier, "tier"))
}

#' Collapsible "What is this test?" explainer (UX-05). Uses a native
#' <details> element so it needs no JS.
#' @noRd
explainer_panel <- function(summary_text, ...) {
  tags$details(
    class = "mb-3",
    tags$summary(class = "fw-semibold", summary_text),
    div(class = "mt-2 text-muted", ...)
  )
}

#' Format a p-value APA-style: leading zero stripped, "< .001" for tiny values.
#' Vectorised; NA renders as an em dash.
#' @noRd
fmt_p <- function(p) {
  out <- ifelse(p < .001, "< .001", sub("^0", "", sprintf("%.3f", p)))
  out[is.na(p)] <- "-"
  out
}
