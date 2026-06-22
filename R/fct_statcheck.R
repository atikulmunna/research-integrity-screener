# statcheck analysis (Module 1, Core tier) — FR-06..FR-10, FR-10a.
# Pure logic, no Shiny, directly unit-testable.

#' Map statcheck's two logical flags to a severity label (FR-09, §9.1).
#' Decision errors dominate. NA-safe (treats NA as not-flagged).
#' @noRd
statcheck_severity <- function(error, decision_error) {
  ifelse(decision_error %in% TRUE, "Critical",
         ifelse(error %in% TRUE, "Concern", "Consistent"))
}

#' Render a single display df string from statcheck's df1/df2 per test type.
#' @noRd
format_df <- function(test_type, df1, df2) {
  mapply(
    function(tt, a, b) {
      switch(
        tt,
        "F"    = paste(a, b, sep = ", "),
        "Chi2" = as.character(a),
        "t"    = as.character(b),
        "r"    = as.character(b),
        "Z"    = "",
        if (!is.na(b)) as.character(b) else as.character(a)
      )
    },
    test_type, df1, df2,
    USE.NAMES = FALSE
  )
}

#' Empty tidy table with the canonical columns (used when nothing is found).
#' @noRd
empty_statcheck_table <- function() {
  data.frame(
    statistic = character(0), test_type = character(0), df = character(0),
    df1 = numeric(0), df2 = numeric(0),
    test_value = numeric(0), reported_p = numeric(0), computed_p = numeric(0),
    consistent = logical(0), decision_error = logical(0), severity = character(0),
    stringsAsFactors = FALSE
  )
}

#' Run statcheck on extracted text and return a tidy, screened result.
#'
#' @param text Character vector (one element per page) or a single string.
#' @return list with: ok, message, table, n_checked, n_inconsistent,
#'   n_decision_errors, severity_counts, computed_p (canonical p-values, FR-10a),
#'   reported_p.
#' @noRd
run_statcheck <- function(text) {
  combined <- paste(text, collapse = "\n")
  res <- tryCatch(statcheck::statcheck(combined, messages = FALSE),
                  error = function(e) e)

  if (inherits(res, "error") || is.null(res) || nrow(res) == 0) {
    return(list(
      ok = FALSE,
      message = paste0(
        "No APA-formatted statistics detected. This test requires ",
        "t, F, χ², z, or r statistics reported in APA format."
      ),
      table = empty_statcheck_table(),
      n_checked = 0L, n_inconsistent = 0L, n_decision_errors = 0L,
      severity_counts = c(Consistent = 0L, Concern = 0L, Critical = 0L),
      computed_p = numeric(0), reported_p = numeric(0)
    ))
  }

  severity <- statcheck_severity(res$error, res$decision_error)
  tbl <- data.frame(
    statistic = res$raw,
    test_type = res$test_type,
    df = format_df(res$test_type, res$df1, res$df2),
    df1 = res$df1,                 # retained for p-curve (noncentral computations)
    df2 = res$df2,
    test_value = res$test_value,
    reported_p = res$reported_p,
    computed_p = res$computed_p,
    consistent = !res$error,
    decision_error = res$decision_error,
    severity = severity,
    stringsAsFactors = FALSE
  )

  list(
    ok = TRUE,
    message = NULL,
    table = tbl,
    n_checked = nrow(tbl),
    n_inconsistent = sum(res$error %in% TRUE),
    n_decision_errors = sum(res$decision_error %in% TRUE),
    severity_counts = c(
      Consistent = sum(severity == "Consistent"),
      Concern    = sum(severity == "Concern"),
      Critical   = sum(severity == "Critical")
    ),
    computed_p = res$computed_p,  # canonical p-values for downstream p-curve (FR-10a)
    reported_p = res$reported_p
  )
}

#' Turn the tidy table into the display table of §9.2 (labels + emoji severity).
#' @noRd
display_statcheck_table <- function(tbl) {
  sev_label <- c(
    Consistent = "✅ Consistent",
    Concern    = "⚠️ Concern",
    Critical   = "\U0001f534 Critical"
  )
  data.frame(
    Statistic        = tbl$statistic,
    Type             = tbl$test_type,
    df               = tbl$df,
    `Test value`     = round(tbl$test_value, 2),
    `Reported p`     = fmt_p(tbl$reported_p),
    `Computed p`     = fmt_p(tbl$computed_p),
    Consistent       = ifelse(tbl$consistent, "Yes", "No"),
    `Decision error` = ifelse(tbl$decision_error %in% TRUE, "Yes", "No"),
    Severity         = unname(sev_label[tbl$severity]),
    check.names = FALSE, stringsAsFactors = FALSE
  )
}
