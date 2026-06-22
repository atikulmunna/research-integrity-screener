# z-curve analysis (Module 3, META-ANALYSIS tier) - FR-15..FR-18.
# Thin wrapper over the zcurve package. z-curve estimates the average power of a
# *body* of studies; it needs a literature-sized set (>= 20 significant tests),
# which single manuscripts rarely supply (FR-18 / DR-02).

#' "about 1 in N" phrasing for a rate in (0, 1].
#' @noRd
edr_freq <- function(rate) {
  if (is.na(rate) || rate <= 0) return("very few")
  sprintf("about 1 in %d", max(1L, round(1 / rate)))
}

#' Plain-language interpretation of EDR and ERR (FR-17).
#' @noRd
zcurve_interpretation <- function(edr, err) {
  paste0(
    sprintf(paste0("Expected Discovery Rate (EDR) ≈ %d%% (%s): this share of the ",
                   "tested hypotheses in this set appears to reflect true effects. "),
            round(100 * edr), edr_freq(edr)),
    sprintf(paste0("Expected Replication Rate (ERR) ≈ %d%%: a significant finding ",
                   "from this set would be expected to replicate roughly %d%% of the time."),
            round(100 * err), round(100 * err))
  )
}

#' Fit a z-curve to a set of p-values.
#'
#' @param p_values Numeric vector of p-values (the canonical statcheck set).
#' @param min_n Minimum significant tests required (FR-18).
#' @param bootstrap Bootstrap resamples for CIs (passed to zcurve).
#' @param seed Optional RNG seed for reproducibility.
#' @return list with ok, n, and (on success) fit, err, edr, interpretation;
#'   or ok = FALSE + message.
#' @noRd
run_zcurve <- function(p_values, min_n = 20, bootstrap = 1000, seed = NULL) {
  p <- p_values[is.finite(p_values) & p_values > 0 & p_values < 0.05]
  n <- length(p)

  if (n < min_n) {
    return(list(
      ok = FALSE, n = n,
      message = sprintf(
        paste0("z-curve needs at least %d significant tests to model a literature. ",
               "Only %d %s available, so this analysis is not applicable here - ",
               "z-curve is intended for meta-analyses and multi-study papers, not ",
               "single manuscripts."),
        min_n, n, if (n == 1) "is" else "are"
      )
    ))
  }

  z <- qnorm(1 - p / 2)  # two-tailed p -> z-score (FR-15)
  if (!is.null(seed)) set.seed(seed)

  fit <- tryCatch(
    suppressWarnings(zcurve::zcurve(z = z, bootstrap = bootstrap)),
    error = function(e) e
  )
  if (inherits(fit, "error")) {
    return(list(ok = FALSE, n = n,
                message = paste0("z-curve fitting failed: ", conditionMessage(fit))))
  }

  co <- summary(fit)$coefficients
  err <- co["ERR", ]; edr <- co["EDR", ]

  list(
    ok = TRUE, n = n, fit = fit,
    err = list(est = unname(err[1]), lo = unname(err[2]), hi = unname(err[3])),
    edr = list(est = unname(edr[1]), lo = unname(edr[2]), hi = unname(edr[3])),
    interpretation = zcurve_interpretation(unname(edr[1]), unname(err[1]))
  )
}
