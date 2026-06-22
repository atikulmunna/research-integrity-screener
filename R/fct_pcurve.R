# p-curve analysis (Module 2, EXPLORATORY tier) — FR-11..FR-14.
# Custom implementation of Simonsohn, Nelson & Simons (2014) + the full/half
# combination of Simonsohn (2015), since there is no maintained CRAN package
# (§11.2). This is a screening heuristic, NOT a methodological p-curve: it runs
# on auto-extracted tests, not hand-selected focal tests (FR-11a).
#
# Every test family is reduced to a noncentral F or chi-square, which absorbs
# the two-/one-tailed handling:
#   t(df)  -> F(1, df),  x = t^2
#   r(df)  -> t = r*sqrt(df/(1-r^2)) -> F(1, df), x = t^2
#   F      -> F(df1, df2)
#   chi^2  -> chi^2(df)
#   z      -> chi^2(1), x = z^2

# --- noncentral engines -------------------------------------------------------

#' Survival function P(stat >= x | ncp) for the F or chi-square family.
#' @noRd
stat_sf <- function(family, x, df1, df2, ncp) {
  if (family == "F") {
    pf(x, df1, df2, ncp = ncp, lower.tail = FALSE)
  } else {
    pchisq(x, df1, ncp = ncp, lower.tail = FALSE)
  }
}

#' Critical statistic value at significance level alpha.
#' @noRd
stat_crit <- function(family, alpha, df1, df2) {
  if (family == "F") qf(1 - alpha, df1, df2) else qchisq(1 - alpha, df1)
}

#' Noncentrality parameter giving `power` at the .05 level (power .05 -> ncp 0).
#' @noRd
ncp_for_power <- function(family, power, df1, df2, crit05) {
  if (power <= 0.05 + 1e-9) return(0)
  hi <- 1
  while (stat_sf(family, crit05, df1, df2, hi) < power && hi < 1e6) hi <- hi * 2
  tryCatch(
    uniroot(function(ncp) stat_sf(family, crit05, df1, df2, ncp) - power, c(0, hi))$root,
    error = function(e) NA_real_
  )
}

#' pp-value: probability of a result at least as extreme as observed, given the
#' result is significant at `alpha`, under a world with the given `power`.
#' power = .05 -> the null (flat) pp = p/.05; power = 1/3 -> the flatness pp.
#' Clamped to (1e-8, 1-1e-8) so qnorm() stays finite.
#' @noRd
pp_at_power <- function(power, family, x, df1, df2, alpha = 0.05) {
  crit05 <- stat_crit(family, 0.05, df1, df2)
  ncp <- ncp_for_power(family, power, df1, df2, crit05)
  if (is.na(ncp)) return(NA_real_)
  crit <- stat_crit(family, alpha, df1, df2)
  num <- stat_sf(family, x, df1, df2, ncp)
  den <- stat_sf(family, crit, df1, df2, ncp)
  val <- if (den <= 0) NA_real_ else num / den
  if (is.na(val)) return(NA_real_)
  min(max(val, 1e-8), 1 - 1e-8)
}

# --- helpers ------------------------------------------------------------------

#' Stouffer Z over a vector of pp-values.
#' @noRd
stouffer_z <- function(pp) {
  pp <- pp[is.finite(pp)]
  if (length(pp) == 0) return(NA_real_)
  sum(qnorm(pp)) / sqrt(length(pp))
}

#' Estimate average power: the power level at which the (full) p-curve's
#' Stouffer Z crosses zero.
#' @noRd
estimate_power <- function(tests, lo = 0.051, hi = 0.99) {
  zfun <- function(power) {
    pp <- vapply(seq_len(nrow(tests)), function(i) {
      pp_at_power(power, tests$family[i], tests$x[i], tests$df1[i], tests$df2[i])
    }, numeric(1))
    stouffer_z(pp)
  }
  zlo <- zfun(lo); zhi <- zfun(hi)
  if (is.na(zlo) || is.na(zhi)) return(NA_real_)
  if (zlo > 0) return(lo)   # flatter than even 5% power
  if (zhi < 0) return(hi)   # stronger than 99% power
  tryCatch(uniroot(zfun, c(lo, hi))$root, error = function(e) NA_real_)
}

#' Evidential-value decision rule (Simonsohn 2015), applied to the four p-values.
#' @noRd
pcurve_decision <- function(p_full_skew, p_half_skew, p_full_flat, p_half_flat) {
  lt <- function(a, b) isTRUE(a < b)
  present <- lt(p_half_skew, 0.05) || (lt(p_full_skew, 0.10) && lt(p_half_skew, 0.10))
  absent  <- lt(p_full_flat, 0.05) || (lt(p_full_flat, 0.10) && lt(p_half_flat, 0.10))
  if (present) "present" else if (absent) "absent" else "inconclusive"
}

#' One-line plain interpretation (the heuristic caveat lives in the UI, FR-11a).
#' @noRd
pcurve_interpretation <- function(ev, power) {
  pwr <- if (is.na(power)) "undetermined" else paste0(round(100 * power), "%")
  base <- switch(
    ev,
    present = "The p-curve leans right-skewed, consistent with the set containing some evidential value.",
    absent = "The p-curve is flat or left-skewed, consistent with little evidential value (e.g. selective reporting or low power).",
    "The p-curve is inconclusive — it neither clearly indicates nor clearly rules out evidential value."
  )
  paste0(base, " Estimated average power: ", pwr, ".")
}

# --- main ---------------------------------------------------------------------

#' Run a p-curve analysis over a set of significant tests.
#'
#' @param tests data.frame with columns family ("F"/"chisq"), x, df1, df2, p.
#' @param min_tests Minimum significant tests required (FR-14).
#' @return list with ok, k, p_values, rightskew, flatness, power,
#'   evidential_value, interpretation (or ok = FALSE + message).
#' @noRd
run_pcurve <- function(tests, min_tests = 5) {
  tests <- tests[is.finite(tests$p) & tests$p < 0.05, , drop = FALSE]
  k <- nrow(tests)

  if (k < min_tests) {
    return(list(
      ok = FALSE, k = k, p_values = tests$p,
      message = sprintf(
        paste0("p-curve requires at least %d focal hypothesis tests. Only %d ",
               "significant p-value%s available; results with fewer values are unreliable."),
        min_tests, k, if (k == 1) " is" else "s are"
      )
    ))
  }

  ppf_h0 <- ppf_33 <- numeric(k)
  pph_h0 <- pph_33 <- rep(NA_real_, k)
  for (i in seq_len(k)) {
    fam <- tests$family[i]; x <- tests$x[i]; d1 <- tests$df1[i]; d2 <- tests$df2[i]
    ppf_h0[i] <- pp_at_power(0.05, fam, x, d1, d2, alpha = 0.05)
    ppf_33[i] <- pp_at_power(1 / 3, fam, x, d1, d2, alpha = 0.05)
    if (tests$p[i] < 0.025) {
      pph_h0[i] <- pp_at_power(0.05, fam, x, d1, d2, alpha = 0.025)
      pph_33[i] <- pp_at_power(1 / 3, fam, x, d1, d2, alpha = 0.025)
    }
  }

  zf_skew <- stouffer_z(ppf_h0); pf_skew <- pnorm(zf_skew)
  zf_flat <- stouffer_z(ppf_33); pf_flat <- pnorm(zf_flat, lower.tail = FALSE)
  zh_skew <- stouffer_z(pph_h0); ph_skew <- pnorm(zh_skew)
  zh_flat <- stouffer_z(pph_33); ph_flat <- pnorm(zh_flat, lower.tail = FALSE)

  power <- estimate_power(tests)
  ev <- pcurve_decision(pf_skew, ph_skew, pf_flat, ph_flat)

  list(
    ok = TRUE, k = k, p_values = tests$p,
    rightskew = list(z_full = zf_skew, p_full = pf_skew, z_half = zh_skew, p_half = ph_skew),
    flatness  = list(z_full = zf_flat, p_full = pf_flat, z_half = zh_flat, p_half = ph_flat),
    power = power,
    evidential_value = ev,
    interpretation = pcurve_interpretation(ev, power)
  )
}

#' Build p-curve inputs from a run_statcheck() result (FR-10a canonical set).
#' @noRd
pcurve_inputs_from_statcheck <- function(sc) {
  empty <- data.frame(family = character(0), x = numeric(0),
                      df1 = numeric(0), df2 = numeric(0), p = numeric(0))
  tbl <- sc$table
  if (is.null(tbl) || nrow(tbl) == 0) return(empty)

  rows <- list()
  for (i in seq_len(nrow(tbl))) {
    tt <- tbl$test_type[i]; v <- tbl$test_value[i]
    d1 <- tbl$df1[i]; d2 <- tbl$df2[i]; p <- tbl$computed_p[i]
    fam <- NA_character_; x <- NA_real_; D1 <- NA_real_; D2 <- NA_real_

    if (tt == "t") {
      fam <- "F"; x <- v^2; D1 <- 1; D2 <- d2
    } else if (tt == "F") {
      fam <- "F"; x <- v; D1 <- d1; D2 <- d2
    } else if (tt == "r" && is.finite(v) && abs(v) < 1) {
      tval <- v * sqrt(d2 / (1 - v^2)); fam <- "F"; x <- tval^2; D1 <- 1; D2 <- d2
    } else if (tt %in% c("Chi2", "chi2")) {
      fam <- "chisq"; x <- v; D1 <- d1
    } else if (tt %in% c("Z", "z")) {
      fam <- "chisq"; x <- v^2; D1 <- 1
    } else {
      next
    }
    rows[[length(rows) + 1]] <- data.frame(
      family = fam, x = x, df1 = D1, df2 = D2, p = p, stringsAsFactors = FALSE
    )
  }
  if (!length(rows)) return(empty)
  do.call(rbind, rows)
}

#' Histogram of significant p-values, binned at .01 (FR-13, §9.3).
#' @noRd
pcurve_plot <- function(pvals) {
  brks <- seq(0, 0.05, by = 0.01)
  labs <- c(".00–.01", ".01–.02", ".02–.03", ".03–.04", ".04–.05")
  bins <- cut(pvals, breaks = brks, include.lowest = TRUE, labels = labs)
  df <- as.data.frame(table(bins)); names(df) <- c("bin", "count")
  ggplot2::ggplot(df, ggplot2::aes(x = .data$bin, y = .data$count)) +
    ggplot2::geom_col(fill = "#E45756") +
    ggplot2::labs(x = "p-value", y = "Count",
                  title = "Distribution of significant p-values") +
    ggplot2::theme_minimal()
}
