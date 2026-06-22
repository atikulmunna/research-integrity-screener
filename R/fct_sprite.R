# SPRITE test (Module 5, Core tier) — FR-26..FR-30. Uses rsprite2 (SPRITE was
# removed from scrutiny). Mean and SD are carried as STRINGS so their reported
# decimal precision drives the feasibility check (m_prec / sd_prec).
#
# Impossibility is proven deterministically (GRIMMER + SD-range check), never by
# search exhaustion (FR-29). The stochastic search only supplies an example
# distribution to plot (FR-28); failing to find one is "inconclusive" (FR-30).

#' Validate one SPRITE entry (FR-26 fields, §8.3 ranges). NA when valid.
#' @noRd
validate_sprite_row <- function(mean, sd, n, scale_min, scale_max, items) {
  m <- suppressWarnings(as.numeric(mean))
  s <- suppressWarnings(as.numeric(sd))
  if (is.na(m)) return("Mean is not numeric.")
  if (is.na(s) || s <= 0) return("SD must be a positive number.")
  if (is.na(scale_min) || is.na(scale_max) || scale_min >= scale_max) {
    return("Scale minimum must be less than scale maximum.")
  }
  if (is.na(n) || n != round(n) || n < 2 || n > 200) {
    return("Sample size must be a whole number between 2 and 200 (v1 limit).")
  }
  if (is.na(items) || items < 1 || items != round(items)) {
    return("Number of items must be a whole number ≥ 1.")
  }
  if (m < scale_min || m > scale_max) {
    return("Mean must be within the scale bounds.")
  }
  NA_character_
}

#' Run SPRITE for one (mean, sd, n, scale, items) entry.
#'
#' @param mean,sd Character strings (precision-preserving), e.g. "3.00", "1.50".
#' @return list with: ok, status ("invalid"|"impossible"|"feasible"|"inconclusive"),
#'   message, feasible (TRUE/FALSE/NA), n_found, capped, example (integer vector
#'   or NULL), and the echoed inputs.
#' @noRd
run_sprite <- function(mean, sd, n, scale_min, scale_max, items = 1,
                       max_distributions = 100, seed = 1) {
  base <- list(ok = TRUE, mean = mean, sd = sd, n = n,
               scale_min = scale_min, scale_max = scale_max, items = items)

  err <- validate_sprite_row(mean, sd, n, scale_min, scale_max, items)
  if (!is.na(err)) {
    return(c(modifyList(base, list(ok = FALSE)),
             list(status = "invalid", message = err, feasible = NA,
                  n_found = 0L, capped = FALSE, example = NULL)))
  }

  meanval <- as.numeric(mean); sdval <- as.numeric(sd)
  m_prec <- decimals_in(mean); sd_prec <- decimals_in(sd)

  # Deterministic granularity proof (FR-29). GRIMMER is mean+SD+n+precision;
  # scale bounds are NOT passed here (they make rsprite2's SD-range calc error
  # on GRIM-inconsistent means). Bound/SD-range impossibility is caught below by
  # set_parameters instead. Guarded so a GRIMMER error never aborts the run.
  grimmer <- tryCatch(
    suppressWarnings(rsprite2::GRIMMER_test(
      mean = meanval, sd = sdval, n_obs = n,
      m_prec = m_prec, sd_prec = sd_prec, n_items = items
    )),
    error = function(e) NA
  )
  impossible_msg <- paste0(
    "No valid response distribution exists that produces this mean and SD ",
    "with this sample size and scale. This combination is mathematically impossible."
  )
  if (isFALSE(grimmer)) {
    return(c(base, list(status = "impossible", feasible = FALSE, n_found = 0L,
                        capped = FALSE, example = NULL, message = impossible_msg)))
  }

  params <- tryCatch(
    rsprite2::set_parameters(
      mean = meanval, sd = sdval, n_obs = n, min_val = scale_min, max_val = scale_max,
      m_prec = m_prec, sd_prec = sd_prec, n_items = items
    ),
    error = function(e) e
  )
  if (inherits(params, "error")) {
    return(c(base, list(status = "impossible", feasible = FALSE, n_found = 0L,
                        capped = FALSE, example = NULL,
                        message = paste0(impossible_msg, " (", conditionMessage(params), ")"))))
  }

  dists <- tryCatch(
    rsprite2::find_possible_distributions(params, n_distributions = max_distributions, seed = seed),
    error = function(e) e
  )
  inconclusive_msg <- paste0(
    "No example distribution was found within the search limit. This is ",
    "inconclusive — not a proof of impossibility."
  )
  if (inherits(dists, "error") || is.null(dists) || nrow(dists) == 0) {
    return(c(base, list(status = "inconclusive", feasible = NA, n_found = 0L,
                        capped = FALSE, example = NULL, message = inconclusive_msg)))
  }

  successes <- dists[dists$outcome == "success", , drop = FALSE]
  n_found <- nrow(successes)
  if (n_found == 0) {
    return(c(base, list(status = "inconclusive", feasible = NA, n_found = 0L,
                        capped = FALSE, example = NULL, message = inconclusive_msg)))
  }

  capped <- n_found >= max_distributions
  c(base, list(
    status = "feasible", feasible = TRUE, n_found = n_found, capped = capped,
    example = successes$distribution[[1]],
    message = sprintf("Feasible. %s%d example distribution%s found.",
                      if (capped) "At least " else "", n_found,
                      if (n_found == 1) "" else "s")
  ))
}

#' Bar plot of one example feasible distribution (FR-28).
#' @noRd
sprite_plot <- function(example, scale_min, scale_max) {
  lv <- scale_min:scale_max
  counts <- as.data.frame(table(factor(example, levels = lv)))
  names(counts) <- c("value", "count")
  ggplot2::ggplot(counts, ggplot2::aes(x = .data$value, y = .data$count)) +
    ggplot2::geom_col(fill = "#4C78A8") +
    ggplot2::labs(x = "Response value", y = "Count",
                  title = "One feasible response distribution") +
    ggplot2::theme_minimal()
}
