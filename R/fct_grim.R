# GRIM test (Module 4, Core tier) — FR-19..FR-25.
# Pure logic, no Shiny. The mean is carried as a STRING throughout because
# GRIM consistency depends on the reported decimal precision (FR-22a):
# scrutiny::grim("5.20", 28) is FALSE but grim("5.2", 28) is TRUE.

#' Number of decimal places in a numeric string ("5.19" -> 2, "5" -> 0).
#' @noRd
decimals_in <- function(s) {
  s <- as.character(s)
  ifelse(grepl("\\.", s), nchar(sub("^[^.]*\\.", "", s)), 0L)
}

#' Validate one GRIM entry (FR-21). Returns NA when valid, else a message.
#' @noRd
validate_grim_row <- function(mean, n, scale_min, scale_max, items) {
  m <- suppressWarnings(as.numeric(mean))
  if (is.na(m)) return("Mean is not numeric.")
  if (is.na(scale_min) || is.na(scale_max) || scale_min >= scale_max) {
    return("Scale minimum must be less than scale maximum.")
  }
  if (is.na(n) || n <= 0 || n != round(n) || n > 100000) {
    return("Sample size must be a whole number between 1 and 100,000.")
  }
  if (is.na(items) || items < 1 || items != round(items)) {
    return("Number of items must be a whole number ≥ 1.")
  }
  if (m < scale_min || m > scale_max) {
    return("Mean must be within the scale bounds.")
  }
  NA_character_
}

#' Run GRIM over a data frame of entries.
#'
#' @param entries data.frame with columns mean (character), n, scale_min,
#'   scale_max, items, label.
#' @return list with table, counts, and the FR-24 summary string.
#' @noRd
run_grim <- function(entries) {
  n <- nrow(entries)
  result <- character(n); valid <- logical(n)
  low_power <- logical(n); note <- character(n)

  for (i in seq_len(n)) {
    e <- entries[i, ]
    err <- validate_grim_row(e$mean, e$n, e$scale_min, e$scale_max, e$items)
    if (!is.na(err)) {
      result[i] <- "Invalid"; valid[i] <- FALSE; note[i] <- err
      next
    }
    valid[i] <- TRUE
    cons <- isTRUE(scrutiny::grim(x = e$mean, n = e$n, items = e$items))
    meanval <- as.numeric(e$mean)
    # A mean sitting exactly on a scale bound is trivially consistent and
    # uninformative (FR-23 boundary case).
    boundary <- isTRUE(all.equal(meanval, e$scale_min)) ||
      isTRUE(all.equal(meanval, e$scale_max))
    result[i] <- if (boundary) "Boundary" else if (cons) "Consistent" else "Inconsistent"

    # FR-24a diagnostic-power notice: once N reaches the reporting granularity
    # almost every mean is consistent, so consistency is weak evidence.
    d <- decimals_in(e$mean)
    if ((e$n * e$items) >= 10^d) {
      low_power[i] <- TRUE
      note[i] <- "GRIM has little discriminating power at this sample size; a 'consistent' result is weak evidence."
    }
  }

  tbl <- data.frame(
    label = entries$label, mean = entries$mean, n = entries$n,
    scale_min = entries$scale_min, scale_max = entries$scale_max, items = entries$items,
    result = result, valid = valid, low_power = low_power, note = note,
    stringsAsFactors = FALSE
  )

  n_valid <- sum(valid)
  n_incon <- sum(result == "Inconsistent")
  list(
    table = tbl,
    n_total = n,
    n_valid = n_valid,
    n_inconsistent = n_incon,
    n_boundary = sum(result == "Boundary"),
    n_invalid = sum(!valid),
    summary = sprintf("%d of %d reported means are GRIM-inconsistent.", n_incon, n_valid)
  )
}

#' Parse the per-row CSV textarea into an entries data frame.
#' Each non-blank line: mean, n, scale_min, scale_max[, items][, label].
#' @return list(data = data.frame or NULL, errors = character()).
#' @noRd
parse_grim_input <- function(text) {
  if (is.null(text) || !nzchar(trimws(text))) return(list(data = NULL, errors = character(0)))

  lines <- trimws(strsplit(text, "\n")[[1]])
  idx <- which(nzchar(lines))
  lines <- lines[idx]

  rows <- list(); errors <- character(0)
  for (i in seq_along(lines)) {
    parts <- trimws(strsplit(lines[i], ",")[[1]])
    if (length(parts) < 4) {
      errors <- c(errors, sprintf(
        "Line %d: expected at least 4 fields (mean, n, scale_min, scale_max).", idx[i]))
      next
    }
    mean_str <- parts[1]
    nums <- suppressWarnings(as.numeric(parts[2:4]))
    items <- if (length(parts) >= 5 && nzchar(parts[5])) suppressWarnings(as.numeric(parts[5])) else 1
    label <- if (length(parts) >= 6) paste(parts[6:length(parts)], collapse = ", ") else ""

    if (is.na(suppressWarnings(as.numeric(mean_str))) || any(is.na(nums)) || is.na(items)) {
      errors <- c(errors, sprintf("Line %d: non-numeric value.", idx[i]))
      next
    }
    rows[[length(rows) + 1]] <- data.frame(
      mean = mean_str, n = nums[1], scale_min = nums[2], scale_max = nums[3],
      items = items, label = label, stringsAsFactors = FALSE
    )
  }

  data <- if (length(rows)) do.call(rbind, rows) else NULL
  list(data = data, errors = errors)
}

#' Display table for §9.5 (with a couple of helpful extra columns).
#' @noRd
display_grim_table <- function(tbl) {
  res_label <- c(
    Consistent   = "✅ Consistent",
    Inconsistent = "❌ Inconsistent",
    Boundary     = "⚠️ Boundary",
    Invalid      = "⛔ Invalid"
  )
  data.frame(
    Label         = ifelse(nzchar(tbl$label), tbl$label, "—"),
    M             = tbl$mean,
    n             = tbl$n,
    Scale         = sprintf("%g–%g", tbl$scale_min, tbl$scale_max),
    Items         = tbl$items,
    `GRIM result` = unname(res_label[tbl$result]),
    Note          = tbl$note,
    check.names = FALSE, stringsAsFactors = FALSE
  )
}
