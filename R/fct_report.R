# Unified report (FR-31..FR-35). Aggregates the five module results into a
# summary card + per-test sections, and renders a self-contained HTML export.
#
# The overall integrity signal is driven by CORE-tier results ONLY (DR-03 /
# FR-32a): statcheck, GRIM, SPRITE. p-curve (Exploratory) and z-curve
# (Meta-analysis) appear as context but never move the signal.

DISCLAIMER <- paste0(
  "This report is a statistical screening tool. Flagged results indicate ",
  "potential inconsistencies warranting further examination, not evidence of ",
  "misconduct. No single automated flag is sufficient grounds for an editorial ",
  "decision or an allegation; all flags require human review and, where ",
  "relevant, author clarification."
)

#' Roll up Core-tier severities into counts + an overall signal.
#' Inputs may be NULL (module not run yet).
#' @noRd
core_severity_counts <- function(statcheck, grim, sprite) {
  crit <- 0L; conc <- 0L; info <- 0L; any_run <- FALSE

  if (isTRUE(statcheck$ok)) {
    any_run <- TRUE
    sc <- statcheck$severity_counts
    crit <- crit + as.integer(sc[["Critical"]])
    conc <- conc + as.integer(sc[["Concern"]])
    info <- info + as.integer(sc[["Consistent"]])
  }
  if (isTRUE(grim$ok) && !is.null(grim$grim)) {
    any_run <- TRUE
    crit <- crit + as.integer(grim$grim$n_inconsistent)  # impossible mean = Critical
    info <- info + as.integer(grim$grim$n_boundary)
  }
  if (isTRUE(sprite$ok)) {
    any_run <- TRUE
    if (identical(sprite$status, "impossible")) crit <- crit + 1L
    else if (identical(sprite$status, "inconclusive")) info <- info + 1L
  }

  signal <- if (!any_run) "none" else if (crit > 0) "red" else if (conc > 0) "yellow" else "green"
  list(signal = signal, n_critical = crit, n_concern = conc,
       n_informational = info, any_run = any_run)
}

#' Map a signal to its FR-32 label.
#' @noRd
signal_label <- function(signal) {
  switch(signal,
         green  = "🟢 No issues detected",
         yellow = "🟡 Minor inconsistencies",
         red    = "🔴 Significant concerns",
         "⚪ No analyses run yet")
}

#' Assemble the full report structure from the five module results.
#' @noRd
build_report <- function(statcheck, pcurve, zcurve, grim, sprite) {
  core <- core_severity_counts(statcheck, grim, sprite)
  list(
    generated = Sys.time(),
    signal = core$signal,
    signal_label = signal_label(core$signal),
    n_statistics_checked = if (isTRUE(statcheck$ok)) statcheck$n_checked else 0L,
    n_critical = core$n_critical,
    n_concern = core$n_concern,
    n_informational = core$n_informational,
    any_core_run = core$any_run,
    statcheck = statcheck, pcurve = pcurve, zcurve = zcurve,
    grim = grim, sprite = sprite
  )
}

# --- HTML helpers -------------------------------------------------------------

#' Render a data frame as a simple HTML table.
#' @noRd
df_to_html_table <- function(df) {
  if (is.null(df) || nrow(df) == 0) return(tags$p(tags$em("No rows.")))
  header <- tags$tr(lapply(names(df), tags$th))
  body <- lapply(seq_len(nrow(df)), function(i) {
    tags$tr(lapply(df[i, ], function(cell) tags$td(as.character(cell))))
  })
  tags$table(class = "ris-table", tags$thead(header), tags$tbody(body))
}

#' @noRd
report_summary_card <- function(report) {
  cls <- switch(report$signal, green = "ris-green", yellow = "ris-yellow",
                red = "ris-red", "ris-none")
  div(
    class = paste("ris-card", cls),
    div(class = "ris-signal", report$signal_label),
    tags$ul(
      tags$li(sprintf("Statistics checked (statcheck, canonical): %d", report$n_statistics_checked)),
      tags$li("Additional p-values not statcheck-verified: none in v1"),
      tags$li(sprintf("Critical flags: %d", report$n_critical)),
      tags$li(sprintf("Concern flags: %d", report$n_concern)),
      tags$li(sprintf("Informational: %d", report$n_informational))
    ),
    tags$p(class = "ris-muted", "Signal is based on Core-tier tests only (statcheck, GRIM, SPRITE).")
  )
}

#' @noRd
not_run_note <- function(x) {
  msg <- if (!is.null(x) && !is.null(x$message)) x$message else "Not run."
  tags$p(tags$em(msg))
}

#' @noRd
section_statcheck <- function(sc) {
  if (is.null(sc) || !isTRUE(sc$ok)) return(not_run_note(sc))
  tagList(
    tags$p(sprintf("%d statistics checked; %d inconsistent; %d decision errors.",
                   sc$n_checked, sc$n_inconsistent, sc$n_decision_errors)),
    df_to_html_table(display_statcheck_table(sc$table))
  )
}

#' @noRd
section_pcurve <- function(pc) {
  if (is.null(pc) || !isTRUE(pc$ok)) return(not_run_note(pc))
  fp <- function(p) if (is.na(p)) "-" else fmt_p(p)
  tagList(
    tags$p(tags$strong("Screening heuristic - not evidence."),
           " Based on auto-extracted tests, not hand-selected focal tests."),
    tags$ul(
      tags$li(sprintf("Right-skew test: Z = %.2f, p = %s", pc$rightskew$z_full, fp(pc$rightskew$p_full))),
      tags$li(sprintf("Flatness test: Z = %.2f, p = %s", pc$flatness$z_full, fp(pc$flatness$p_full))),
      tags$li(sprintf("Estimated power: %s",
                      if (is.na(pc$power)) "-" else paste0(round(100 * pc$power), "%")))
    ),
    tags$p(pc$interpretation)
  )
}

#' @noRd
section_zcurve <- function(zc) {
  if (is.null(zc) || !isTRUE(zc$ok)) return(not_run_note(zc))
  pct <- function(x) sprintf("%d%%", round(100 * x))
  tagList(
    tags$ul(
      tags$li(sprintf("EDR: %s, 95%% CI [%s, %s]", pct(zc$edr$est), pct(zc$edr$lo), pct(zc$edr$hi))),
      tags$li(sprintf("ERR: %s, 95%% CI [%s, %s]", pct(zc$err$est), pct(zc$err$lo), pct(zc$err$hi)))
    ),
    tags$p(zc$interpretation)
  )
}

#' @noRd
section_grim <- function(g) {
  if (is.null(g) || !isTRUE(g$ok) || is.null(g$grim)) return(not_run_note(g))
  tagList(
    tags$p(g$grim$summary),
    df_to_html_table(display_grim_table(g$grim$table))
  )
}

#' @noRd
section_sprite <- function(s) {
  if (is.null(s) || !isTRUE(s$ok)) return(not_run_note(s))
  tags$p(sprintf("M = %s, SD = %s, n = %d (scale %g–%g): %s",
                 s$mean, s$sd, s$n, s$scale_min, s$scale_max, s$message))
}

#' Named list of report sections (FR-33). Shared by the on-screen view and the
#' HTML export.
#' @noRd
report_sections <- function(report) {
  list(
    list(title = "statcheck (Core)",          body = section_statcheck(report$statcheck)),
    list(title = "p-curve (Exploratory)",     body = section_pcurve(report$pcurve)),
    list(title = "z-curve (Meta-analysis)",   body = section_zcurve(report$zcurve)),
    list(title = "GRIM (Core)",               body = section_grim(report$grim)),
    list(title = "SPRITE (Core)",             body = section_sprite(report$sprite))
  )
}

#' @noRd
report_css <- function() {
  paste(
    "body{font-family:system-ui,Arial,sans-serif;margin:2rem;max-width:900px;color:#222}",
    ".meta{color:#666;font-size:.9rem}",
    ".ris-card{border:1px solid #ddd;border-left-width:8px;border-radius:6px;padding:1rem 1.25rem;margin:1rem 0}",
    ".ris-green{border-left-color:#2e7d32}.ris-yellow{border-left-color:#f9a825}",
    ".ris-red{border-left-color:#c62828}.ris-none{border-left-color:#9e9e9e}",
    ".ris-signal{font-size:1.3rem;font-weight:700;margin-bottom:.5rem}",
    ".ris-muted{color:#666;font-size:.85rem}",
    ".ris-table{border-collapse:collapse;width:100%;margin:.5rem 0;font-size:.9rem}",
    ".ris-table th,.ris-table td{border:1px solid #ddd;padding:4px 8px;text-align:left}",
    ".ris-table thead{background:#f5f5f5}",
    ".disclaimer{color:#555;font-size:.85rem;font-style:italic}",
    sep = "\n"
  )
}

#' Build the self-contained HTML report document (FR-34). Pure in-memory tag
#' construction (FR-34a preferred path) - no rmarkdown/knitr temp directory, and
#' it contains computed results only, never the PDF or extracted text (§9.7).
#' @noRd
build_report_html <- function(report, file_name = NULL) {
  secs <- report_sections(report)
  tags$html(
    lang = "en",
    tags$head(
      tags$meta(charset = "utf-8"),
      tags$title("Research Integrity Screener - Report"),
      tags$style(htmltools::HTML(report_css()))
    ),
    tags$body(
      tags$h1("Research Integrity Screener - Report"),
      tags$p(
        class = "meta",
        sprintf("Generated: %s", format(report$generated, "%Y-%m-%d %H:%M:%S")),
        if (!is.null(file_name) && nzchar(file_name)) sprintf(" · File: %s", file_name) else NULL
      ),
      report_summary_card(report),
      lapply(secs, function(s) tagList(tags$h2(s$title), s$body)),
      tags$hr(),
      tags$p(class = "disclaimer", DISCLAIMER)
    )
  )
}
