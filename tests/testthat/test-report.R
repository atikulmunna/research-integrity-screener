# Tests for the unified report (FR-31..FR-35, DR-03).

# Minimal stand-in module results.
sc_ok <- function(crit = 0, conc = 0, cons = 5) {
  list(ok = TRUE, n_checked = crit + conc + cons,
       n_inconsistent = crit + conc, n_decision_errors = crit,
       severity_counts = c(Consistent = cons, Concern = conc, Critical = crit),
       table = data.frame(
         statistic = "t(28) = 2.20, p = .04", test_type = "t", df = "28",
         test_value = 2.2, reported_p = .04, computed_p = .036,
         consistent = TRUE, decision_error = FALSE, severity = "Consistent",
         stringsAsFactors = FALSE))
}
grim_ok <- function(incon = 0, boundary = 0, valid = 3) {
  list(ok = TRUE, errors = character(0), message = NULL,
       grim = list(table = data.frame(
         label = "x", mean = "5.19", n = 28, scale_min = 1, scale_max = 7,
         items = 1, result = "Inconsistent", valid = TRUE, low_power = FALSE, note = "",
         stringsAsFactors = FALSE),
       n_total = valid, n_valid = valid, n_inconsistent = incon,
       n_boundary = boundary, n_invalid = 0,
       summary = sprintf("%d of %d reported means are GRIM-inconsistent.", incon, valid)))
}
sprite_res_status <- function(status) {
  list(ok = TRUE, status = status, feasible = status == "feasible",
       message = paste("status:", status), mean = "4.00", sd = "1.95", n = 20,
       scale_min = 1, scale_max = 7, items = 1, n_found = 1, example = NULL)
}

# --- core severity rollup / signal (DR-03) ---

test_that("clean Core results give a green signal", {
  c0 <- core_severity_counts(sc_ok(0, 0, 5), NULL, NULL)
  expect_equal(c0$signal, "green")
  expect_equal(c0$n_critical, 0)
})

test_that("a Concern (no Critical) gives yellow", {
  expect_equal(core_severity_counts(sc_ok(0, 2, 3), NULL, NULL)$signal, "yellow")
})

test_that("a statcheck decision error gives red", {
  expect_equal(core_severity_counts(sc_ok(1, 0, 4), NULL, NULL)$signal, "red")
})

test_that("a GRIM inconsistency is Critical (red)", {
  expect_equal(core_severity_counts(NULL, grim_ok(incon = 1), NULL)$signal, "red")
})

test_that("a SPRITE-impossible result is Critical (red)", {
  expect_equal(core_severity_counts(NULL, NULL, sprite_res_status("impossible"))$signal, "red")
})

test_that("nothing run gives the 'none' signal", {
  c0 <- core_severity_counts(NULL, NULL, NULL)
  expect_equal(c0$signal, "none")
  expect_false(c0$any_run)
})

# --- DR-03: Exploratory/Meta results must NOT move the signal ---

test_that("an 'absent' p-curve does not change a green Core signal", {
  pc <- list(ok = TRUE, k = 6, rightskew = list(z_full = 3, p_full = .99),
             flatness = list(z_full = 4, p_full = .0001), power = .05,
             evidential_value = "absent", interpretation = "flat")
  rep <- build_report(sc_ok(0, 0, 5), pc, NULL, NULL, NULL)
  expect_equal(rep$signal, "green")
})

# --- signal labels ---

test_that("signal_label maps to FR-32 text", {
  expect_match(signal_label("green"), "No issues")
  expect_match(signal_label("yellow"), "Minor")
  expect_match(signal_label("red"), "Significant")
  expect_match(signal_label("none"), "No analyses")
})

# --- build_report ---

test_that("build_report counts canonical statistics and severities", {
  rep <- build_report(sc_ok(1, 2, 3), NULL, NULL, grim_ok(incon = 1), NULL)
  expect_equal(rep$n_statistics_checked, 6)
  expect_equal(rep$n_critical, 2)         # 1 statcheck critical + 1 GRIM inconsistent
  expect_equal(rep$n_concern, 2)
  expect_equal(rep$signal, "red")
})

# --- HTML export ---

test_that("build_report_html produces a self-contained document with key content", {
  rep <- build_report(sc_ok(1, 0, 4), NULL, NULL, NULL, NULL)
  html <- as.character(build_report_html(rep, "paper.pdf"))
  expect_match(html, "Research Integrity Screener")
  expect_match(html, "Significant concerns")          # red label
  expect_match(html, "paper.pdf")                      # filename included
  expect_match(html, "No single automated flag")       # disclaimer (FR-35)
  expect_false(grepl("<script", html))                 # no external script deps
})

test_that("df_to_html_table renders rows", {
  tt <- df_to_html_table(data.frame(A = 1:2, B = c("x", "y")))
  expect_match(as.character(tt), "<table")
  expect_match(as.character(tt), "x")
})

# --- shiny module server ---

test_that("mod_report_server aggregates the module reactives", {
  shiny::testServer(
    mod_report_server,
    args = list(
      extraction   = reactive(list(ok = TRUE, text = "x", name = "paper.pdf")),
      statcheck_res = reactive(sc_ok(1, 0, 4)),
      pcurve_res   = reactive(NULL),
      zcurve_res   = reactive(NULL),
      grim_res     = reactive(grim_ok(incon = 1)),
      sprite_res   = reactive(sprite_res_status("impossible"))
    ),
    {
      rep <- session$returned()
      expect_equal(rep$signal, "red")
      expect_equal(rep$n_critical, 3)   # statcheck 1 + GRIM 1 + SPRITE 1
      expect_false(is.null(output$report))
    }
  )
})
