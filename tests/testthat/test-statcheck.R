# Tests for the statcheck module (FR-06..FR-10, FR-10a).

# --- pure helpers ---

test_that("statcheck_severity maps flags to labels (decision error dominates)", {
  expect_equal(statcheck_severity(FALSE, FALSE), "Consistent")
  expect_equal(statcheck_severity(TRUE,  FALSE), "Concern")
  expect_equal(statcheck_severity(TRUE,  TRUE),  "Critical")
  expect_equal(statcheck_severity(FALSE, TRUE),  "Critical")
  expect_equal(statcheck_severity(NA,    NA),    "Consistent")  # NA-safe
})

test_that("format_df renders the df string per test type", {
  expect_equal(format_df("t",    NA, 28), "28")
  expect_equal(format_df("F",    2,  30), "2, 30")
  expect_equal(format_df("Chi2", 1,  NA), "1")
  expect_equal(format_df("r",    NA, 50), "50")
  expect_equal(format_df("Z",    NA, NA), "")
})

test_that("fmt_p formats APA p-values", {
  expect_equal(fmt_p(0.0362), ".036")
  expect_equal(fmt_p(0.0005), "< .001")
  expect_equal(fmt_p(0.05),   ".050")
  expect_equal(fmt_p(NA_real_), "—")
})

# --- run_statcheck against the real statcheck engine ---

test_that("a decision error is flagged Critical", {
  res <- run_statcheck("A test t(28) = 1.50, p = .04 was significant.")
  expect_true(res$ok)
  expect_equal(res$n_checked, 1)
  expect_true(res$table$decision_error)
  expect_equal(res$table$severity, "Critical")
  expect_equal(res$n_decision_errors, 1)
})

test_that("a rounding-consistent stat is Consistent", {
  res <- run_statcheck("We report t(28) = 2.20, p = .04.")
  expect_true(res$ok)
  expect_true(res$table$consistent)
  expect_equal(res$table$severity, "Consistent")
  expect_equal(res$n_inconsistent, 0)
})

test_that("a non-decision inconsistency is Concern", {
  res <- run_statcheck("Here F(2, 30) = 4.10, p = .001 was observed.")
  expect_true(res$ok)
  expect_false(res$table$consistent)
  expect_equal(res$table$severity, "Concern")
  expect_equal(res$n_decision_errors, 0)
})

test_that("text with no APA statistics returns the FR-10 message", {
  res <- run_statcheck("This manuscript contains no formatted statistics at all.")
  expect_false(res$ok)
  expect_match(res$message, "No APA-formatted statistics")
  expect_equal(res$n_checked, 0)
  expect_equal(nrow(res$table), 0)
})

test_that("computed_p is exposed as the canonical p-value set (FR-10a)", {
  res <- run_statcheck("Results: t(28) = 2.20, p = .04; r(50) = .30, p = .03.")
  expect_true(res$ok)
  expect_equal(length(res$computed_p), 2)
  expect_true(all(res$computed_p > 0 & res$computed_p < 1))
})

test_that("display table has the §9.2 columns", {
  res <- run_statcheck("t(28) = 2.20, p = .04")
  disp <- display_statcheck_table(res$table)
  expect_setequal(
    names(disp),
    c("Statistic", "Type", "df", "Test value", "Reported p",
      "Computed p", "Consistent", "Decision error", "Severity")
  )
})

# --- shiny module server ---

test_that("mod_statcheck_server runs against an extraction reactive", {
  shiny::testServer(
    mod_statcheck_server,
    args = list(extraction = reactive(list(ok = TRUE, text = "t(28) = 1.50, p = .04"))),
    {
      r <- session$returned()
      expect_true(r$ok)
      expect_equal(r$table$severity, "Critical")
      expect_false(is.null(output$summary))
    }
  )
})

test_that("mod_statcheck_server stays idle until extraction succeeds", {
  shiny::testServer(
    mod_statcheck_server,
    args = list(extraction = reactive(list(ok = FALSE, text = NULL))),
    {
      # result() requires ok == TRUE, so it should not resolve.
      expect_error(result(), class = "shiny.silent.error")
    }
  )
})
