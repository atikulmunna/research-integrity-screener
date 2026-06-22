# Real-paper validation corpus (NFR-16/19). Runs the actual extraction ->
# statcheck -> report pipeline on open-access PDFs (CC BY; see
# fixtures/FIXTURES_ATTRIBUTION.md). Skipped gracefully if the PDFs are absent.
#
# Assertions are stated as invariants (clean stays clean, flagged gets flagged)
# rather than brittle exact counts, so minor extraction differences don't break
# them while the validation claim still holds.

fixture <- function(name) testthat::test_path("fixtures", name)

test_that("known-clean paper: real PDF yields no critical/concern flags (green)", {
  pdf <- fixture("plos_timeperception_2011.pdf")
  skip_if_not(file.exists(pdf), "clean fixture PDF not present")

  ext <- extract_pdf_text(pdf)
  expect_true(ext$ok)

  sc <- run_statcheck(ext$text)
  expect_true(sc$ok)
  expect_gte(sc$n_checked, 5)                         # real APA stats were parsed
  expect_equal(unname(sc$severity_counts[["Critical"]]), 0)
  expect_equal(unname(sc$severity_counts[["Concern"]]), 0)

  rep <- build_report(sc, NULL, NULL, NULL, NULL)
  expect_equal(rep$signal, "green")
})

test_that("known-inconsistent paper: real PDF surfaces a decision error (red)", {
  pdf <- fixture("plos_timeperception_2016.pdf")
  skip_if_not(file.exists(pdf), "inconsistent fixture PDF not present")

  ext <- extract_pdf_text(pdf)
  expect_true(ext$ok)

  sc <- run_statcheck(ext$text)
  expect_true(sc$ok)
  expect_gte(sc$n_decision_errors, 1)                 # at least one decision error
  expect_gte(unname(sc$severity_counts[["Critical"]]), 1)

  rep <- build_report(sc, NULL, NULL, NULL, NULL)
  expect_equal(rep$signal, "red")

  # And p-curve can run on this paper's significant tests (>= 5).
  expect_gte(sum(sc$computed_p < 0.05, na.rm = TRUE), 5)
})
