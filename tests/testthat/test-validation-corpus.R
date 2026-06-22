# Validation corpus (NFR-16/17/19). A committed set of inputs with golden
# expected outputs, exercised on every test run as a cross-module regression
# guard and acceptance check.
#
# NOTE: real-paper fixtures (known-inconsistent e.g. Wansink, known-clean) still
# need to be sourced as PDFs and are a pending addition. The synthetic cases
# below fully cover NFR-16's hand-computed requirements. statcheck does not parse
# bare `z = ...` statistics (no df anchor), so the statcheck corpus covers t, F,
# chi-square, and r.

# --- statcheck golden cases (severities verified against statcheck 1.5.0) ------

statcheck_corpus <- list(
  list(text = "t(28) = 2.20, p = .04",     severity = "Consistent"),
  list(text = "t(28) = 1.50, p = .04",     severity = "Critical"),    # decision error
  list(text = "F(2, 30) = 4.10, p = .001", severity = "Concern"),     # inconsistent, not decision
  list(text = "F(2, 30) = 1.00, p = .04",  severity = "Critical"),
  list(text = "chi2(1) = 3.84, p = .05",   severity = "Consistent"),
  list(text = "chi2(1) = 2.00, p = .04",   severity = "Critical"),
  list(text = "r(48) = .30, p = .04",      severity = "Consistent"),
  list(text = "r(48) = .10, p = .04",      severity = "Critical")
)

test_that("statcheck corpus: severities are stable (golden)", {
  for (case in statcheck_corpus) {
    res <- run_statcheck(case$text)
    expect_true(res$ok, info = case$text)
    expect_equal(nrow(res$table), 1, info = case$text)
    expect_equal(res$table$severity[1], case$severity, info = case$text)
  }
})

# --- GRIM golden cases (hand-computed) ----------------------------------------

grim_entry <- function(mean, n) {
  data.frame(mean = mean, n = n, scale_min = 1, scale_max = 7, items = 1,
             label = "", stringsAsFactors = FALSE)
}

grim_corpus <- list(
  list(mean = "5.19", n = 28, expect = "Inconsistent"),
  list(mean = "5.21", n = 28, expect = "Consistent"),
  list(mean = "2.5",  n = 3,  expect = "Inconsistent"),  # 2.5*3 = 7.5, impossible
  list(mean = "2.0",  n = 3,  expect = "Consistent"),    # 6/3
  list(mean = "1.50", n = 2,  expect = "Consistent")     # 3/2
)

test_that("GRIM corpus: results match hand computation", {
  for (case in grim_corpus) {
    res <- run_grim(grim_entry(case$mean, case$n))
    expect_equal(res$table$result[1], case$expect,
                 info = paste("GRIM", case$mean, "n =", case$n))
  }
})

# Real documented GRIM failures from van der Zee, Anaya & Brown (2017),
# "Statistical heartburn" (BMC Nutrition, CC BY) — Cornell pizza papers, 1-9
# Likert scales. Numbers only; each re-verified against scrutiny::grim().
test_that("GRIM corpus: documented Wansink pizza-paper means are inconsistent", {
  wansink <- list(
    list(mean = "6.62", n = 62),  # Art 1: "hungry when I came in"
    list(mean = "1.88", n = 62),  # Art 1: "I am hungry now"
    list(mean = "2.25", n = 10),  # Art 4: n=10 requires .X0
    list(mean = "3.92", n = 10)   # Art 4: "ate more than I should have"
  )
  for (case in wansink) {
    e <- data.frame(mean = case$mean, n = case$n, scale_min = 1, scale_max = 9,
                    items = 1, label = "", stringsAsFactors = FALSE)
    expect_equal(run_grim(e)$table$result[1], "Inconsistent",
                 info = paste("Wansink", case$mean, "n =", case$n))
  }
})

# --- SPRITE golden cases (deterministic feasibility) --------------------------

sprite_corpus <- list(
  list(mean = "4.00", sd = "1.95", n = 20, min = 1, max = 7, expect = "feasible"),
  list(mean = "1.00", sd = "2.00", n = 20, min = 1, max = 5, expect = "impossible"),
  list(mean = "5.23", sd = "1.11", n = 28, min = 1, max = 7, expect = "impossible")
)

test_that("SPRITE corpus: feasibility verdicts match", {
  for (case in sprite_corpus) {
    res <- run_sprite(case$mean, case$sd, case$n, case$min, case$max,
                      max_distributions = 5)
    expect_equal(res$status, case$expect,
                 info = paste("SPRITE", case$mean, case$sd, "n =", case$n))
  }
})

# --- acceptance checks (NFR-19) -----------------------------------------------

test_that("acceptance: a clean paper yields no critical flags and a green signal", {
  sc <- run_statcheck("t(28) = 2.20, p = .04. r(48) = .30, p = .04.")
  expect_true(sc$ok)
  expect_equal(unname(sc$severity_counts[["Critical"]]), 0)
  rep <- build_report(sc, NULL, NULL, NULL, NULL)
  expect_equal(rep$signal, "green")
})

test_that("acceptance: a decision error drives a red signal", {
  sc <- run_statcheck("t(28) = 1.50, p = .04.")
  rep <- build_report(sc, NULL, NULL, NULL, NULL)
  expect_equal(rep$signal, "red")
})

test_that("acceptance: synthetic impossible values are all flagged", {
  g <- run_grim(grim_entry("5.19", 28))
  expect_equal(g$table$result[1], "Inconsistent")
  s <- run_sprite("1.00", "2.00", 20, 1, 5, max_distributions = 5)
  expect_equal(s$status, "impossible")
})
