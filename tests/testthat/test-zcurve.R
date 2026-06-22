# Tests for the z-curve module (FR-15..FR-18). Small bootstrap + seed keep the
# fit fast and deterministic.

# --- gating (FR-18) ---

test_that("z-curve is not applicable with fewer than 20 significant tests", {
  r <- run_zcurve(rep(.01, 10))
  expect_false(r$ok)
  expect_equal(r$n, 10)
  expect_match(r$message, "at least 20")
  expect_match(r$message, "meta-analyses")
})

test_that("non-significant p-values do not count toward the threshold", {
  # 19 significant + 10 non-significant -> still below 20.
  r <- run_zcurve(c(rep(.01, 19), rep(.30, 10)))
  expect_false(r$ok)
  expect_equal(r$n, 19)
})

# --- fitting (FR-16) ---

test_that("z-curve fits and returns EDR/ERR with ordered 95% CIs", {
  p <- rep(c(.001, .005, .01, .02, .03), 6)   # 30 significant tests
  r <- run_zcurve(p, bootstrap = 100, seed = 1)
  expect_true(r$ok)
  expect_equal(r$n, 30)
  for (est in list(r$edr, r$err)) {
    expect_gte(est$est, 0); expect_lte(est$est, 1)
    expect_lte(est$lo, est$est)
    expect_gte(est$hi, est$est)
  }
  expect_s3_class(r$fit, "zcurve")
})

# --- interpretation (FR-17) ---

test_that("zcurve_interpretation reports EDR and ERR in plain language", {
  txt <- zcurve_interpretation(0.31, 0.62)
  expect_match(txt, "EDR")
  expect_match(txt, "ERR")
  expect_match(txt, "31%")
  expect_match(txt, "62%")
})

test_that("edr_freq gives an 'about 1 in N' phrase", {
  expect_match(edr_freq(0.33), "1 in 3")
  expect_match(edr_freq(0.25), "1 in 4")
  expect_equal(edr_freq(0), "very few")
})

# --- shiny module server ---

test_that("mod_zcurve_server is not applicable for a typical single paper", {
  sc <- list(ok = TRUE, computed_p = rep(.01, 8))   # too few
  shiny::testServer(
    mod_zcurve_server,
    args = list(statcheck_res = reactive(sc)),
    {
      r <- session$returned()
      expect_false(r$ok)
      expect_match(r$message, "at least 20")
    }
  )
})
