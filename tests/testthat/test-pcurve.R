# Tests for the p-curve module (FR-11..FR-14). Tests are built as chi^2(1)
# cases from target two-tailed p-values (via z), so the inputs have known p.

mk <- function(ps) {
  z <- qnorm(1 - ps / 2)
  data.frame(family = "chisq", x = z^2, df1 = 1, df2 = NA, p = ps)
}

# --- pp machinery ---

test_that("pp at the null (power = .05) equals p / .05", {
  x <- qchisq(1 - 0.02, 1)                       # chi^2(1) with upper-tail p = .02
  pp <- pp_at_power(0.05, "chisq", x, 1, NA, alpha = 0.05)
  expect_equal(pp, 0.02 / 0.05, tolerance = 1e-4)
})

test_that("estimate_power increases with stronger evidence", {
  expect_gt(estimate_power(mk(rep(.001, 6))), estimate_power(mk(rep(.04, 6))))
})

# --- run_pcurve behaviour ---

test_that("a strongly right-skewed set shows evidential value", {
  r <- run_pcurve(mk(rep(.001, 6)))
  expect_true(r$ok)
  expect_equal(r$k, 6)
  expect_lt(r$rightskew$p_full, 0.05)
  expect_equal(r$evidential_value, "present")
  expect_gt(r$power, 0.5)
})

test_that("a flat set near .05 shows inadequate evidential value", {
  r <- run_pcurve(mk(c(.04, .045, .048, .049, .043, .041)))
  expect_true(r$ok)
  expect_gt(r$rightskew$p_full, 0.05)     # not right-skewed
  expect_lt(r$flatness$p_full, 0.05)      # significantly flat
  expect_equal(r$evidential_value, "absent")
  expect_lt(r$power, 0.20)
})

test_that("fewer than 5 tests is refused (FR-14)", {
  r <- run_pcurve(mk(rep(.01, 4)))
  expect_false(r$ok)
  expect_equal(r$k, 4)
  expect_match(r$message, "at least 5")
})

test_that("non-significant p-values are dropped before counting", {
  r <- run_pcurve(mk(c(.01, .02, .03, .04, .045, .60, .80)))  # 2 are >= .05
  expect_true(r$ok)
  expect_equal(r$k, 5)                                        # 5 significant remain
})

# --- mapping from statcheck ---

test_that("pcurve_inputs_from_statcheck maps every family", {
  sc <- list(ok = TRUE, table = data.frame(
    test_type  = c("t", "F", "r", "Chi2", "Z"),
    df1        = c(NA, 2, NA, 1, NA),
    df2        = c(28, 30, 48, NA, NA),
    test_value = c(2.5, 5, 0.3, 6, 2.4),
    computed_p = c(.02, .01, .03, .014, .016),
    stringsAsFactors = FALSE
  ))
  inp <- pcurve_inputs_from_statcheck(sc)
  expect_equal(nrow(inp), 5)
  expect_equal(inp$family, c("F", "F", "F", "chisq", "chisq"))
  expect_equal(inp$x[1], 2.5^2)       # t -> F(1, df), x = t^2
  expect_equal(inp$df1[1], 1)
  expect_equal(inp$df2[1], 28)
})

test_that("pcurve_plot returns a ggplot", {
  expect_s3_class(pcurve_plot(c(.01, .02, .049, .03, .005)), "ggplot")
})

# --- end-to-end through statcheck ---

test_that("run_pcurve works end-to-end on statcheck output", {
  txt <- paste(
    "t(50) = 5.0, p < .001.",
    "t(40) = 4.5, p < .001.",
    "F(1, 60) = 20.1, p < .001.",
    "r(80) = .45, p < .001.",
    "z = 4.2, p < .001.",
    "chi2(1) = 16.0, p < .001."
  )
  sc <- run_statcheck(txt)
  skip_if_not(sc$ok && sum(sc$table$computed_p < .05) >= 5,
              "statcheck did not parse enough significant tests")
  r <- run_pcurve(pcurve_inputs_from_statcheck(sc))
  expect_true(r$ok)
  expect_equal(r$evidential_value, "present")
})

# --- shiny module server ---

test_that("mod_pcurve_server runs against a statcheck reactive", {
  z <- qnorm(1 - .001 / 2)
  sc <- list(ok = TRUE, table = data.frame(
    test_type  = rep("Z", 6),
    df1        = rep(NA, 6),
    df2        = rep(NA, 6),
    test_value = rep(z, 6),
    computed_p = rep(.001, 6),
    stringsAsFactors = FALSE
  ))
  shiny::testServer(
    mod_pcurve_server,
    args = list(statcheck_res = reactive(sc)),
    {
      r <- session$returned()
      expect_true(r$ok)
      expect_equal(r$evidential_value, "present")
    }
  )
})
