# NFR-15 parity: RIS's custom p-curve vs dmetar::pcurve(), an independent R
# implementation of Simonsohn's p-curve. Both engines receive identical inputs
# (p-values -> z-scores; dmetar via metagen(TE = z, seTE = 1), RIS via chi^2(1)
# with x = z^2). dmetar/meta are dev-only references - skipped if absent, never
# added to the package's runtime Imports.
#
# Tolerances are generous because dmetar rounds its reported statistics to three
# decimals and estimates power on a grid (RIS uses a root finder).

skip_if_not_installed("dmetar")
skip_if_not_installed("meta")

dmetar_pcurve <- function(ps) {
  z <- qnorm(1 - ps / 2)
  m <- meta::metagen(TE = z, seTE = rep(1, length(z)),
                     studlab = paste0("S", seq_along(ps)))
  pc <- suppressWarnings(dmetar::pcurve(m))
  list(
    skew_p = pc$pcurveResults["Right-skewness test", "pFull"],
    flat_p = pc$pcurveResults["Flatness test", "pFull"],
    power  = pc$Power$powerEstimate
  )
}

ris_pcurve <- function(ps) {
  z <- qnorm(1 - ps / 2)
  run_pcurve(data.frame(family = "chisq", x = z^2, df1 = 1, df2 = NA, p = ps))
}

expect_parity <- function(ps) {
  d <- dmetar_pcurve(ps)
  r <- ris_pcurve(ps)
  expect_equal(r$rightskew$p_full, d$skew_p, tolerance = 0.01,
               info = "right-skew pFull")
  expect_equal(r$flatness$p_full, d$flat_p, tolerance = 0.01,
               info = "flatness pFull")
  expect_equal(r$power, d$power, tolerance = 0.03, info = "power estimate")
}

test_that("p-curve matches dmetar on a mixed-strength set", {
  expect_parity(c(.001, .003, .008, .012, .02, .03, .04))
})

test_that("p-curve matches dmetar on a strong set", {
  expect_parity(c(.001, .001, .002, .004, .006, .009))
})

test_that("p-curve matches dmetar on a moderate set", {
  expect_parity(c(.005, .01, .015, .02, .024, .03, .04))
})
