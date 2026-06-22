# Tests for the SPRITE module (FR-26..FR-30). Small max_distributions keeps the
# stochastic search fast; seed makes it deterministic.

# --- validation ---

test_that("validate_sprite_row enforces FR-26 / §8.3 constraints", {
  expect_true(is.na(validate_sprite_row("3.00", "1.50", 20, 1, 7, 1)))
  expect_match(validate_sprite_row("3.00", "0",    20, 1, 7, 1), "SD must be a positive")
  expect_match(validate_sprite_row("3.00", "1.50", 1,  1, 7, 1), "between 2 and 200")
  expect_match(validate_sprite_row("3.00", "1.50", 250,1, 7, 1), "between 2 and 200")
  expect_match(validate_sprite_row("3.00", "1.50", 20, 7, 1, 1), "minimum must be less")
  expect_match(validate_sprite_row("9.99", "1.50", 20, 1, 7, 1), "within the scale bounds")
  expect_match(validate_sprite_row("x",    "1.50", 20, 1, 7, 1), "Mean is not numeric")
})

# --- run_sprite ---

test_that("a feasible combination returns an example distribution", {
  # mean=4.00, sd=1.95 is feasible by construction at 2 dp for n=20 (1-7).
  r <- run_sprite("4.00", "1.95", 20, 1, 7, max_distributions = 5, seed = 1)
  expect_true(r$ok)
  expect_equal(r$status, "feasible")
  expect_true(r$feasible)
  expect_gte(r$n_found, 1)
  expect_length(r$example, 20)
  expect_true(all(r$example >= 1 & r$example <= 7))
  expect_true(all(r$example == round(r$example)))   # integers
})

test_that("SD outside the achievable range is mathematically impossible", {
  # mean at the floor with a non-zero SD on a tight scale is impossible.
  r <- run_sprite("1.00", "2.00", 20, 1, 5, max_distributions = 5)
  expect_equal(r$status, "impossible")
  expect_false(r$feasible)
  expect_match(r$message, "mathematically impossible")
  expect_null(r$example)
})

test_that("a GRIMMER-inconsistent combination is impossible", {
  # 5.23 is GRIM-inconsistent at n = 28, so GRIMMER fails -> impossible.
  r <- run_sprite("5.23", "1.11", 28, 1, 7, max_distributions = 5)
  expect_equal(r$status, "impossible")
  expect_false(r$feasible)
})

test_that("invalid input is reported, not crashed", {
  r <- run_sprite("3.00", "1.50", 1, 1, 7)
  expect_false(r$ok)
  expect_equal(r$status, "invalid")
  expect_true(is.na(r$feasible))
})

test_that("the capped flag is set when the search hits max_distributions", {
  r <- run_sprite("4.00", "1.95", 20, 1, 7, max_distributions = 2, seed = 1)
  expect_equal(r$status, "feasible")
  expect_true(r$capped)
  expect_match(r$message, "At least")
})

# --- plot ---

test_that("sprite_plot returns a ggplot over the full scale", {
  p <- sprite_plot(c(1, 2, 2, 3, 7), 1, 7)
  expect_s3_class(p, "ggplot")
})

# --- shiny module server ---

test_that("mod_sprite_server runs on click and returns a feasible result", {
  shiny::testServer(mod_sprite_server, {
    session$setInputs(mean = "4.00", sd = "1.95", n = 20,
                      scale_min = 1, scale_max = 7, items = 1, run = 1)
    r <- session$returned()
    expect_equal(r$status, "feasible")
    expect_false(is.null(output$status))
  })
})
