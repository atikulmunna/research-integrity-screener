# Tests for the GRIM module (FR-19..FR-25). Values verified against
# scrutiny::grim() and hand computation (NFR-16).

entries_df <- function(...) {
  rows <- list(...)
  do.call(rbind, lapply(rows, function(r) {
    data.frame(mean = r[[1]], n = r[[2]], scale_min = r[[3]],
               scale_max = r[[4]], items = r[[5]], label = r[[6]],
               stringsAsFactors = FALSE)
  }))
}

# --- pure helpers ---

test_that("decimals_in counts decimal places", {
  expect_equal(decimals_in("5.19"), 2)
  expect_equal(decimals_in("5.2"), 1)
  expect_equal(decimals_in("5"), 0)
  expect_equal(decimals_in(c("5.190", "7")), c(3, 0))
})

test_that("validate_grim_row enforces FR-21 constraints", {
  expect_true(is.na(validate_grim_row("5.19", 28, 1, 7, 1)))
  expect_match(validate_grim_row("5.19", 28, 7, 1, 1), "minimum must be less")
  expect_match(validate_grim_row("5.19", 0, 1, 7, 1), "Sample size")
  expect_match(validate_grim_row("5.19", 28.5, 1, 7, 1), "Sample size")
  expect_match(validate_grim_row("5.19", 28, 1, 7, 0), "items")
  expect_match(validate_grim_row("9.99", 28, 1, 7, 1), "within the scale bounds")
  expect_match(validate_grim_row("abc", 28, 1, 7, 1), "not numeric")
})

# --- run_grim against scrutiny ---

test_that("run_grim classifies consistent and inconsistent means", {
  res <- run_grim(entries_df(
    list("5.19", 28, 1, 7, 1, "a"),   # FALSE -> Inconsistent
    list("5.21", 28, 1, 7, 1, "b"),   # TRUE  -> Consistent
    list("3.45", 40, 1, 5, 1, "c")    # TRUE  -> Consistent
  ))
  expect_equal(res$table$result, c("Inconsistent", "Consistent", "Consistent"))
  expect_equal(res$n_inconsistent, 1)
  expect_equal(res$n_valid, 3)
  expect_match(res$summary, "^1 of 3 reported means are GRIM-inconsistent")
})

test_that("hand-computed impossible mean is inconsistent (mean 2.5 of 3 integers)", {
  res <- run_grim(entries_df(list("2.5", 3, 1, 7, 1, "")))
  expect_equal(res$table$result, "Inconsistent")
})

test_that("a mean on a scale bound is flagged Boundary", {
  res <- run_grim(entries_df(list("7", 28, 1, 7, 1, "max")))
  expect_equal(res$table$result, "Boundary")
})

test_that("items count feeds the GRIM denominator", {
  # grim('5.19', 28, items = 2) is FALSE -> Inconsistent
  res <- run_grim(entries_df(list("5.19", 28, 1, 7, 2, "")))
  expect_equal(res$table$result, "Inconsistent")
})

test_that("invalid rows are reported, not crashed", {
  res <- run_grim(entries_df(list("9.99", 10, 1, 7, 1, "oob")))
  expect_equal(res$table$result, "Invalid")
  expect_equal(res$n_invalid, 1)
  expect_equal(res$n_valid, 0)
})

test_that("FR-24a low-power notice fires for large n", {
  # 1.5 (1 dp) -> threshold 10^1 = 10; n = 30 >= 10 -> low power.
  res <- run_grim(entries_df(list("1.5", 30, 1, 7, 1, "")))
  expect_true(res$table$low_power)
  expect_match(res$table$note, "little discriminating power")
})

test_that("FR-24a notice does NOT fire below the granularity threshold", {
  # 1.50 (2 dp) -> threshold 100; n = 30 < 100 -> not low power.
  res <- run_grim(entries_df(list("1.50", 30, 1, 7, 1, "")))
  expect_false(res$table$low_power)
})

# --- parser ---

test_that("parse_grim_input handles full and short rows", {
  p <- parse_grim_input("5.19, 28, 1, 7, 1, Extraversion\n3.45, 40, 1, 5")
  expect_equal(nrow(p$data), 2)
  expect_equal(p$data$mean, c("5.19", "3.45"))
  expect_equal(p$data$items, c(1, 1))          # default when omitted
  expect_equal(p$data$label, c("Extraversion", ""))
  expect_length(p$errors, 0)
})

test_that("parse_grim_input flags malformed lines and skips blanks", {
  p <- parse_grim_input("5.19, 28, 1, 7\n\nbad line\n3.0, x, 1, 7")
  expect_equal(nrow(p$data), 1)
  expect_length(p$errors, 2)                    # 'bad line' + non-numeric n
  expect_match(p$errors[1], "Line 3")           # blank line did not shift index
})

test_that("parse_grim_input returns NULL data on empty input", {
  expect_null(parse_grim_input("")$data)
  expect_null(parse_grim_input(NULL)$data)
})

# --- shiny module server ---

test_that("mod_grim_server runs on button click and returns analysis", {
  shiny::testServer(mod_grim_server, {
    session$setInputs(entries = "5.19, 28, 1, 7\n5.21, 28, 1, 7", run = 1)
    a <- session$returned()
    expect_true(a$ok)
    expect_equal(a$grim$table$result, c("Inconsistent", "Consistent"))
  })
})

test_that("mod_grim_server caps at 50 rows", {
  many <- paste(rep("5.21, 28, 1, 7", 51), collapse = "\n")
  shiny::testServer(mod_grim_server, {
    session$setInputs(entries = many, run = 1)
    a <- session$returned()
    expect_false(a$ok)
    expect_match(a$message, "limited to 50 rows")
  })
})
