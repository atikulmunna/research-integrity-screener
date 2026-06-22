# Scaffold sanity checks: the golem app shell is wired up correctly.
# These are deliberately minimal - real coverage arrives module by module.

test_that("app_ui returns a shiny tag list", {
  ui <- app_ui(request = list())
  expect_s3_class(ui, "shiny.tag.list")
})

test_that("app_server is a function with the shiny signature", {
  expect_type(app_server, "closure")
  expect_named(formals(app_server), c("input", "output", "session"))
})

test_that("run_app is a function", {
  expect_type(run_app, "closure")
})

test_that("app_ui has the five UX-01 tabs", {
  html <- as.character(app_ui(request = list()))
  for (v in c("upload", "statcheck", "curves", "manual", "report")) {
    expect_match(html, sprintf('data-value="%s"', v), fixed = TRUE)
  }
})

test_that("app_ui loads shinyjs and the tab-gating style (UX-02)", {
  rt <- htmltools::renderTags(app_ui(request = list()))
  head <- paste(as.character(rt$head), collapse = "")
  expect_match(head, "shinyjs")
  expect_match(paste(as.character(rt$html), head), "ris-tab-disabled")
})
