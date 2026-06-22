# Tests for the applicability heuristic (UX-08 / UX-08a).

test_that("out-of-scope keywords are flagged", {
  ac <- applicability_check("This deep learning model was trained on images.")
  expect_true(ac$out_of_scope)
  expect_true("deep learning" %in% ac$matched)
})

test_that("a typical quantitative paper is not flagged", {
  ac <- applicability_check("Participants rated items; we ran t(48) = 2.1, p = .04.")
  expect_false(ac$out_of_scope)
  expect_length(ac$matched, 0)
})

test_that("false-positive guard: neuroscience terms are NOT flagged", {
  # 'neural network' / 'neural correlates' / 'network analysis' are in scope.
  ac <- applicability_check(
    "We examined neural correlates and used network analysis of the neural network."
  )
  expect_false(ac$out_of_scope)
})

test_that("matching is case-insensitive and reports all hits", {
  ac <- applicability_check("A QUALITATIVE INTERVIEW study using Grounded Theory.")
  expect_true(ac$out_of_scope)
  expect_setequal(ac$matched, c("grounded theory", "qualitative interview"))
})

test_that("applicability_banner is NULL when in scope, a tag when not", {
  expect_null(applicability_banner(list(out_of_scope = FALSE, matched = character(0))))
  banner <- applicability_banner(list(out_of_scope = TRUE, matched = "deep learning"))
  expect_s3_class(banner, "shiny.tag")
  expect_match(as.character(banner), "scope limitation")
})

test_that("mod_upload shows the banner for an out-of-scope PDF", {
  p <- make_text_pdf("This deep learning study trained a model with many many words here")
  on.exit(unlink(p))
  shiny::testServer(mod_upload_server, {
    session$setInputs(pdf = list(datapath = p, name = "ml.pdf", size = file.size(p)))
    expect_false(is.null(output$applicability$html))
    expect_match(output$applicability$html, "scope limitation")
  })
})
