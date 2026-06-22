# Tests for the PDF extraction module (FR-01..FR-05).
# PDF generators live in helper-pdf.R.

# --- pure helpers ---

test_that("count_words counts whitespace-delimited tokens", {
  expect_equal(count_words("alpha beta gamma"), 3)
  expect_equal(count_words(c("one two", "three")), 3)
  expect_equal(count_words(c("", "   ")), 0)
  expect_equal(count_words(character(0)), 0)
})

test_that("classify_pdf_error distinguishes encrypted from corrupted", {
  expect_equal(classify_pdf_error("PDF requires a password to open"), "encrypted")
  expect_equal(classify_pdf_error("file is encrypted"), "encrypted")
  expect_equal(classify_pdf_error("insufficient permission"), "encrypted")
  expect_equal(classify_pdf_error("some random damage"), "corrupted")
  expect_equal(classify_pdf_error("damaged", encrypted_flag = TRUE), "encrypted")
})

test_that("pdf_error_message returns a non-empty human-readable string", {
  for (t in c("encrypted", "corrupted", "scanned", "not_found", "unknown")) {
    expect_true(nzchar(pdf_error_message(t)))
    expect_false(grepl("Error in|stack", pdf_error_message(t)))  # not a stack trace
  }
})

# --- extraction roundtrip ---

test_that("a valid text PDF extracts successfully", {
  p <- make_text_pdf("The quick brown fox jumps over the lazy dog repeatedly today")
  on.exit(unlink(p))
  res <- extract_pdf_text(p)
  expect_true(res$ok)
  expect_equal(res$type, "ok")
  expect_equal(res$n_pages, 1)
  expect_gt(res$n_words, 5)
  expect_type(res$text, "character")
})

test_that("multi-page PDF reports the correct page count", {
  p <- make_text_pdf(c(
    "page one has several distinct words present here now",
    "page two also has several distinct words present here now",
    "page three likewise has several distinct words present"
  ))
  on.exit(unlink(p))
  res <- extract_pdf_text(p)
  expect_true(res$ok)
  expect_equal(res$n_pages, 3)
})

test_that("image-only / scanned PDF (no text layer) is flagged", {
  p <- make_blank_pdf(2)
  on.exit(unlink(p))
  res <- extract_pdf_text(p)
  expect_false(res$ok)
  expect_equal(res$type, "scanned")
  expect_match(res$message, "image-only|scanned")
})

test_that("corrupted / non-PDF file is flagged, not crashed", {
  p <- tempfile(fileext = ".pdf")
  writeLines("this is definitely not a pdf", p)
  on.exit(unlink(p))
  res <- extract_pdf_text(p)
  expect_false(res$ok)
  expect_equal(res$type, "corrupted")
})

test_that("missing file is handled gracefully", {
  res <- extract_pdf_text(tempfile(fileext = ".pdf"))
  expect_false(res$ok)
  expect_equal(res$type, "not_found")
})

# --- shiny module server ---

test_that("mod_upload_server returns an extraction reactive and renders status", {
  p <- make_text_pdf("alpha beta gamma delta epsilon zeta eta theta iota kappa lambda")
  on.exit(unlink(p))
  shiny::testServer(mod_upload_server, {
    session$setInputs(pdf = list(datapath = p, name = "x.pdf", size = file.size(p)))
    out <- session$returned()
    expect_true(out$ok)
    expect_equal(out$type, "ok")
    expect_false(is.null(output$status))  # status UI rendered
  })
})
