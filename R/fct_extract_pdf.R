# PDF text extraction and validation (FR-01..FR-05).
# Pure functions, no Shiny dependency, so they are directly unit-testable.

#' Build a uniform extraction result object.
#' @noRd
pdf_result <- function(ok, type, message, text = NULL, n_pages = 0L, n_words = 0L) {
  list(
    ok = ok,
    type = type,        # "ok" | "scanned" | "encrypted" | "corrupted" | "not_found"
    message = message,  # human-readable (NFR-10), never a raw stack trace
    text = text,        # character vector, one element per page (NULL on failure)
    n_pages = n_pages,
    n_words = n_words
  )
}

#' Count whitespace-delimited tokens across a character vector.
#' @noRd
count_words <- function(text) {
  if (length(text) == 0) return(0L)
  tokens <- unlist(strsplit(paste(text, collapse = " "), "\\s+"))
  sum(nzchar(tokens))
}

#' Classify a poppler/pdftools read failure into a user-facing error type.
#' Factored out so the encrypted-vs-corrupted decision is testable without a
#' real encrypted fixture (which is awkward to generate).
#' @noRd
classify_pdf_error <- function(msg, encrypted_flag = FALSE) {
  if (isTRUE(encrypted_flag) ||
      grepl("password|encrypt|permission", msg, ignore.case = TRUE)) {
    "encrypted"
  } else {
    "corrupted"
  }
}

#' Human-readable message for each failure type.
#' @noRd
pdf_error_message <- function(type) {
  switch(
    type,
    encrypted = "This PDF is password-protected and cannot be read. Please upload an unlocked copy.",
    corrupted = "This file could not be read as a PDF. It may be corrupted or not a valid PDF.",
    scanned   = paste0(
      "This PDF appears to be image-only or scanned — no text layer was found. ",
      "RIS needs a text-based PDF (OCR is not supported)."
    ),
    not_found = "The uploaded file could not be found.",
    "An unknown error occurred while reading the PDF."
  )
}

#' Extract and validate the text layer of a PDF.
#'
#' Implements FR-02 (pdftools extraction), FR-03 (reject encrypted / scanned /
#' corrupted with a clear message), and FR-04 stage 1 (page + word counts).
#' Nothing is written to disk here (FR-05 / NFR-06).
#'
#' @param path Path to a PDF file.
#' @param scanned_word_threshold At or below this many extracted words the PDF
#'   is treated as image-only/scanned.
#' @return A `pdf_result()` list.
#' @noRd
extract_pdf_text <- function(path, scanned_word_threshold = 10) {
  if (!file.exists(path)) {
    return(pdf_result(FALSE, "not_found", pdf_error_message("not_found")))
  }

  info <- tryCatch(pdftools::pdf_info(path), error = function(e) e)
  if (inherits(info, "error")) {
    type <- classify_pdf_error(conditionMessage(info))
    return(pdf_result(FALSE, type, pdf_error_message(type)))
  }

  text <- tryCatch(pdftools::pdf_text(path), error = function(e) e)
  if (inherits(text, "error")) {
    type <- classify_pdf_error(conditionMessage(text), encrypted_flag = isTRUE(info$encrypted))
    return(pdf_result(FALSE, type, pdf_error_message(type)))
  }

  n_pages <- if (!is.null(info$pages)) info$pages else length(text)
  n_words <- count_words(text)

  # An openable PDF with effectively no text layer is image-only/scanned.
  if (n_words <= scanned_word_threshold) {
    return(pdf_result(FALSE, "scanned", pdf_error_message("scanned"),
                      n_pages = n_pages, n_words = n_words))
  }

  pdf_result(TRUE, "ok", "Extraction succeeded.",
             text = text, n_pages = n_pages, n_words = n_words)
}
