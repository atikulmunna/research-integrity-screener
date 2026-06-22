# Diagnostic: run the extraction + statcheck pipeline on a PDF and print what
# RIS sees. Usage:  Rscript dev/inspect_pdf.R "C:/path/to/paper.pdf"

.libPaths(c(file.path(Sys.getenv("LOCALAPPDATA"), "R", "win-library", "4.6"), .libPaths()))
suppressMessages(devtools::load_all(".", quiet = TRUE))

pdf <- commandArgs(trailingOnly = TRUE)[1]
cat("file:", pdf, "\n")
cat("exists:", file.exists(pdf), "\n\n")

ext <- extract_pdf_text(pdf)
cat(sprintf("extraction: ok=%s type=%s pages=%d words=%d\n",
            ext$ok, ext$type, ext$n_pages, ext$n_words))
if (!isTRUE(ext$ok)) { cat("message:", ext$message, "\n"); quit(save = "no") }

ac <- applicability_check(ext$text)
cat("applicability out_of_scope:", ac$out_of_scope,
    if (length(ac$matched)) paste0(" (", paste(ac$matched, collapse = ", "), ")") else "", "\n")

txt <- paste(ext$text, collapse = " ")
cnt <- function(p) length(gregexpr(p, txt)[[1]][gregexpr(p, txt)[[1]] > 0])
cat(sprintf("APA-ish markers in text:  t( =%d  F( =%d  chi/χ =%d  ' p' =%d\n",
            cnt("t\\("), cnt("F\\("), cnt("χ|chi"), cnt(" p *[=<>]")))

sc <- run_statcheck(ext$text)
if (!isTRUE(sc$ok)) {
  cat("\nstatcheck:", sc$message, "\n")
} else {
  cat(sprintf("\nstatcheck: %d checked, %d inconsistent, %d decision errors\n",
              sc$n_checked, sc$n_inconsistent, sc$n_decision_errors))
}
