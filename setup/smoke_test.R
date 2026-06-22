# Smoke test: confirm the environment actually runs, not just installs.
.libPaths(c(file.path(Sys.getenv("LOCALAPPDATA"), "R", "win-library", "4.6"), .libPaths()))

cat("1) scrutiny::grim — known GRIM-inconsistent mean (5.19, n=28) should be FALSE\n")
stopifnot(isFALSE(scrutiny::grim(x = "5.19", n = 28)))
cat("   ok: 5.19/28 flagged inconsistent\n")
cat("   check consistent case (5.21, n=28) should be TRUE\n")
stopifnot(isTRUE(scrutiny::grim(x = "5.21", n = 28)))
cat("   ok\n\n")

cat("2) statcheck — a known decision error: t(28)=2.20, p=.04 (recomputes ~.036, consistent) ",
    "vs an inconsistent one\n")
res <- statcheck::statcheck("t(28) = 1.00, p = .01", messages = FALSE)
stopifnot(is.data.frame(res), nrow(res) == 1)
cat("   ok: statcheck parsed 1 statistic; Error =", res$Error, "\n\n")

cat("3) pdftools — bundled poppler: generate a PDF and read its text back\n")
tmp <- tempfile(fileext = ".pdf")
pdf(tmp); plot.new(); text(0.5, 0.5, "RIS smoke test p = .023"); dev.off()
txt <- pdftools::pdf_text(tmp)
stopifnot(grepl("RIS smoke test", txt))
unlink(tmp)
cat("   ok: pdf_text extracted the expected string\n\n")

cat("4) zcurve — fit on a small synthetic z-vector\n")
set.seed(1)
z <- abs(rnorm(50, 2.5, 1)); z <- z[z > 1.96]
fit <- zcurve::zcurve(z = z)
stopifnot(!is.null(fit))
cat("   ok: zcurve fit returned\n\n")

cat("5) shiny + bslib + ggplot2 load\n")
suppressMessages({ library(shiny); library(bslib); library(ggplot2) })
cat("   ok\n\nSMOKE TEST PASSED\n")
