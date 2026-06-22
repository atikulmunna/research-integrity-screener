# RIS dependency installer - run once to provision the R environment.
# Installs CRAN binaries (no source compilation expected on R 4.6 for Windows).

options(
  repos = c(CRAN = "https://cloud.r-project.org"),
  install.packages.check.source = "no",
  Ncpus = max(1L, parallel::detectCores())
)

lib <- file.path(Sys.getenv("LOCALAPPDATA"), "R", "win-library", "4.6")
dir.create(lib, recursive = TRUE, showWarnings = FALSE)
.libPaths(c(lib, .libPaths()))

pkgs <- c(
  # Core analysis
  "pdftools",        # PDF text extraction (bundles poppler on Windows)
  "statcheck",       # APA statistic consistency
  "zcurve",          # z-curve (meta-analysis tier)
  "scrutiny",        # GRIM + SPRITE
  # App framework + UI
  "golem",
  "shiny",
  "bslib",
  "shinycssloaders",
  # Visualization + reporting + text
  "ggplot2",
  "rmarkdown",
  "stringr",
  # Testing
  "testthat"
)

to_install <- pkgs[!vapply(pkgs, requireNamespace, logical(1), quietly = TRUE)]
cat("Already present:", paste(setdiff(pkgs, to_install), collapse = ", "), "\n")
cat("To install:", paste(to_install, collapse = ", "), "\n\n")

if (length(to_install)) {
  install.packages(to_install, lib = lib, type = "binary", dependencies = TRUE)
}

# Verify
cat("\n--- Verification ---\n")
ok <- TRUE
for (p in pkgs) {
  v <- tryCatch(as.character(packageVersion(p)), error = function(e) NA_character_)
  if (is.na(v)) { ok <- FALSE; cat(sprintf("MISSING  %s\n", p)) }
  else cat(sprintf("ok  %-16s %s\n", p, v))
}
cat(if (ok) "\nALL DEPENDENCIES INSTALLED\n" else "\nSOME DEPENDENCIES MISSING\n")
