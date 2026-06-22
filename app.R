# Entry point for hosted deployment (shinyapps.io / Posit Connect / Shiny Server).
# Loads the RIS package from the bundled source and launches the app.

pkgload::load_all(export_all = FALSE, helpers = FALSE, attach_testthat = FALSE)
options("golem.app.prod" = TRUE)
run_app()
