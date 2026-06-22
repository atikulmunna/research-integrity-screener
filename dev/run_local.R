# Launch the app locally for manual testing.
#   Rscript dev/run_local.R [port]
# Then open http://127.0.0.1:<port> in a browser.

.libPaths(c(file.path(Sys.getenv("LOCALAPPDATA"), "R", "win-library", "4.6"), .libPaths()))

port <- suppressWarnings(as.integer(commandArgs(trailingOnly = TRUE)[1]))
if (is.na(port)) port <- 8100

suppressMessages(devtools::load_all(".", quiet = TRUE))
shiny::runApp(run_app(), host = "127.0.0.1", port = port, launch.browser = FALSE)
