# Convenience runner: load the package from source and run the full test suite.
# Run from the project root:
#   Rscript dev/run_tests.R
# Uses devtools::test() so no install step is needed during development.

.libPaths(c(file.path(Sys.getenv("LOCALAPPDATA"), "R", "win-library", "4.6"), .libPaths()))

devtools::test(".")
