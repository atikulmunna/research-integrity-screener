#' Run the Shiny Application
#'
#' @param ... arguments to pass to golem_opts.
#' See `?golem::get_golem_options` for more details.
#' @inheritParams shiny::shinyApp
#'
#' @export
#' @importFrom shiny shinyApp
run_app <- function(
  onStart = NULL,
  options = list(),
  enableBookmarking = NULL,
  uiPattern = "/",
  ...
) {
  # Plain shinyApp (no golem::with_golem_options) so the app has no runtime
  # dependency on golem internals — more robust across hosting environments.
  # We don't use golem_opts/get_golem_options anywhere.
  shiny::shinyApp(
    ui = app_ui,
    server = app_server,
    onStart = onStart,
    options = options,
    enableBookmarking = enableBookmarking,
    uiPattern = uiPattern
  )
}
