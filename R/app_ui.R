#' The application User-Interface
#'
#' @param request Internal parameter for `{shiny}`.
#'     DO NOT REMOVE.
#' @import shiny
#' @noRd
app_ui <- function(request) {
  tagList(
    # Leave this function for adding external resources
    golem_add_external_resources(),
    shinyjs::useShinyjs(),
    tags$head(tags$style(htmltools::HTML(
      ".ris-tab-disabled{pointer-events:none;opacity:.45;}"
    ))),
    # Five-tab layout (UX-01). Auto-extract tabs are gated until upload (UX-02);
    # GRIM/SPRITE is always available (UX-03).
    bslib::page_navbar(
      title = "Research Integrity Screener",
      id = "main_nav",
      theme = bslib::bs_theme(
        version = 5,
        bootswatch = "flatly",
        primary = "#2c7be5",
        base_font = bslib::font_collection("system-ui", "Segoe UI", "Helvetica Neue", "Arial")
      ),
      window_title = "Research Integrity Screener",
      bslib::nav_panel(
        title = "Upload & Overview", value = "upload",
        overview_intro(),
        mod_upload_ui("upload")
      ),
      bslib::nav_panel(
        title = "statcheck", value = "statcheck",
        tags$p(class = "text-muted small",
               "Runs automatically once a PDF is uploaded and extracted."),
        mod_statcheck_ui("statcheck")
      ),
      bslib::nav_panel(
        title = "p-curve & z-curve", value = "curves",
        tags$p(class = "text-muted small",
               "Runs automatically once a PDF is uploaded and extracted."),
        mod_pcurve_ui("pcurve"),
        mod_zcurve_ui("zcurve")
      ),
      bslib::nav_panel(
        title = "GRIM / SPRITE", value = "manual",
        tags$p(class = "text-muted small", "Manual entry - no PDF required."),
        mod_grim_ui("grim"),
        mod_sprite_ui("sprite")
      ),
      bslib::nav_panel(
        title = "Report", value = "report",
        mod_report_ui("report")
      )
    )
  )
}

#' Overview/intro block shown on the first tab.
#' @noRd
overview_intro <- function() {
  tagList(
    tags$p(
      class = "lead",
      "Statistical consistency checks for quantitative research manuscripts."
    ),
    tags$p(
      class = "text-muted",
      # NFR-08 privacy notice
      "Uploaded documents are processed in-memory and are not stored or transmitted."
    ),
    div(
      class = "mb-3",
      tier_badge("Core"), " statcheck, GRIM, SPRITE - reliable on a single paper. ",
      tier_badge("Exploratory"), " p-curve - a screening heuristic only. ",
      tier_badge("Meta-analysis"), " z-curve - for literatures, usually not a single paper."
    ),
    tags$hr()
  )
}

#' Add external Resources to the Application
#'
#' This function is internally used to add external
#' resources inside the Shiny application.
#'
#' @import shiny
#' @noRd
golem_add_external_resources <- function() {
  shiny::addResourcePath("www", app_sys("app/www"))

  tags$head(
    tags$link(rel = "icon", type = "image/x-icon", href = "www/favicon.ico")
    # Add other external resources here if needed.
  )
}
