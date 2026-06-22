#' The application server-side
#'
#' @param input,output,session Internal parameters for {shiny}.
#'     DO NOT REMOVE.
#' @import shiny
#' @noRd
app_server <- function(input, output, session) {
  # Accept uploads up to 20 MB (FR-01).
  options(shiny.maxRequestSize = 20 * 1024^2)

  # Extraction result is shared with downstream modules.
  extraction <- mod_upload_server("upload")

  # Module 1: statcheck (Core tier).
  statcheck_res <- mod_statcheck_server("statcheck", extraction)

  # Module 2: p-curve (Exploratory tier) - consumes the canonical statcheck set.
  pcurve_res <- mod_pcurve_server("pcurve", statcheck_res)

  # Module 3: z-curve (Meta-analysis tier) - consumes the canonical statcheck set.
  zcurve_res <- mod_zcurve_server("zcurve", statcheck_res)

  # Module 4: GRIM (Core tier, manual entry - independent of upload).
  grim_res <- mod_grim_server("grim")

  # Module 5: SPRITE (Core tier, manual entry - independent of upload).
  sprite_res <- mod_sprite_server("sprite")

  # Unified report - aggregates all module results.
  mod_report_server("report", extraction, statcheck_res, pcurve_res,
                    zcurve_res, grim_res, sprite_res)

  # UX-02: gray out the auto-extract tabs until extraction succeeds. GRIM/SPRITE
  # (manual) and Report stay available (UX-03).
  gated_tabs <- c("statcheck", "curves")
  observe({
    ready <- tryCatch(isTRUE(extraction()$ok), error = function(e) FALSE)
    for (v in gated_tabs) {
      sel <- sprintf("a[data-value='%s']", v)
      if (ready) {
        shinyjs::removeClass(selector = sel, class = "ris-tab-disabled")
      } else {
        shinyjs::addClass(selector = sel, class = "ris-tab-disabled")
      }
    }
  })
}
