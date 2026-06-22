# Applicability heuristic (UX-08 / UX-08a). Advisory only - it never gates or
# disables anything; it just surfaces a soft banner when a paper looks like it
# may fall outside the types RIS is designed for (§4.2).

#' Keywords that suggest a paper may be out of scope. Single place to tune
#' (UX-08a). Multi-word phrases are used deliberately to limit false positives:
#' e.g. "convolutional neural network" rather than "neural network", which would
#' wrongly flag legitimate neuroscience ("neural correlates", "network analysis").
#' @noRd
out_of_scope_keywords <- function() {
  c(
    "deep learning",
    "convolutional neural network",
    "reinforcement learning",
    "thematic analysis",
    "grounded theory",
    "ethnographic",
    "ethnography",
    "qualitative interview"
  )
}

#' Run the scope heuristic over extracted text.
#' @return list(out_of_scope = logical, matched = character vector of hits).
#' @noRd
applicability_check <- function(text) {
  hay <- tolower(paste(text, collapse = " "))
  kw <- out_of_scope_keywords()
  hits <- kw[vapply(kw, function(k) grepl(k, hay, fixed = TRUE), logical(1))]
  list(out_of_scope = length(hits) > 0, matched = unname(hits))
}

#' Build the soft, dismissible applicability banner, or NULL if in scope.
#' @noRd
applicability_banner <- function(ac) {
  if (!isTRUE(ac$out_of_scope)) return(NULL)
  div(
    class = "alert alert-info alert-dismissible", role = "alert",
    tags$strong("Possible scope limitation. "),
    "This paper may not contain the types of statistics these tests require. ",
    "Results may be limited.",
    tags$div(class = "small mt-1",
             sprintf("Matched terms: %s", paste(ac$matched, collapse = ", "))),
    tags$button(type = "button", class = "btn-close",
                `data-bs-dismiss` = "alert", `aria-label` = "Close")
  )
}
