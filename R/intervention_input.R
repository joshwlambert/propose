#' Generate [bslib::accordion()] with input for parameterising the
#' `quarantine` argument in [ringbp::intervention_opts()]
#'
#' @param ns A namespace created with [shiny::NS()].
#' @param ... [dots] Not used, will throw a warning if arguments are supplied.
#'
#' @return A [bslib::accordion()] object.
#' @keywords internal
intervention_input <- function(ns, defaults = PROPOSE_DEFAULTS, ...) {
  accordion(
    accordion_panel(
      title = "Interventions:",
      icon = bs_icon("shield-shaded"),
      checkboxInput(
        ns("quarantine"),
        "Quarantine",
        value = PROPOSE_DEFAULTS$quarantine
      )
    ),
    open = FALSE
  )
}
