#' Generate [bslib::accordion()] with input for parameterising the
#' `quarantine` argument in [ringbp::intervention_opts()]
#'
#' @param ns A namespace created with [shiny::NS()].
#' @param ... [dots] Not used, will throw a warning if arguments are supplied.
#'
#' @return A [bslib::accordion()] object.
#' @keywords internal
intervention_input <- function(ns, ...) {
  accordion(
    accordion_panel(
      title = "Interventions:",
      icon = bs_icon("shield-shaded"),
      checkboxInput(
        ns("quarantine"),
        label = tagList(
          "Quarantine",
          tooltip(
            bs_icon("info-circle"),
            "When quarantine is enabled, traced contacts are isolated as soon
            as they are identified, regardless of whether they are symptomatic
            (presymptomatic isolation). When disabled, traced contacts are only
            isolated once they show symptoms, allowing for transmission during
            the presymptomatic phase."
          )
        ),
        value = PROPOSE_DEFAULTS$quarantine
      )
    ),
    open = FALSE
  )
}
