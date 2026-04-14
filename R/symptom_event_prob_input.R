#' Generate [bslib::accordion()] with inputs for parameterising `asymptomatic`
#' and `presymptomatic_transmission` arguments in [ringbp::event_prob_opts()]
#'
#' @param ns A namespace created with [shiny::NS()].
#' @param ... [dots] Not used, will throw a warning if arguments are supplied.
#'
#' @return A [bslib::accordion()] object.
#' @keywords internal
symptom_event_prob_input <- function(ns, ...) {
  accordion(
    accordion_panel(
      title = "Symptom event probabilities:",
      icon = bs_icon("person-fill-gear"),
      numericInput(
        ns("asymptomatic"),
        "Probability asymptomatic:",
        value = PROPOSE_DEFAULTS$disease_x$asymptomatic
      ),
      numericInput(
        ns("presymptomatic_transmission"),
        "Probability of presymptomatic transmission:",
        value = PROPOSE_DEFAULTS$disease_x$presymptomatic_transmission
      )
    ),
    open = FALSE
  )
}
