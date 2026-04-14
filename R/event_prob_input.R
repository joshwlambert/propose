#' Generate [bslib::accordion()] with inputs for parameterising `asymptomatic`,
#' `presymptomatic_transmission` and `symptomatic_traced` arguments in
#' [ringbp::event_prob_opts()]
#'
#' @param ns A namespace created with [shiny::NS()].
#' @param ... [dots] Not used, will throw a warning if arguments are supplied.
#'
#' @return A [bslib::accordion()] object.
#' @keywords internal
event_prob_input <- function(ns, defaults = PROPOSE_DEFAULTS, ...) {
  accordion(
    accordion_panel(
      title = "Event probabilities:",
      icon = bs_icon("person-fill-gear"),
      numericInput(
        ns("asymptomatic"),
        "Probability asymptomatic:",
        value = PROPOSE_DEFAULTS$asymptomatic
      ),
      numericInput(
        ns("presymptomatic_transmission"),
        "Probability of presymptomatic transmission:",
        value = PROPOSE_DEFAULTS$presymptomatic_transmission
      ),
      numericInput(
        ns("symptomatic_traced"),
        "Probability of contact traced:",
        value = PROPOSE_DEFAULTS$symptomatic_traced
      )
    ),
    open = FALSE
  )
}
