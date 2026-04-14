#' Generate [bslib::accordion()] with inputs for parameterising the
#' `symptomatic_traced` argument in [ringbp::event_prob_opts()]
#'
#' @param ns A namespace created with [shiny::NS()].
#' @param ... [dots] Not used, will throw a warning if arguments are supplied.
#'
#' @return A [bslib::accordion()] object.
#' @keywords internal
contact_tracing_input <- function(ns, ...) {
  accordion(
    accordion_panel(
      title = "Contact tracing:",
      icon = bs_icon("people-fill"),
      numericInput(
        ns("symptomatic_traced"),
        "Probability of contact traced:",
        value = PROPOSE_DEFAULTS$symptomatic_traced
      )
    ),
    open = FALSE
  )
}
