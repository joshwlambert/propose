#' Generate [bslib::accordion()] with inputs for parameterising `asymptomatic`,
#' `presymptomatic_transmission` and `symptomatic_ascertained` arguments in
#' [ringbp::event_prob_opts()]
#'
#' @param ns A namespace created with [shiny::NS()].
#' @param ... [dots] Not used, will throw a warning if arguments are supplied.
#'
#' @return A [bslib::accordion()] object.
#' @keywords internal
event_prob_input <- function(ns, ...) {
  accordion(
    accordion_panel(
      title = "Event probabilities:",
      icon = bs_icon("person-fill-gear"),
      numericInput(ns("asymptomatic"), "Probability asymptomatic:", value = 0.1),
      numericInput(ns("presymptomatic_transmission"), "Probability of presymptomatic transmission:", value = 0.1),
      numericInput(ns("symptomatic_ascertained"), "Probability of contact traced:", value = 0.8)
    ),
    open = FALSE
  )
}
