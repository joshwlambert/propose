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
        label = tagList(
          "Probability of contact traced",
          tooltip(
            bs_icon("info-circle"),
            "Probability that a contact of a symptomatic infectious
            individual is successfully traced. Traced individuals are isolated
            at the earliest of the time their infector is isolated or until
            they get a positive test result and isolate themselves, whichever
            comes first."
          )
        ),
        value = PROPOSE_DEFAULTS$symptomatic_traced
      )
    ),
    open = FALSE
  )
}
