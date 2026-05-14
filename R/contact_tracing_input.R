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

#' Generate [bslib::accordion()] with `From`/`To`/`By` numeric inputs that
#' parameterise a [seq()] of contact tracing probabilities
#' (`symptomatic_traced` argument in [ringbp::event_prob_opts()]) to sweep over
#'
#' @param ns A namespace created with [shiny::NS()].
#' @param from,to,by `numeric` values input into [seq()].
#' @param ... [dots] Not used, will throw a warning if arguments are supplied.
#'
#' @return A [bslib::accordion()] object.
#' @keywords internal
contact_tracing_seq_input <- function(ns, from, to, by, ...) {
  chkDots(...)
  seq_tip <- "Probability that a contact of a symptomatic infectious
    individual is successfully traced. The 'From', 'To', and 'By' inputs
    parameterise a seq() of probabilities (between 0 and 1) over which the
    outbreak simulation is repeated."
  accordion(
    accordion_panel(
      title = "Contact tracing sweep:",
      icon = bs_icon("people-fill"),
      numericInput(
        ns("symptomatic_traced_from"),
        label = tagList(
          "From",
          tooltip(bs_icon("info-circle"), seq_tip)
        ),
        value = from,
        min = 0,
        max = 1
      ),
      numericInput(
        ns("symptomatic_traced_to"),
        label = tagList(
          "To",
          tooltip(bs_icon("info-circle"), seq_tip)
        ),
        value = to,
        min = 0,
        max = 1
      ),
      numericInput(
        ns("symptomatic_traced_by"),
        label = tagList(
          "By",
          tooltip(bs_icon("info-circle"), seq_tip)
        ),
        value = by,
        min = 0,
        max = 1
      )
    ),
    open = TRUE
  )
}
