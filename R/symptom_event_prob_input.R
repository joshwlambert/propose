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
        label = tagList(
          "Probability asymptomatic:",
          tooltip(
            bs_icon("info-circle"),
            "Probability that an infected individual shows no symptoms
            (asymptomatic) throughout their entire infectious period. 0 causes
            everyone to develop symptoms during their infection. Conversely,
            1 causes no one to develop symptoms. Asymptomatic individuals
            never develop symptoms and so are never isolated, and their
            contacts are never traced."
          )
        ),
        value = PROPOSE_DEFAULTS$disease_x$asymptomatic
      ),
      numericInput(
        ns("presymptomatic_transmission"),
        label = tagList(
          "Probability of presymptomatic transmission:",
          tooltip(
            bs_icon("info-circle"),
            "The proportion of all transmission events that occur before the
            infector's symptom onset. Higher values mean more transmission
            happens during the presymptomatic phase, before isolation can be
            triggered by symptoms."
          )
        ),
        value = PROPOSE_DEFAULTS$disease_x$presymptomatic_transmission
      )
    ),
    open = FALSE
  )
}
