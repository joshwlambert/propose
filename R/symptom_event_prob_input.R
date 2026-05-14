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

#' Register input-validation feedback for [symptom_event_prob_input()]
#'
#' @param input The Shiny `input` reactive of the calling module.
#'
#' @return Invisible `NULL`; called for side-effects.
#' @keywords internal
symptom_event_prob_feedback_server <- function(input) {
  observeEvent(input$asymptomatic, {
    req(!is.na(input$asymptomatic))
    if (input$asymptomatic < 0 || input$asymptomatic > 1) {
      showFeedbackDanger(
        "asymptomatic",
        text = "Error: Probability of asymptomatic cases must be between 0 and 1."
      )
    } else {
      hideFeedback("asymptomatic")
    }
  })
  observeEvent(input$presymptomatic_transmission, {
    req(!is.na(input$presymptomatic_transmission))
    if (input$presymptomatic_transmission < 0 || input$presymptomatic_transmission > 1) {
      showFeedbackDanger(
        "presymptomatic_transmission",
        text = "Error: Probability of presymptomatic transmission cases must be between 0 and 1."
      )
    } else {
      hideFeedback("presymptomatic_transmission")
    }
  })
  invisible(NULL)
}
