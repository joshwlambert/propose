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

#' Register input-validation feedback for [contact_tracing_input()]
#'
#' @param input The Shiny `input` reactive of the calling module.
#'
#' @return Invisible `NULL`; called for side-effects.
#' @keywords internal
contact_tracing_feedback_server <- function(input) {
  observeEvent(input$symptomatic_traced, {
    req(!is.na(input$symptomatic_traced))
    if (input$symptomatic_traced < 0 || input$symptomatic_traced > 1) {
      showFeedbackDanger(
        "symptomatic_traced",
        text = "Error: Probability of a symptomatic contact being traced must be between 0 and 1."
      )
    } else {
      hideFeedback("symptomatic_traced")
    }
  })
  invisible(NULL)
}

#' Register input-validation feedback for [contact_tracing_seq_input()]
#'
#' Validates that `From`/`To`/`By` lie in `[0, 1]` (with `By > 0`) and that
#' `From <= To`.
#'
#' @param input The Shiny `input` reactive of the calling module.
#'
#' @return Invisible `NULL`; called for side-effects.
#' @keywords internal
contact_tracing_seq_feedback_server <- function(input) {
  observeEvent(input$symptomatic_traced_from, {
    req(!is.na(input$symptomatic_traced_from))
    if (input$symptomatic_traced_from < 0 || input$symptomatic_traced_from > 1) {
      showFeedbackDanger(
        "symptomatic_traced_from",
        text = "Error: 'From' must be between 0 and 1."
      )
    } else {
      hideFeedback("symptomatic_traced_from")
    }
  })
  observeEvent(input$symptomatic_traced_to, {
    req(!is.na(input$symptomatic_traced_to))
    if (input$symptomatic_traced_to < 0 || input$symptomatic_traced_to > 1) {
      showFeedbackDanger(
        "symptomatic_traced_to",
        text = "Error: 'To' must be between 0 and 1."
      )
    } else if (!is.na(input$symptomatic_traced_from) &&
               input$symptomatic_traced_to < input$symptomatic_traced_from) {
      showFeedbackDanger(
        "symptomatic_traced_to",
        text = "Error: 'To' must be greater than or equal to 'From'."
      )
    } else {
      hideFeedback("symptomatic_traced_to")
    }
  })
  observeEvent(input$symptomatic_traced_by, {
    req(!is.na(input$symptomatic_traced_by))
    if (input$symptomatic_traced_by <= 0 || input$symptomatic_traced_by > 1) {
      showFeedbackDanger(
        "symptomatic_traced_by",
        text = "Error: 'By' must be greater than 0 and at most 1."
      )
    } else {
      hideFeedback("symptomatic_traced_by")
    }
  })
  invisible(NULL)
}
