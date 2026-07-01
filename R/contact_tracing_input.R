#' Generate [bslib::accordion()] with `From`/`To`/`By` numeric inputs that
#' parameterise a [seq()] of contact tracing probabilities
#' (`symptomatic_traced` argument in [ringbp::event_prob_opts()]) to sweep over
#'
#' @param ns A namespace created with [shiny::NS()].
#' @param from,to,by `numeric` percentages (between 0 and 100) seeding the
#'   `From`/`To`/`By` inputs. The consuming module divides the swept values by
#'   100 to obtain proportions before passing them to the model.
#' @param ... [dots] Not used, will throw a warning if arguments are supplied.
#'
#' @return A [bslib::accordion()] object.
#' @keywords internal
contact_tracing_seq_input <- function(ns, from, to, by, ...) {
  chkDots(...)
  seq_tip <- "The percentage of contacts of a symptomatic infectious individual
    that are successfully traced, on average. The 'From', 'To', and 'By' inputs
    parameterise a seq() of percentages (between 0 and 100) over which the
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
        max = 100
      ),
      numericInput(
        ns("symptomatic_traced_to"),
        label = tagList(
          "To",
          tooltip(bs_icon("info-circle"), seq_tip)
        ),
        value = to,
        min = 0,
        max = 100
      ),
      numericInput(
        ns("symptomatic_traced_by"),
        label = tagList(
          "By",
          tooltip(bs_icon("info-circle"), seq_tip)
        ),
        value = by,
        min = 0,
        max = 100
      )
    ),
    open = TRUE
  )
}

#' Register input-validation feedback for the `symptomatic_traced` contact
#' tracing input rendered by [intervention_input()]
#'
#' @param input The Shiny `input` reactive of the calling module.
#'
#' @return Invisible `NULL`; called for side-effects.
#' @keywords internal
contact_tracing_feedback_server <- function(input) {
  observeEvent(input$symptomatic_traced, {
    req(!is.na(input$symptomatic_traced))
    if (input$symptomatic_traced < 0 || input$symptomatic_traced > 100) {
      showFeedbackDanger(
        "symptomatic_traced",
        text = "Error: Percentage of symptomatic contacts traced must be between 0 and 100."
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
    if (input$symptomatic_traced_from < 0 || input$symptomatic_traced_from > 100) {
      showFeedbackDanger(
        "symptomatic_traced_from",
        text = "Error: 'From' must be between 0 and 100."
      )
    } else {
      hideFeedback("symptomatic_traced_from")
    }
  })
  observeEvent(input$symptomatic_traced_to, {
    req(!is.na(input$symptomatic_traced_to))
    if (input$symptomatic_traced_to < 0 || input$symptomatic_traced_to > 100) {
      showFeedbackDanger(
        "symptomatic_traced_to",
        text = "Error: 'To' must be between 0 and 100."
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
    if (input$symptomatic_traced_by <= 0 || input$symptomatic_traced_by > 100) {
      showFeedbackDanger(
        "symptomatic_traced_by",
        text = "Error: 'By' must be greater than 0 and at most 100."
      )
    } else {
      hideFeedback("symptomatic_traced_by")
    }
  })
  invisible(NULL)
}
