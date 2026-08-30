#' Generate [bslib::accordion()] with inputs for parameterising the simulation
#' controls.
#'
#' @description
#' Including parameterising:
#'   * `cap_max_days` and `cap_cases` arguments in
#'   [ringbp::sim_opts()]
#'   * The seed used in the simulation (see [set.seed()]). By default the seed
#'   is set to `NA` which is interpreted in the server as choosing a random
#'   seed for each time the simulation is run. A single `numeric` can be set
#'   to specify the seed.
#'
#' @param ns A namespace created with [shiny::NS()].
#' @param ... [dots] Not used, will throw a warning if arguments are supplied.
#'
#' @return A [bslib::accordion()] object.
#' @keywords internal
sim_input <- function(ns, ...) {
  accordion(
    accordion_panel(
      title = "Simulation controls",
      icon = bs_icon("gear-wide-connected"),
      numericInput(
        ns("cap_max_days"),
        label = tagList(
          "Maximum number of days",
          tooltip(
            bs_icon("info-circle"),
            "The maximum number of days to run the outbreak simulation. This
            cap prevents uncontrolled outbreaks from running for an excessively
            long time."
          )
        ),
        value = PROPOSE_DEFAULTS$cap_max_days
      ),
      numericInput(
        ns("cap_cases"),
        label = tagList(
          "Maximum number of cases",
          tooltip(
            bs_icon("info-circle"),
            "The maximum number of cumulative cases before stopping the
            outbreak simulation. This cap prevents uncontrolled outbreaks from
            running for an excessively long time."
          )
        ),
        value = PROPOSE_DEFAULTS$cap_cases
      ),
      numericInput(
        ns("seed"),
        label = tagList(
          "Seed for simulation model",
          tooltip(
            bs_icon("info-circle"),
            "A seed to control the random number generating sequence in the
            stochastic simulation. By default the simulation uses a random
            seed so each simulation produces a different output, even with the
            same model parameters. Setting the seed to an integer will produce
            the same output when the same model parameters are used."
          )
        ),
        value = NA_integer_)
      ),
    open = FALSE
  )
}

#' Register input-validation feedback for [sim_input()]
#'
#' @param input The Shiny `input` reactive of the calling module.
#'
#' @return Invisible `NULL`; called for side-effects.
#' @keywords internal
sim_feedback_server <- function(input) {
  observeEvent(input$cap_max_days, {
    req(!is.na(input$cap_max_days))
    if (input$cap_max_days < 1) {
      showFeedbackDanger(
        "cap_max_days",
        text = "Error: The maximum number of days in the simulation must be at least 1."
      )
    } else {
      hideFeedback("cap_max_days")
    }
  })
  observeEvent(input$cap_cases, {
    req(!is.na(input$cap_cases))
    if (input$cap_cases < 1) {
      showFeedbackDanger(
        "cap_cases",
        text = "Error: The maximum number of cases in the simulation must be at least 1."
      )
    } else {
      hideFeedback("cap_cases")
    }
  })
  invisible(NULL)
}

#' Generate an input for the number of simulation replicates
#'
#' @description
#' Two variants of the same input, so that pages needing more replicates than a
#' slider can comfortably offer are not forced to hand-write their own control:
#'
#' * a [shiny::sliderInput()] in a [bslib::card()], capped at 100, which suits
#'   pages that simulate a single scenario and default to a handful of
#'   replicates; and
#' * a bare [shiny::numericInput()] with no upper bound, for pages that simulate
#'   many scenarios at once and need enough replicates for the confidence
#'   intervals to separate them.
#'
#' The starting value is looked up by page rather than passed in, so a page's
#' default lives with the others in `PROPOSE_DEFAULTS$replicates` instead of
#' being written into the page that uses it. A page's "Reset Defaults" reads the
#' same entry, so the two cannot disagree.
#'
#' @param ns A namespace created with [shiny::NS()].
#' @param page A `character` string naming the page, used to look its default
#'   number of replicates up in `PROPOSE_DEFAULTS$replicates`. One of
#'   `"explore"`, `"compare"`, `"tracing_effectiveness"`, `"tracing_strategies"`
#'   or `"outbreak_size"`. A name with no entry yields an input with no starting
#'   value, which is visible the moment the page is opened.
#' @param numeric A `logical` scalar. When `TRUE` the numeric variant is
#'   returned, with no card wrapper and no upper bound. Defaults to `FALSE`.
#' @param tip A `character` string of tooltip text, for pages that need to say
#'   something more than the default about what replicates buy them. Defaults to
#'   `NULL`, which uses the shared wording.
#' @param ... [dots] Not used, will throw a warning if arguments are supplied.
#'
#' @return A [bslib::card()] object, or a [shiny::numericInput()] when
#'   `numeric = TRUE`.
#' @keywords internal
replicates_input <- function(ns, page, numeric = FALSE, tip = NULL, ...) {
  chkDots(...)
  value <- PROPOSE_DEFAULTS$replicates[[page]]
  if (is.null(tip)) {
    tip <- "This controls the number of independent outbreaks to simulate."
  }

  if (numeric) {
    return(
      numericInput(
        ns("replicates"),
        label = tagList(
          "Number of simulation replicates",
          tooltip(bsicons::bs_icon("info-circle"), tip)
        ),
        value = value,
        min = 1
      )
    )
  }

  card(
    card_header(
      "Number of simulation replicates",
      tooltip(bsicons::bs_icon("info-circle"), tip, id = "tooltip")
    ),
    sliderInput(
      ns("replicates"),
      label = "",
      value = value, min = 1, max = 100
    )
  )
}

#' Generate [bslib::card()] with input for number of initial cases in the
#' simulation
#'
#' @param ns A namespace created with [shiny::NS()].
#' @param value The initial value of [shiny::sliderInput()]. Defaults to
#'   `PROPOSE_DEFAULTS$initial_cases`.
#' @param ... [dots] Not used, will throw a warning if arguments are supplied.
#'
#' @return A [bslib::card()] object.
#' @keywords internal
initial_cases_input <- function(ns, value = PROPOSE_DEFAULTS$initial_cases, ...) {
  card(
    card_header(
      "Number of initial cases",
      tooltip(
        bsicons::bs_icon("info-circle"),
        "Number of initially infectious individuals at the start of each
        simulated outbreak. They all seed independent transmission chains
        within the same simulation run.",
        id = "tooltip"
      )
    ),
    sliderInput(
      ns("initial_cases"),
      label = "",
      value = value, min = 1, max = 100
    )
  )
}