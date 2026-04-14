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
sim_input <- function(ns, defaults = PROPOSE_DEFAULTS, ...) {
  accordion(
    accordion_panel(
      title = "Simulation controls: ",
      icon = bs_icon("gear-wide-connected"),
      numericInput(
        ns("cap_max_days"),
        "Maximum number of days:",
        value = PROPOSE_DEFAULTS$cap_max_days
      ),
      numericInput(
        ns("cap_cases"),
        "Maximum number of cases:",
        value = PROPOSE_DEFAULTS$cap_cases
      ),
      numericInput(ns("seed"), "Seed for simulation model", value = NA_integer_)
    ),
    open = FALSE
  )
}

#' Generate [bslib::card()] with input for number of simulation replicates
#'
#' @param ns A namespace created with [shiny::NS()].
#' @param ... [dots] Not used, will throw a warning if arguments are supplied.
#'
#' @return A [bslib::card()] object.
#' @keywords internal
replicates_input <- function(ns, defaults = PROPOSE_DEFAULTS, ...) {
  card(
    card_header(
      "Number of simulation replicates:",
      tooltip(
        bsicons::bs_icon("info-circle"),
        "This controls the number of independent outbreaks to simulate.",
        id = "tooltip",
      )
    ),
    sliderInput(
      ns("replicates"),
      label = "",
      value = PROPOSE_DEFAULTS$replicates, min = 1, max = 100
    )
  )
}

#' Generate [bslib::card()] with input for number of initial cases in the
#' simulation
#'
#' @param ns A namespace created with [shiny::NS()].
#' @param ... [dots] Not used, will throw a warning if arguments are supplied.
#'
#' @return A [bslib::card()] object.
#' @keywords internal
initial_cases_input <- function(ns, defaults = PROPOSE_DEFAULTS, ...) {
  card(
    card_header(
      "Number of initial cases:",
      tooltip(
        bsicons::bs_icon("info-circle"),
        "This controls the number of initially infectious individuals.
              Each individual seeds an independent outbreak.",
        id = "tooltip",
      )
    ),
    sliderInput(
      ns("initial_cases"),
      label = "",
      value = PROPOSE_DEFAULTS$initial_cases, min = 1, max = 100
    )
  )
}