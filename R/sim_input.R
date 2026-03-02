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
      title = "Simulation controls: ",
      icon = bs_icon("gear-wide-connected"),
      numericInput(ns("cap_max_days"), "Maximum number of days:", value = 100),
      numericInput(ns("cap_cases"), "Maximum number of cases:", value = 5000),
      numericInput(ns("seed"), "Seed for simulation model", value = NA_integer_)
    ),
    open = FALSE
  )
}
