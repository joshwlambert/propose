#' Generate [bslib::accordion()] with inputs for parameterising
#' `incubation_period` argument in [ringbp::delay_opts()]
#'
#' @param ns A namespace created with [shiny::NS()].
#' @param ... [dots] Not used, will throw a warning if arguments are supplied.
#'
#' @return A [bslib::accordion()] object.
#' @keywords internal
incubation_input <- function(ns, ...) {
  chkDots(...)
  accordion(
    accordion_panel(
      title = "Incubation period distribution parameters:",
      icon = bs_icon("hourglass-split"),
      selectInput(
        inputId = ns("incubation_distribution"),
        label = "Incubation Period Distribution",
        choices = list(
          "Lognormal" = "lnorm",
          "Gamma" = "gamma",
          "Weibull" = "weibull"
        )
      ),
      conditionalPanel(
        condition = "input.incubation_distribution == 'lnorm'",
        numericInput(
          ns("incubation_meanlog"),
          "Incubation period meanlog:",
          value = PROPOSE_DEFAULTS$disease_x$incubation_meanlog
        ),
        numericInput(
          ns("incubation_sdlog"),
          "Incubation period sdlog:",
          value = PROPOSE_DEFAULTS$disease_x$incubation_sdlog
        ),
        ns = ns
      ),
      conditionalPanel(
        condition = "input.incubation_distribution == 'gamma'",
        numericInput(ns("incubation_shape"), "Incubation period shape:", value = 2),
        numericInput(ns("incubation_scale"), "Incubation period scale:", value = 1),
        ns = ns
      ),
      conditionalPanel(
        condition = "input.incubation_distribution == 'weibull'",
        numericInput(ns("incubation_shape"), "Incubation period shape:", value = 2),
        numericInput(ns("incubation_scale"), "Incubation period scale:", value = 1),
        ns = ns
      )
    ),
    open = FALSE
  )
}
