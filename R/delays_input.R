#' Generate [bslib::accordion()] with inputs for parameterising
#' `incubation_period` and `onset_to_isolation` arguments in
#' [ringbp::delay_opts()]
#'
#' @param ns A namespace created with [shiny::NS()].
#' @param ... [dots] Not used, will throw a warning if arguments are supplied.
#'
#' @return A [bslib::accordion()] object.
#' @keywords internal
delays_input <- function(ns, ...) {
  chkDots(...)
  accordion(
    accordion_panel(
      title = "Delay distribution parameters:",
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
          value = PROPOSE_DEFAULTS$incubation_meanlog
        ),
        numericInput(
          ns("incubation_sdlog"),
          "Incubation period sdlog:",
          value = PROPOSE_DEFAULTS$incubation_sdlog
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
      ),
      selectInput(
        inputId = ns("onset_to_isolation_distribution"),
        label = "Onset-to-isolation Distribution",
        choices = list(
          "Lognormal" = "lnorm",
          "Gamma" = "gamma",
          "Weibull" = "weibull"
        )
      ),
      conditionalPanel(
        condition = "input.onset_to_isolation_distribution == 'lnorm'",
        numericInput(
          ns("onset_to_isolation_meanlog"),
          "Onset-to-isolation meanlog:",
          value = PROPOSE_DEFAULTS$onset_to_isolation_meanlog
        ),
        numericInput(
          ns("onset_to_isolation_sdlog"),
          "Onset-to-isolation sdlog:",
          value = PROPOSE_DEFAULTS$onset_to_isolation_sdlog
        ),
        ns = ns
      ),
      conditionalPanel(
        condition = "input.onset_to_isolation_distribution == 'gamma'",
        numericInput(ns("onset_to_isolation_shape"), "Onset-to-isolation shape:", value = 2),
        numericInput(ns("onset_to_isolation_scale"), "Onset-to-isolation scale:", value = 1),
        ns = ns
      ),
      conditionalPanel(
        condition = "input.onset_to_isolation_distribution == 'weibull'",
        numericInput(ns("onset_to_isolation_shape"), "Onset-to-isolation shape:", value = 2),
        numericInput(ns("onset_to_isolation_scale"), "Onset-to-isolation scale:", value = 1),
        ns = ns
      )
    ),
    open = FALSE
  )
}
