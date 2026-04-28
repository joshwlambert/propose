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

  # tooltip messages for incubation period distribution parameters
  shape_tip <- function(distribution) {
    paste0(
      "Shape parameter of the ", distribution, " distribution for the
              incubation period."
    )
  }
  scale_tip <- function(distribution) {
    paste0(
      "Scale parameter of the ", distribution, " distribution for the
              incubation period. Stretches the distribution along the time
              axis; larger values produce longer incubation periods."
    )
  }

  accordion(
    accordion_panel(
      title = "Incubation period distribution parameters:",
      icon = bs_icon("hourglass-split"),
      selectInput(
        inputId = ns("incubation_distribution"),
        label = tagList(
          "Incubation Period Distribution",
          tooltip(
            bs_icon("info-circle"),
            "The incubation period is the time between infection
              and symptom onset. Select the parametric distribution used to
              model this delay. Lognormal, Gamma, and Weibull are all
              right-skewed distributions commonly used for incubation periods."
          )
        ),
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
          label = tagList(
            "Incubation period meanlog",
            tooltip(
              bs_icon("info-circle"),
              "The mean of the incubation period on the natural-log scale
              (a parameter of the Lognormal distribution). Note that the mean
              of the incubation period itself is exp(meanlog + sdlog^2 / 2),
              not exp(meanlog)."
            )
          ),
          value = PROPOSE_DEFAULTS$disease_x$incubation_meanlog
        ),
        numericInput(
          ns("incubation_sdlog"),
          label = tagList(
            "Incubation period sdlog",
            tooltip(
              bs_icon("info-circle"),
              "The standard deviation of the incubation period on the
              natural-log scale (a parameter of the Lognormal distribution).
              Larger values produce a more dispersed distribution with a
              heavier right tail."
            )
          ),
          value = PROPOSE_DEFAULTS$disease_x$incubation_sdlog
        ),
        ns = ns
      ),
      conditionalPanel(
        condition = "input.incubation_distribution == 'gamma'",
        numericInput(
          ns("incubation_shape"),
          label = tagList(
            "Incubation period shape",
            tooltip(bs_icon("info-circle"), shape_tip("Gamma"))
          ),
          value = 2
        ),
        numericInput(
          ns("incubation_scale"),
          label = tagList(
            "Incubation period scale",
            tooltip(bs_icon("info-circle"), scale_tip("Gamma"))
          ),
          value = 1
        ),
        ns = ns
      ),
      conditionalPanel(
        condition = "input.incubation_distribution == 'weibull'",
        numericInput(
          ns("incubation_shape"),
          label = tagList(
            "Incubation period shape",
            tooltip(bs_icon("info-circle"), shape_tip("Weibull"))
          ),
          value = 2
        ),
        numericInput(
          ns("incubation_scale"),
          label = tagList(
            "Incubation period scale",
            tooltip(bs_icon("info-circle"), scale_tip("Weibull"))
          ),
          value = 1
        ),
        ns = ns
      )
    ),
    open = FALSE
  )
}
