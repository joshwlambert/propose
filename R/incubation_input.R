#' Gamma dispersion (`shape`) for the basic delay-distribution UI
#'
#' @description
#' Maps the basic delay "variability" radio choices (used by
#' [incubation_input()] and [onset_to_isolation_input()]) to the shape
#' parameter of a Gamma distribution (`shape` in [stats::rgamma()]). With the
#' mean held fixed (`scale = mean / shape`), `shape` controls the skewness of
#' the tail (skewness = 2 / sqrt(shape)): `"low"` (`shape = 20`) is
#' near-symmetric, `"moderate"` (`shape = 5`) is a classic right skew, and
#' `"high"` (`shape = 2`) has a long, heavy tail.
#'
#' @keywords internal
BASIC_DELAY_SHAPE <- c(
  low = 20,
  moderate = 5,
  high = 2
)

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
  # default basic mean derived from the disease_x advanced (lnorm) defaults,
  # rounded to 1 dp for a simpler default value
  basic_mean_default <- round(
    epiparameter::convert_params_to_summary_stats(
      "lnorm",
      meanlog = PROPOSE_DEFAULTS$disease_x$incubation_meanlog,
      sdlog = PROPOSE_DEFAULTS$disease_x$incubation_sdlog
    )$mean,
    1
  )
  accordion(
    accordion_panel(
      title = "Incubation period",
      icon = bs_icon("hourglass-split"),

      # basic or advanced UI options
      radioButtons(
        inputId = ns("incubation_ui"),
        label = "User mode",
        choices = list(
          "Basic" = "basic",
          "Advanced" = "advanced"
        )
      ),

      # basic incubation UI
      conditionalPanel(
        condition = "input.incubation_ui == 'basic'",
        numericInput(
          ns("basic_incubation_mean"),
          label = tagList(
            "Average incubation period (days)",
            tooltip(
              bs_icon("info-circle"),
              "The average number of days between infection and the onset of
              symptoms."
            )
          ),
          value = basic_mean_default,
          min = 0
        ),
        radioButtons(
          inputId = ns("basic_incubation_variability"),
          label = tagList(
            "Incubation period variability",
            tooltip(
              bs_icon("info-circle"),
              "How much the incubation period varies between individuals.
              Higher variability gives a longer tail, where some people take
              much longer than the average to develop symptoms. These options
              set the shape of a Gamma distribution with the average above:
              low (near-symmetric), moderate, and high (long-tailed)."
            )
          ),
          choices = list(
            "Low variability" = "low",
            "Moderate variability" = "moderate",
            "High variability" = "high"
          ),
          selected = "moderate"
        ),
        ns = ns
      ),

      # advanced incubation UI
      conditionalPanel(
        condition = "input.incubation_ui == 'advanced'",
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
              tooltip(
                bs_icon("info-circle"),
                shape_tip("Gamma", "incubation period")
              )
            ),
            value = 2
          ),
          numericInput(
            ns("incubation_scale"),
            label = tagList(
              "Incubation period scale",
              tooltip(
                bs_icon("info-circle"),
                scale_tip("Gamma", "incubation period")
              )
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
              tooltip(
                bs_icon("info-circle"),
                shape_tip("Weibull", "incubation period")
              )
            ),
            value = 2
          ),
          numericInput(
            ns("incubation_scale"),
            label = tagList(
              "Incubation period scale",
              tooltip(
                bs_icon("info-circle"),
                scale_tip("Weibull", "incubation period")
              )
            ),
            value = 1
          ),
          ns = ns
        ),
        ns = ns
      )
    ),
    open = FALSE
  )
}
