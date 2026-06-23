#' Generate [bslib::accordion()] with inputs for parameterising
#' `onset_to_isolation` argument in [ringbp::delay_opts()]
#'
#' @param ns A namespace created with [shiny::NS()].
#' @param id_prefix A `character` string prepended to every input ID generated
#' by this function (e.g. `"dct_"`). Allows the inputs to be reused several
#' times within the same module (e.g. one set per contact tracing strategy)
#' without clashing. Defaults to `""` (no prefix).
#' @param ... [dots] Not used, will throw a warning if arguments are supplied.
#'
#' @return A [bslib::accordion()] object.
#' @keywords internal
onset_to_isolation_input <- function(ns, id_prefix = "", ...) {
  chkDots(...)
  # Helper to build prefixed, namespaced input IDs
  iid <- function(suffix) ns(paste0(id_prefix, suffix))
  # Condition for the conditionalPanels, referencing the prefixed distribution
  dist_is <- function(value) {
    sprintf("input.%sonset_to_isolation_distribution == '%s'", id_prefix, value)
  }
  accordion(
    accordion_panel(
      title = "Onset-to-isolation distribution parameters:",
      icon = bs_icon("hourglass-split"),
      selectInput(
        inputId = iid("onset_to_isolation_distribution"),
        label = tagList(
          "Onset-to-isolation Distribution",
          tooltip(
            bs_icon("info-circle"),
            "The onset-to-isolation delay is the time between symptom onset
              and being isolated. Select the parametric distribution used to
              model this delay. Lognormal, Gamma, and Weibull are all
              right-skewed distributions commonly used to model this delay."
          )
        ),
        choices = list(
          "Lognormal" = "lnorm",
          "Gamma" = "gamma",
          "Weibull" = "weibull"
        )
      ),
      conditionalPanel(
        condition = dist_is("lnorm"),
        numericInput(
          iid("onset_to_isolation_meanlog"),
          label = tagList(
            "Onset-to-isolation meanlog",
            tooltip(
              bs_icon("info-circle"),
              "The mean of the onset-to-isolation delay on the natural-log scale
              (a parameter of the Lognormal distribution). Note that the mean
              of the onset-to-isolation delay itself is
              exp(meanlog + sdlog^2 / 2), not exp(meanlog)."
            )
          ),
          value = PROPOSE_DEFAULTS$onset_to_isolation_meanlog
        ),
        numericInput(
          iid("onset_to_isolation_sdlog"),
          label = tagList(
            "Onset-to-isolation sdlog",
            tooltip(
              bs_icon("info-circle"),
              "The standard deviation of the onset-to-isolation delay on the
              natural-log scale (a parameter of the Lognormal distribution).
              Larger values produce a more dispersed distribution with a
              heavier right tail."
            )
          ),
          value = PROPOSE_DEFAULTS$onset_to_isolation_sdlog
        ),
        ns = ns
      ),
      conditionalPanel(
        condition = dist_is("gamma"),
        numericInput(
          iid("onset_to_isolation_shape"),
          label = tagList(
            "Onset-to-isolation shape",
            tooltip(
              bs_icon("info-circle"),
              shape_tip("Gamma", "onset-to-isolation delay")
            )
          ),
          value = 2
        ),
        numericInput(
          iid("onset_to_isolation_scale"),
          label = tagList(
            "Onset-to-isolation scale",
            tooltip(
              bs_icon("info-circle"),
              scale_tip("Gamma", "onset-to-isolation delay")
            )
          ),
          value = 1),
        ns = ns
      ),
      conditionalPanel(
        condition = dist_is("weibull"),
        numericInput(
          iid("onset_to_isolation_shape"),
          label = tagList(
            "Onset-to-isolation shape",
            tooltip(
              bs_icon("info-circle"),
              shape_tip("Weibull", "onset-to-isolation delay")
            )
          ),
          value = 2
        ),
        numericInput(
          iid("onset_to_isolation_scale"),
          label = tagList(
            "Onset-to-isolation scale",
            tooltip(
              bs_icon("info-circle"),
              scale_tip("Weibull", "onset-to-isolation delay")
            )
          ),
          value = 1
        ),
        ns = ns
      )
    ),
    open = FALSE
  )
}
