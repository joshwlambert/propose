#' Generate [bslib::accordion()] with inputs for parameterising model
#' interventions: the `onset_to_isolation` argument in [ringbp::delay_opts()],
#' `symptomatic_traced` argument in [ringbp::event_prob_opts()], and
#' `quarantine` argument in [ringbp::intervention_opts()]
#'
#' @description
#' Renders the "Interventions" accordion. The onset-to-isolation delay
#' ([delays_input()]) and the `quarantine` checkbox (the `quarantine` argument
#' in [ringbp::intervention_opts()]) are always included. The other controls are
#' composed via flags so each page can show only the intervention inputs it
#' needs:
#'
#' * `contact_tracing = TRUE` adds the single contact tracing percentage input
#'   (the `symptomatic_traced` argument in [ringbp::event_prob_opts()]). Pages
#'   that sweep contact tracing (e.g. the Tracing Effectiveness page) leave this
#'   `FALSE` and supply their own sweep input ([contact_tracing_seq_input()]).
#' * `isolation_switch = TRUE` prepends a master [bslib::input_switch()]
#'   (`isolation_on`) and reveals the controls above (plus `quarantine`) only
#'   when it is on. When the switch is off, the consuming module overrides
#'   `onset_to_isolation` to a very large finite delay so no isolation (and hence
#'   no contact tracing) occurs.
#'
#' @param ns A namespace created with [shiny::NS()].
#' @param isolation_switch A `logical` scalar. When `TRUE` the master
#' `isolation_on` switch is shown and gates the other controls. Defaults to
#' `FALSE`.
#' @param contact_tracing A `logical` scalar. When `TRUE` the single contact
#' tracing percentage input is included. Defaults to `FALSE`.
#' @param ... [dots] Not used, will throw a warning if arguments are supplied.
#'
#' @return A [bslib::accordion()] object.
#' @keywords internal
intervention_input <- function(ns,
                               isolation_switch = FALSE,
                               contact_tracing = FALSE,
                               ...) {
  chkDots(...)
  # assemble the controls
  controls <- tagList(
    tags$b("Onset-to-isolation delay"),
    delays_input(
      ns = ns, delay_type = "onset_to_isolation", accordion = FALSE
    ),
    tags$hr()
  )
  if (contact_tracing) {
    controls <- tagList(
      controls,
      tags$b("Contact tracing"),
      numericInput(
        ns("symptomatic_traced"),
        label = tagList(
          "The percentage of contacts that are successfully traced (on average)",
          tooltip(
            bs_icon("info-circle"),
            "The percentage of contacts of a symptomatic infectious individual
            that are successfully traced, on average. Each contact is traced
            independently with this percentage as its chance, so the realised
            proportion varies between simulations. Traced individuals are
            isolated at the earliest of the time their infector is isolated or
            until they get a positive test result and isolate themselves,
            whichever comes first."
          )
        ),
        value = PROPOSE_DEFAULTS$symptomatic_traced,
        min = 0,
        max = 100
      ),
      tags$hr()
    )
  }
  controls <- tagList(
    controls,
    checkboxInput(
      ns("quarantine"),
      label = tagList(
        "Quarantine",
        tooltip(
          bs_icon("info-circle"),
          "When quarantine is enabled, traced contacts are isolated as soon
          as they are identified, regardless of whether they are symptomatic
          (presymptomatic isolation). When disabled, traced contacts are only
          isolated once they show symptoms, allowing for transmission during
          the presymptomatic phase."
        )
      ),
      value = PROPOSE_DEFAULTS$quarantine
    )
  )

  # optionally gate the controls behind the isolation switch.
  body <- if (isolation_switch) {
    tagList(
      input_switch(
        ns("isolation_on"),
        label = tagList(
          "Isolate cases",
          tooltip(
            bs_icon("info-circle"),
            "When enabled, symptomatic cases are isolated after the
            onset-to-isolation delay, and contact tracing and quarantine
            options become available. When disabled, no cases are isolated and
            therefore no intervention is active."
          )
        ),
        value = PROPOSE_DEFAULTS$isolation_on
      ),
      conditionalPanel(
        condition = "input.isolation_on == true",
        ns = ns,
        tags$hr(),
        controls
      )
    )
  } else {
    controls
  }

  accordion(
    accordion_panel(
      title = "Interventions:",
      icon = bs_icon("shield-shaded"),
      body
    ),
    open = FALSE
  )
}
