#' Generate [bslib::card()] with a [shiny::selectInput()] for selecting the
#' pathogen parameters for the simulation
#'
#' @param ns A namespace created with [shiny::NS()].
#' @param ... [dots] Not used, will throw a warning if arguments are supplied.
#'
#' @return A [bslib::card()] object.
#' @keywords internal
patho_param_input <- function(ns, ...) {
  chkDots(...)
  card(
    class = "allow-overflow",
    card_header(
      "Select Pathogen Parameters",
      tooltip(
        bsicons::bs_icon("info-circle"),
        "Named pathogens (e.g. COVID-19, SARS, Ebola) use parameters based on
        estimates published in the literature. The default, 'Disease X', is
        not based on a specific pathogen: its parameters are sensible generic
        starting values, not a consensus or a prediction of a future
        pandemic's characteristics. See the FAQs for more.",
        id = "tooltip"
      )
    ),
    selectInput(
      ns("pathogen_defaults"),
      label = "",
      # names are what the user sees, values are the PROPOSE_DEFAULTS keys.
      # Taken from PATHOGEN_LABELS so the selector and the scenario names on the
      # Compare page cannot drift apart.
      choices = stats::setNames(names(PATHOGEN_LABELS), PATHOGEN_LABELS)
    )
  )
}