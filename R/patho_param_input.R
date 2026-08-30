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
        "Pathogen parameters based on estimates published in the literature.
        The default scenario is 'Disease X' and has generic pathogen
        parameters.",
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