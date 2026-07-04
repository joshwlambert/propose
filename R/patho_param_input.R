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
      choices = list(
        "Disease X" = "disease_x",
        "COVID-19 (Wild-type)" = "covid_19_wt",
        "COVID-19 (Alpha)" = "covid_19_alpha",
        "COVID-19 (Delta)" = "covid_19_delta",
        "COVID-19 (Omicron)" = "covid_19_omicron",
        "SARS" = "sars",
        "MERS" = "mers",
        "Ebola (Zaire)" = "ebola_zaire",
        "Ebola (Sudan)" = "ebola_sudan",
        "Marburg" = "marburg",
        "Influenza (H5N1)" = "influenza_h5n1",
        "Influenza (H1N1pdm)" = "influenza_h1n1pdm",
        "Influenza (H7N9)" = "influenza_h7n9",
        "Meningitis B" = "meningitis_b",
        "Andes (Hanta)virus" = "andes_hantavirus"
      )
    )
  )
}