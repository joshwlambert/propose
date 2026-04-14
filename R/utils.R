#' \pkg{ringbp} simulation defaults used in \pkg{propose}
#'
#' @name defaults
#'
#' @description
#' `PROPOSE_DEFAULTS` is a list of lists and values. Each inner list contains
#' default pathogen parameters for the outbreak simulation for a specific
#' pathogen. The non-list values in the `PROPOSE_DEFAULTS` are the non-pathogen
#' parameters for the outbreak simulation.

#' @rdname defaults
PROPOSE_DEFAULTS <- list(
  disease_x = list(
    # pathogen parameters
    # offspring
    community_offspring_distribution = "nbinom",
    community_r0 = 2,
    community_disp = 1,
    isolated_offspring_distribution = "nbinom",
    isolated_r0 = 0,
    isolated_disp = 1,
    # delays
    incubation_distribution = "lnorm",
    incubation_meanlog = 1.5,
    incubation_sdlog = 0.4,
    # event probs
    asymptomatic = 0.1,
    presymptomatic_transmission = 0.1
  ),
  covid_19 = list(
    # pathogen parameters
    # offspring
    community_offspring_distribution = "nbinom",
    community_r0 = 2.66,
    community_disp = 0.1,
    isolated_offspring_distribution = "pois",
    isolated_r0 = 0.5,
    # delays
    incubation_distribution = "lnorm",
    incubation_shape = 1.525,
    incubation_scale = 0.629,
    # event probs
    asymptomatic = 0.35,
    presymptomatic_transmission = 0.4
  ),
  # intervetnion parameters
  #delays
  onset_to_isolation_distribution = "lnorm",
  onset_to_isolation_meanlog = 2,
  onset_to_isolation_sdlog = 0.5,
  # contact tracing
  symptomatic_traced = 0.8,
  # intervention
  quarantine = FALSE,
  # sim controls
  cap_max_days = 100,
  cap_cases = 5000,
  replicates = 5,
  initial_cases = 5
)

#' Reset the pathogen parameters in the ***Explore*** page to default values
#'
#' @inheritParams shiny::updateSelectInput
#' @param defaults A list of pathogen parameters, one of the sublist from
#' [PROPOSE_DEFAULTS].
#'
#' @return Nothing, called for side-effects from `shiny::update*()` functions.
#' @keywords internal
reset_pathogen_params <- function(session, defaults) {
  # update pathogen parameters
  updateSelectInput(
    session,
    "community_offspring_distribution",
    selected = defaults$community_offspring_distribution
  )
  updateNumericInput(
    session,
    "community_r0",
    value = defaults$community_r0
  )
  updateNumericInput(
    session,
    "community_disp",
    value = defaults$community_disp
  )
  updateSelectInput(
    session,
    "isolated_offspring_distribution",
    selected = defaults$isolated_offspring_distribution
  )
  updateNumericInput(
    session,
    "isolated_r0",
    value = defaults$isolated_r0
  )
  updateNumericInput(
    session,
    "isolated_disp",
    value = defaults$isolated_disp
  )
  updateSelectInput(
    session,
    "incubation_distribution",
    selected = defaults$incubation_distribution
  )
  updateNumericInput(
    session,
    "incubation_meanlog",
    value = defaults$incubation_meanlog
  )
  updateNumericInput(
    session,
    "incubation_sdlog",
    value = defaults$incubation_sdlog
  )
  updateNumericInput(
    session,
    "asymptomatic",
    value = defaults$asymptomatic
  )
  updateNumericInput(
    session,
    "presymptomatic_transmission",
    value = defaults$presymptomatic_transmission
  )
}
