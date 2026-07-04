#' Sentinel onset-to-isolation delay used to switch isolation off
#'
#' @description
#' A very large finite onset-to-isolation delay, in days, used when the
#' intervention (isolation) is switched off. \pkg{ringbp} rejects an infinite
#' delay (see `ringbp:::check_dist_func()`), so isolation is
#' instead pushed far beyond any simulation horizon (`cap_max_days`), which makes
#' cases effectively never isolated while keeping `isolated_time` finite.
#'
#' @keywords internal
NO_ISOLATION_DELAY <- 1e10

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
  # pathogen parameters
  disease_x = list(
    # offspring
    community_offspring_distribution = "nbinom",
    community_r0 = 2,
    community_disp = 1,
    isolated_offspring_distribution = "nbinom",
    isolated_r0 = 0,
    isolated_disp = 1,
    # incubation period
    incubation_distribution = "lnorm",
    incubation_meanlog = 1.5,
    incubation_sdlog = 0.4,
    # symptom event probs (%)
    asymptomatic = 10,
    presymptomatic_transmission = 10
  ),
  covid_19_wt = list(
    # offspring
    community_offspring_distribution = "nbinom",
    community_r0 = 2.66,
    community_disp = 0.1,
    isolated_offspring_distribution = "pois",
    isolated_r0 = 0.5,
    # incubation period
    incubation_distribution = "lnorm",
    incubation_meanlog = 1.525,
    incubation_sdlog = 0.629,
    # symptom event probs (%)
    asymptomatic = 35,
    presymptomatic_transmission = 40
  ),
  covid_19_alpha = list(
    # offspring
    community_offspring_distribution = "nbinom",
    community_r0 = 4.5,
    community_disp = 0.32,
    isolated_offspring_distribution = "pois",
    isolated_r0 = 0.5,
    # incubation period
    incubation_distribution = "gamma",
    incubation_shape = 3.08,
    incubation_scale = 1.58,
    # symptom event probs (%)
    asymptomatic = 35,
    presymptomatic_transmission = 40
  ),
  covid_19_delta = list(
    # offspring
    community_offspring_distribution = "nbinom",
    community_r0 = 6.5,
    community_disp = 0.23,
    isolated_offspring_distribution = "pois",
    isolated_r0 = 0.5,
    # incubation period
    incubation_distribution = "gamma",
    incubation_shape = 4.43,
    incubation_scale = 1.01,
    # symptom event probs (%)
    asymptomatic = 8.4,
    presymptomatic_transmission = 40
  ),
  covid_19_omicron = list(
    # offspring
    community_offspring_distribution = "nbinom",
    community_r0 = 9.5,
    community_disp = 0.5,
    isolated_offspring_distribution = "pois",
    isolated_r0 = 0.5,
    # incubation period
    incubation_distribution = "lnorm",
    incubation_meanlog = 1.19,
    incubation_sdlog = 0.36,
    # symptom event probs (%)
    asymptomatic = 29,
    presymptomatic_transmission = 40
  ),
  sars = list(
    # offspring
    community_offspring_distribution = "nbinom",
    community_r0 = 2.7,
    community_disp = 0.16,
    isolated_offspring_distribution = "pois",
    isolated_r0 = 0.05,
    # incubation period
    incubation_distribution = "lnorm",
    incubation_meanlog = 1.39,
    incubation_sdlog = 0.59,
    # symptom event probs (%)
    asymptomatic = 7.5,
    presymptomatic_transmission = 5.5
  ),
  mers = list(
    # offspring
    community_offspring_distribution = "nbinom",
    community_r0 = 0.93,
    community_disp = 0.26,
    isolated_offspring_distribution = "pois",
    isolated_r0 = 0.05,
    # incubation period
    incubation_distribution = "lnorm",
    incubation_meanlog = 1.65,
    incubation_sdlog = 0.53,
    # symptom event probs (%)
    asymptomatic = 19,
    presymptomatic_transmission = 0.01
  ),
  ebola_zaire = list(
    # offspring
    community_offspring_distribution = "nbinom",
    community_r0 = 1.75,
    community_disp = 0.5,
    isolated_offspring_distribution = "pois",
    isolated_r0 = 0.1,
    # incubation period
    incubation_distribution = "gamma",
    incubation_shape = 1.58,
    incubation_scale = 6.53,
    # symptom event probs (%)
    asymptomatic = 0,
    presymptomatic_transmission = 0
  ),
  ebola_sudan = list(
    # offspring
    community_offspring_distribution = "nbinom",
    community_r0 = 2.5,
    community_disp = 0.3,
    isolated_offspring_distribution = "pois",
    isolated_r0 = 0.1,
    # incubation period
    incubation_distribution = "gamma",
    incubation_shape = 3.0,
    incubation_scale = 2.33,
    # symptom event probs (%)
    asymptomatic = 0,
    presymptomatic_transmission = 0
  ),
  marburg = list(
    # offspring
    community_offspring_distribution = "nbinom",
    community_r0 = 0.8,
    community_disp = 0.6,
    isolated_offspring_distribution = "pois",
    isolated_r0 = 0.1,
    # incubation period
    incubation_distribution = "gamma",
    incubation_shape = 4.7,
    incubation_scale = 1.6,
    # symptom event probs (%)
    asymptomatic = 1,
    presymptomatic_transmission = 0
  ),
  influenza_h5n1 = list(
    # offspring
    community_offspring_distribution = "nbinom",
    community_r0 = 0.15,
    community_disp = 0.2,
    isolated_offspring_distribution = "pois",
    isolated_r0 = 0.05,
    # incubation period
    incubation_distribution = "lnorm",
    incubation_meanlog = 1.30,
    incubation_sdlog = 0.41,
    # symptom event probs (%)
    asymptomatic = 3,
    presymptomatic_transmission = 5
  ),
  influenza_h1n1pdm = list(
    # offspring
    community_offspring_distribution = "nbinom",
    community_r0 = 1.44,
    community_disp = 0.8,
    isolated_offspring_distribution = "pois",
    isolated_r0 = 0.05,
    # incubation period
    incubation_distribution = "gamma",
    incubation_shape = 3.36,
    incubation_scale = 0.50,
    # symptom event probs (%)
    asymptomatic = 36,
    presymptomatic_transmission = 20
  ),
  influenza_h7n9 = list(
    # offspring
    community_offspring_distribution = "nbinom",
    community_r0 = 0.08,
    community_disp = 0.2,
    isolated_offspring_distribution = "pois",
    isolated_r0 = 0.05,
    # incubation period
    incubation_distribution = "weibull",
    incubation_shape = 2.1,
    incubation_scale = 3.8,
    # symptom event probs (%)
    asymptomatic = 10,
    presymptomatic_transmission = 5
  ),
  meningitis_b = list(
    # offspring
    community_offspring_distribution = "nbinom",
    community_r0 = 1.36,
    community_disp = 1,
    isolated_offspring_distribution = "pois",
    isolated_r0 = 0.05,
    # incubation period
    incubation_distribution = "lnorm",
    incubation_meanlog = 1.30,
    incubation_sdlog = 0.41,
    # symptom event probs (%)
    asymptomatic = 98, # carriage-model interpretation
    presymptomatic_transmission = 0
  ),
  andes_hantavirus = list(
    # offspring
    community_offspring_distribution = "nbinom",
    community_r0 = 2.12,
    community_disp = 0.15,
    isolated_offspring_distribution = "pois",
    isolated_r0 = 0.2,
    # incubation period
    incubation_distribution = "lnorm",
    incubation_meanlog = 3.13,
    incubation_sdlog = 0.38,
    # symptom event probs (%)
    asymptomatic = 0,
    presymptomatic_transmission = 0.05
  ),
  # intervention parameters
  # delays
  onset_to_isolation_distribution = "lnorm",
  onset_to_isolation_meanlog = 2,
  onset_to_isolation_sdlog = 0.5,
  # contact tracing
  symptomatic_traced = 80,
  # intervention
  isolation_on = TRUE,
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
  # basic transmissibility UI: R0s mirror the advanced defaults, variability
  # resets to homogeneous (the basic UI default)
  updateNumericInput(
    session,
    "basic_community_r0",
    value = defaults$community_r0
  )
  updateNumericInput(
    session,
    "basic_isolated_r0",
    value = defaults$isolated_r0
  )
  updateRadioButtons(
    session,
    "basic_transmission_variability",
    selected = "homogeneous"
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
    "incubation_shape",
    value = defaults$incubation_shape
  )
  updateNumericInput(
    session,
    "incubation_scale",
    value = defaults$incubation_scale
  )
  # basic incubation UI: mean derived from the advanced default distribution
  incubation_mean <- round(
    switch(
      defaults$incubation_distribution,
      lnorm = epiparameter::convert_params_to_summary_stats(
        "lnorm",
        meanlog = defaults$incubation_meanlog,
        sdlog = defaults$incubation_sdlog
      )$mean,
      gamma = epiparameter::convert_params_to_summary_stats(
        "gamma",
        shape = defaults$incubation_shape,
        scale = defaults$incubation_scale
      )$mean,
      weibull = epiparameter::convert_params_to_summary_stats(
        "weibull",
        shape = defaults$incubation_shape,
        scale = defaults$incubation_scale
      )$mean
    ),
    1
  )
  updateNumericInput(
    session,
    "basic_incubation_mean",
    value = incubation_mean
  )
  updateRadioButtons(
    session,
    "basic_incubation_variability",
    selected = "moderate"
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

#' Generate [bslib::tooltip()] message for delay distribution parameters
#'
#' @param distribution a `character` string for the distribution type, e.g.,
#' `"Gamma"` or `"Weibull"`.
#' @param epiparameter a `character` string for the name of the epidemiological
#' parameter, e.g. `"incubation period"` or `"onset-to-isolation delay"`.
#'
#' @return a `character` string
#' @keywords internal
#' @name tooltip
shape_tip <- function(distribution, epiparameter) {
  paste0(
    "Shape parameter of the ", distribution, " distribution for the ",
    epiparameter, "."
  )
}

#' @rdname tooltip
scale_tip <- function(distribution, epiparameter) {
  paste0(
    "Scale parameter of the ", distribution, " distribution for the ",
    epiparameter, ". Stretches the distribution along the time axis; ",
    "larger values produce longer ", epiparameter, "s."
  )
}
