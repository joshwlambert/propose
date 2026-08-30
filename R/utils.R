#' Wrap an intervention value in a time-varying NPI-activation function
#'
#' @description
#' Non-pharmaceutical interventions (NPIs) are assumed to activate on a given
#' day of the outbreak. Before that day the intervention value is 0 (no
#' contact tracing / no test-driven isolation); on and after it the
#' user-specified value applies. This mirrors how the `symptomatic_traced`
#' argument in [ringbp::event_prob_opts()] and the `test_sensitivity` argument
#' in [ringbp::intervention_opts()] accept a function of time `t`.
#'
#' [ringbp] coerces scalar intervention values to constant functions of `t`
#' internally, and only ever evaluates them at the (continuous, strictly
#' positive) `onset` / `exposure` times of cases, so `activation_day = 0`
#' reproduces the original scalar behaviour exactly. The strict `t >`
#' comparison follows the same convention as upstream ringbp usage; it would
#' only differ from the scalar if the function were ever evaluated at a
#' discrete `t = 0`, which ringbp does not do.
#'
#' @param value A `numeric` scalar in `[0, 1]`: the intervention value applied
#' once NPIs are active.
#' @param activation_day A `numeric` scalar: the day of the outbreak on which
#' NPIs activate. `0` means active immediately.
#'
#' @return A `function` of a single `numeric` argument `t`.
#' @keywords internal
npi_activation <- function(value, activation_day) {
  # snapshot both args into the returned closure now, rather than capturing
  # promises that resolve lazily on first call (e.g. if called in a loop)
  force(value)
  force(activation_day)
  function(t) ifelse(t > activation_day, value, 0)
}

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
  # asymptomatic transmissibility: reset the toggle off (asymptomatic assumed
  # equal to community) and restore the (hidden) asymptomatic offspring inputs
  update_switch(
    "asymptomatic_transmissibility_different",
    value = FALSE,
    session = session
  )
  updateSelectInput(
    session,
    "asymptomatic_offspring_distribution",
    selected = defaults$asymptomatic_offspring_distribution
  )
  updateNumericInput(
    session,
    "asymptomatic_r0",
    value = defaults$asymptomatic_r0
  )
  updateNumericInput(
    session,
    "asymptomatic_disp",
    value = defaults$asymptomatic_disp
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

#' The display name of one pathogen preset
#'
#' @param key A `character` string naming a pathogen preset, such as `"sars"`.
#'
#' @return A `character` string. Falls back to `"Scenario"` when the key is
#' missing or unrecognised, so a scenario is always named something.
#' @keywords internal
pathogen_label <- function(key) {
  if (is.null(key) || length(key) != 1L || is.na(key) ||
      !key %in% names(PATHOGEN_LABELS)) {
    return("Scenario")
  }
  unname(PATHOGEN_LABELS[[key]])
}

#' Reset the intervention parameters to default values
#'
#' @description
#' The counterpart to [reset_pathogen_params()] for the parameters that describe
#' the response rather than the pathogen: the onset-to-isolation delay, contact
#' tracing, isolation, quarantine, test sensitivity and the NPI activation day.
#' Shared by the ***Explore*** and ***Compare*** pages, which offer the same
#' "Reset Defaults" action over the same sidebar.
#'
#' The simulation controls (`cap_max_days`, `cap_cases`, `replicates` and
#' `initial_cases`) are left to the caller, because pages differ in which of
#' them they expose and at what defaults.
#'
#' @inheritParams shiny::updateSelectInput
#'
#' @return Nothing, called for side-effects from `shiny::update*()` functions.
#' @keywords internal
reset_intervention_params <- function(session) {
  updateSelectInput(
    session,
    "onset_to_isolation_distribution",
    selected = PROPOSE_DEFAULTS$onset_to_isolation_distribution
  )
  updateNumericInput(
    session,
    "onset_to_isolation_meanlog",
    value = PROPOSE_DEFAULTS$onset_to_isolation_meanlog
  )
  updateNumericInput(
    session,
    "onset_to_isolation_sdlog",
    value = PROPOSE_DEFAULTS$onset_to_isolation_sdlog
  )
  # basic onset-to-isolation UI: mean derived from the advanced (lnorm)
  # default, variability resets to moderate
  updateNumericInput(
    session,
    "basic_onset_to_isolation_mean",
    value = round(
      epiparameter::convert_params_to_summary_stats(
        "lnorm",
        meanlog = PROPOSE_DEFAULTS$onset_to_isolation_meanlog,
        sdlog = PROPOSE_DEFAULTS$onset_to_isolation_sdlog
      )$mean,
      1
    )
  )
  updateRadioButtons(
    session,
    "basic_onset_to_isolation_variability",
    selected = "moderate"
  )
  updateNumericInput(
    session,
    "symptomatic_traced",
    value = PROPOSE_DEFAULTS$symptomatic_traced
  )
  update_switch("isolation_on", value = PROPOSE_DEFAULTS$isolation_on, session = session)
  updateCheckboxInput(session, "quarantine", value = PROPOSE_DEFAULTS$quarantine)
  updateNumericInput(session, "test_sensitivity", value = PROPOSE_DEFAULTS$test_sensitivity)
  updateNumericInput(session, "npi_activation_day", value = PROPOSE_DEFAULTS$npi_activation_day)
  invisible(NULL)
}

#' Probability that an outbreak is controlled, with a 95% confidence interval
#'
#' @description
#' The proportion of replicates in which the outbreak went extinct, with a
#' Clopper-Pearson exact 95% confidence interval.
#'
#' Must be called on the `scenario` exactly as [ringbp::scenario_sim()] returned
#' it. [ringbp::detect_extinct()] reads the `extinct` attribute the simulation
#' attaches, and that attribute is dropped by subsetting or row-binding — so on
#' the ***Compare*** page this is computed per scenario *before* scenarios are
#' combined for plotting.
#'
#' @param scenario A [data.table::data.table] returned by
#' [ringbp::scenario_sim()], or `NULL`.
#'
#' @return A `list` with elements `p`, `lower` and `upper`, or `NULL` if
#' `scenario` is `NULL`.
#' @keywords internal
control_stats <- function(scenario) {
  if (is.null(scenario)) {
    return(NULL)
  }
  n <- max(scenario$sim)
  k <- sum(detect_extinct(scenario)$extinct)
  ci <- stats::binom.test(k, n)$conf.int
  list(p = k / n, lower = ci[1], upper = ci[2])
}

#' Caption reporting the confidence interval from [control_stats()]
#'
#' @param stats A `list` as returned by [control_stats()], or `NULL`.
#' @param class A `character` string of CSS classes for the caption. Defaults to
#' `"text-white-50"`, which suits the dark value box on the ***Explore*** page.
#' The ***Compare*** page's value boxes have a light background, where white
#' text would be unreadable, and pass `"text-muted"` instead.
#'
#' @return A [shiny::tags] `small` element, or `NULL`.
#' @keywords internal
control_ci_caption <- function(stats, class = "text-white-50") {
  if (is.null(stats)) {
    return(NULL)
  }
  tags$small(
    class = class,
    sprintf(
      "95%% CI: %s – %s",
      signif(stats$lower, digits = 2),
      signif(stats$upper, digits = 2)
    )
  )
}

#' Total outbreak size across replicates, with the spread around it
#'
#' @description
#' The number of cases each replicate reached by the end of the simulation,
#' which is to say after `cap_max_days` days. Reported as a median with a 95%
#' interval rather than a mean: the distribution mixes outbreaks that were
#' controlled early with outbreaks that grew unchecked, so the mean sits between
#' the two and describes neither.
#'
#' Sizes are taken **uncapped**. A replicate can finish above `cap_cases`,
#' because the whole of the week in which it crosses the cap is generated before
#' the simulation stops, and clamping it would report the cap as though it were
#' a result. `capped` instead reports what proportion of replicates reached the
#' cap, so a caller can say that those outbreaks were still growing when
#' counting stopped rather than presenting a number the cap chose.
#'
#' @param scenario A [data.table::data.table] returned by
#' [ringbp::scenario_sim()], or `NULL`.
#' @param cap_cases A `numeric` scalar, the maximum cumulative cases the
#' simulation was run with.
#'
#' @return A `list` with elements `median`, `lower`, `upper` and `capped`, or
#' `NULL` if `scenario` is `NULL`.
#' @keywords internal
outbreak_size_stats <- function(scenario, cap_cases) {
  if (is.null(scenario)) {
    return(NULL)
  }
  sizes <- scenario[, max(cumulative), by = sim]$V1
  list(
    median = stats::median(sizes),
    lower = stats::quantile(sizes, 0.025, names = FALSE),
    upper = stats::quantile(sizes, 0.975, names = FALSE),
    capped = mean(sizes >= cap_cases)
  )
}

#' Format a case count for display
#'
#' @param x A `numeric` scalar.
#'
#' @return A `character` string, rounded to a whole case and thousands-separated.
#' @keywords internal
format_cases <- function(x) {
  format(round(x), big.mark = ",", scientific = FALSE, trim = TRUE)
}

#' Clamp a simulated outbreak at the maximum-cases cap for plotting
#'
#' @description
#' A replicate can overshoot `cap_cases` in the week it reaches the cap, because
#' the whole of that week's transmission is generated before the simulation
#' stops. Plotting the raw values would show trajectories rising above a cap the
#' plot also draws as a guide line. Extinct and uncapped replicates are
#' unchanged.
#'
#' Copies before mutating: `scenario` is normally a cached reactive value, and
#' \pkg{data.table} would otherwise modify it in place.
#'
#' @param scenario A [data.table::data.table] returned by
#' [ringbp::scenario_sim()].
#' @param cap_cases A `numeric` scalar, the maximum cumulative cases the
#' simulation was run with.
#'
#' @return A [data.table::data.table] with `cumulative` clamped and
#' `weekly_cases` recomputed to match.
#' @keywords internal
cap_scenario <- function(scenario, cap_cases) {
  dt <- data.table::copy(scenario)
  dt[, cumulative := pmin(cumulative, cap_cases), by = sim]
  dt[, weekly_cases := cumulative - data.table::shift(cumulative, fill = 0), by = sim]
  dt[]
}

#' The styled app name, *propose*, for use in UI text
#'
#' @description
#' Returns the app name "propose" styled in bold italic, for use inline in UI
#' text. Bundling it here keeps the styling consistent across the app and makes
#' restyling a one-line change. Returning [htmltools::HTML()] (rather than
#' nested tags) also avoids the whitespace \pkg{htmltools} inserts between tag
#' children, so trailing punctuation can be attached with no rogue space.
#'
#' @param suffix A `character` string appended directly after the name with no
#' separating space, for punctuation such as `"?"`, `"."` or `","`. Defaults to
#' `""`.
#'
#' @return An [htmltools::HTML()] fragment.
#' @keywords internal
propose_name <- function(suffix = "") {
  HTML(paste0("<strong><em>propose</em></strong>", suffix))
}

#' The styled dependency name, `{ringbp}`, for use in UI text
#'
#' @description
#' Returns the \pkg{ringbp} package name in monospace (a `<code>` element, the
#' HTML equivalent of markdown backticks), keeping `{ringbp}` styled
#' consistently across the app's HTML and markdown UI text. Like [propose_name()]
#' it returns [htmltools::HTML()] so trailing punctuation can be attached with no
#' rogue whitespace.
#'
#' @param suffix A `character` string appended directly after the name with no
#' separating space, for punctuation such as `"."` or `","`. Defaults to `""`.
#'
#' @return An [htmltools::HTML()] fragment.
#' @keywords internal
ringbp_name <- function(suffix = "") {
  HTML(paste0("<code>{ringbp}</code>", suffix))
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
