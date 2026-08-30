#' Canonical parameters describing a single outbreak scenario
#'
#' @name canonical
#'
#' @description
#' A *canonical parameter list* is a flat, fixed-order `list` of scalars that
#' fully describes one outbreak scenario, independent of how the user entered
#' it. It is the interchange format between the sidebar inputs and
#' [ringbp::scenario_sim()]:
#'
#' * [canonical_params()] resolves raw input values into it,
#' * [scenario_opts()] turns it back into \pkg{ringbp} option objects,
#' * [param_diff()] compares several of them to report what differs between
#'   scenarios.
#'
#' Two properties make it suitable for the ***Compare*** page:
#'
#' 1. **It holds values, not closures.** The sampling functions \pkg{ringbp}
#'    needs close over `input$...`, so storing them would make an already-added
#'    scenario silently change when the sidebar is edited afterwards. Scalars
#'    snapshot cleanly.
#' 2. **It is mode-resolved and fixed-order.** The Basic and Advanced UI modes
#'    collapse onto the same fields, so two scenarios that describe the same
#'    simulation compare equal under [identical()] regardless of which mode was
#'    used to enter them. Fixed field order matters because [identical()] on
#'    lists is order-sensitive.
#'
#' `NA` has a single, uniform meaning: **this parameter cannot affect this
#' scenario**, because the UI toggle governing it is off. That covers the
#' dispersion of a non-Negative-Binomial offspring distribution, the
#' asymptomatic offspring parameters when asymptomatic transmissibility is not
#' set separately, and every intervention parameter when isolation is switched
#' off. Reporting those as `NA` keeps them out of the "what differs" table,
#' where they would otherwise show up as differences that change nothing.
#' [scenario_opts()] substitutes the model-neutral value for each when it builds
#' the simulation.
NULL

#' Resolve raw sidebar input values into a canonical parameter list
#'
#' @param vals A named `list` of raw input values, as produced by
#' [shiny::reactiveValuesToList()] on a module's `input`. Only the sidebar
#' inputs are read; any other elements are ignored.
#'
#' @return A named `list` of scalars. See [canonical].
#' @keywords internal
canonical_params <- function(vals) {
  # offspring: the Basic mode is a Negative Binomial whose dispersion is set by
  # the transmission-variability radio, so it resolves onto the same three
  # fields the Advanced mode exposes directly.
  if (identical(vals$transmissibility_ui, "basic")) {
    k <- lookup(BASIC_K, vals$basic_transmission_variability)
    community <- list(distribution = "nbinom", r0 = as_num_scalar(vals$basic_community_r0), disp = k)
    isolated <- list(distribution = "nbinom", r0 = as_num_scalar(vals$basic_isolated_r0), disp = k)
    asymptomatic <- list(distribution = NA_character_, r0 = NA_real_, disp = NA_real_)
  } else {
    community <- offspring_fields(
      vals$community_offspring_distribution, vals$community_r0, vals$community_disp
    )
    isolated <- offspring_fields(
      vals$isolated_offspring_distribution, vals$isolated_r0, vals$isolated_disp
    )
    asymptomatic <- if (isTRUE(vals$asymptomatic_transmissibility_different)) {
      offspring_fields(
        vals$asymptomatic_offspring_distribution,
        vals$asymptomatic_r0,
        vals$asymptomatic_disp
      )
    } else {
      list(distribution = NA_character_, r0 = NA_real_, disp = NA_real_)
    }
  }

  incubation <- delay_fields(vals, "incubation")

  # isolation switched off: no case is ever isolated, which makes the
  # onset-to-isolation delay and every intervention parameter inert. They are
  # reported as NA so they cannot appear as a difference that changes nothing.
  isolation_on <- !identical(vals$isolation_on, FALSE)
  if (isolation_on) {
    onset_to_isolation <- delay_fields(vals, "onset_to_isolation")
    interventions <- list(
      symptomatic_traced = as_num_scalar(vals$symptomatic_traced),
      quarantine = isTRUE(vals$quarantine),
      test_sensitivity = as_num_scalar(vals$test_sensitivity),
      npi_activation_day = as_num_scalar(vals$npi_activation_day)
    )
  } else {
    onset_to_isolation <- list(
      distribution = NA_character_, par1 = NA_real_, par2 = NA_real_
    )
    interventions <- list(
      symptomatic_traced = NA_real_,
      quarantine = NA,
      test_sensitivity = NA_real_,
      npi_activation_day = NA_real_
    )
  }

  # fixed order: identical() on lists is order-sensitive, and duplicate
  # scenario detection relies on it
  list(
    community_distribution = community$distribution,
    community_r0 = community$r0,
    community_disp = community$disp,
    isolated_distribution = isolated$distribution,
    isolated_r0 = isolated$r0,
    isolated_disp = isolated$disp,
    asymptomatic_distribution = asymptomatic$distribution,
    asymptomatic_r0 = asymptomatic$r0,
    asymptomatic_disp = asymptomatic$disp,
    incubation_distribution = incubation$distribution,
    incubation_par1 = incubation$par1,
    incubation_par2 = incubation$par2,
    isolation_on = isolation_on,
    onset_to_isolation_distribution = onset_to_isolation$distribution,
    onset_to_isolation_par1 = onset_to_isolation$par1,
    onset_to_isolation_par2 = onset_to_isolation$par2,
    asymptomatic_pct = as_num_scalar(vals$asymptomatic),
    presymptomatic_transmission = as_num_scalar(vals$presymptomatic_transmission),
    symptomatic_traced = interventions$symptomatic_traced,
    quarantine = interventions$quarantine,
    test_sensitivity = interventions$test_sensitivity,
    npi_activation_day = interventions$npi_activation_day
  )
}

#' Resolve one offspring distribution into canonical fields
#'
#' @description
#' Dispersion only parameterises the Negative Binomial, so it is `NA` for the
#' Poisson and Geometric distributions, whose dispersion input is hidden by a
#' [shiny::conditionalPanel()].
#'
#' @param distribution A `character` string, one of `"nbinom"`, `"pois"` or
#' `"geom"`.
#' @param r0,disp `numeric` scalars for the reproduction number and the
#' Negative Binomial dispersion.
#'
#' @return A `list` with elements `distribution`, `r0` and `disp`.
#' @keywords internal
offspring_fields <- function(distribution, r0, disp) {
  list(
    distribution = as_chr_scalar(distribution),
    r0 = as_num_scalar(r0),
    disp = if (identical(distribution, "nbinom")) as_num_scalar(disp) else NA_real_
  )
}

#' Coerce a raw input value to a length-one scalar
#'
#' @description
#' Coercion that guarantees a length-one result, which base R's `as.numeric()`
#' and `as.character()` do not: a Shiny input that has not yet registered with
#' the server reads as `NULL`, and `as.numeric(NULL)` is length **zero** rather
#' than `NA`. A length-zero element would break the canonical parameter list,
#' whose comparison by [identical()] underpins both duplicate-scenario detection
#' and the "what differs" table. Anything that is not already a single value
#' becomes `NA` of the right type.
#'
#' @param x A value of length zero or one.
#'
#' @return A length-one `numeric` (`as_num_scalar()`) or `character`
#' (`as_chr_scalar()`), `NA` of that type when `x` is absent.
#' @name scalar
#' @keywords internal
as_num_scalar <- function(x) {
  if (is.null(x) || length(x) != 1L) NA_real_ else as.numeric(x)
}

#' @rdname scalar
as_chr_scalar <- function(x) {
  if (is.null(x) || length(x) != 1L) NA_character_ else as.character(x)
}

#' Look a name up in a named vector, tolerating a missing name
#'
#' @param table A named vector, such as [BASIC_K] or [BASIC_DELAY_SHAPE].
#' @param name A `character` string, possibly `NULL` if the input driving it
#' has not yet registered.
#'
#' @return The matching element of `table`, unnamed, or `NA_real_`.
#' @keywords internal
lookup <- function(table, name) {
  if (is.null(name) || length(name) != 1L || !name %in% names(table)) {
    return(NA_real_)
  }
  unname(table[[name]])
}

#' Resolve one delay distribution into canonical fields
#'
#' @description
#' The Basic mode collects an average and a variability category, which
#' parameterise a Gamma distribution with shape from [BASIC_DELAY_SHAPE] and
#' `scale = mean / shape`. That resolves onto the same
#' `distribution`/`par1`/`par2` fields the Advanced mode fills directly, so a
#' delay entered in either mode compares equal.
#'
#' @param vals A named `list` of raw input values.
#' @param delay_type A `character` string, either `"incubation"` or
#' `"onset_to_isolation"`.
#'
#' @return A `list` with elements `distribution`, `par1` and `par2`. For the
#' Lognormal, `par1`/`par2` are `meanlog`/`sdlog`; for the Gamma and Weibull
#' they are `shape`/`scale`.
#' @keywords internal
delay_fields <- function(vals, delay_type) {
  val <- function(suffix) vals[[paste0(delay_type, suffix)]]

  if (identical(val("_ui"), "basic")) {
    shape <- lookup(
      BASIC_DELAY_SHAPE, vals[[paste0("basic_", delay_type, "_variability")]]
    )
    mean <- as_num_scalar(vals[[paste0("basic_", delay_type, "_mean")]])
    return(list(distribution = "gamma", par1 = shape, par2 = mean / shape))
  }

  distribution <- as_chr_scalar(val("_distribution"))
  if (identical(distribution, "lnorm")) {
    list(
      distribution = distribution,
      par1 = as_num_scalar(val("_meanlog")),
      par2 = as_num_scalar(val("_sdlog"))
    )
  } else {
    list(
      distribution = distribution,
      par1 = as_num_scalar(val("_shape")),
      par2 = as_num_scalar(val("_scale"))
    )
  }
}

#' Collect the sidebar inputs of the calling module into a canonical parameter
#' list
#'
#' @description
#' The \pkg{shiny}-facing wrapper around [canonical_params()]. It applies the
#' same range guards the ***Explore*** page applies before simulating, so it
#' must be called from a reactive context. [canonical_params()] itself is pure
#' and takes a plain `list`, which keeps the resolution logic testable without
#' a Shiny session.
#'
#' @param input The Shiny `input` reactive of the calling module.
#'
#' @return A named `list` of scalars. See [canonical].
#' @keywords internal
collect_params <- function(input) {
  req(input$asymptomatic >= 0 && input$asymptomatic <= 100)
  req(
    input$presymptomatic_transmission >= 0 &&
      input$presymptomatic_transmission <= 100
  )
  req(input$symptomatic_traced >= 0 && input$symptomatic_traced <= 100)
  req(input$test_sensitivity >= 0 && input$test_sensitivity <= 1)
  req(!is.na(input$npi_activation_day), input$npi_activation_day >= 0)
  # guards for startup, where the mode toggles are NULL until the radioButtons
  # register with the server
  req(input$transmissibility_ui, input$incubation_ui)

  canonical_params(reactiveValuesToList(input))
}

#' Check a canonical parameter list for values the simulation cannot accept
#'
#' @description
#' A last line of defence behind the `shinyFeedback` validators registered by
#' the `*_feedback_server()` functions. Those give live feedback next to the
#' offending input; this reports the same problems as text, so a scenario with
#' an invalid parameter can be refused outright rather than added and then
#' failing at simulation time.
#'
#' @param params A canonical parameter list. See [canonical].
#'
#' @return A `character` vector of human-readable problems, empty when the
#' parameters are valid.
#' @keywords internal
validate_params <- function(params) {
  problems <- character()
  check <- function(ok, message) {
    if (!isTRUE(ok)) problems <<- c(problems, message)
  }
  # NA is meaningful (the parameter is inert), but NULL or NaN means an input
  # was blank or never registered
  present <- function(x) !is.null(x) && length(x) == 1L && (is.na(x) || !is.nan(x))
  # a finite, non-NA number; inert (NA) parameters are skipped by their caller
  number <- function(x) present(x) && !is.na(x) && is.finite(x)
  # dispersion is the one parameter for which an infinite value is meaningful,
  # so it cannot use number(): the "homogeneous transmission" option sets
  # k = BASIC_K[["homogeneous"]] = Inf, the Poisson limit of the Negative
  # Binomial, which rnbinom() accepts. Every other numeric parameter must be
  # finite.
  dispersion <- function(x) present(x) && !is.na(x) && x > 0
  # dispersion is required exactly when the offspring distribution is Negative
  # Binomial. Keying off the distribution rather than off `!is.na(disp)` matters:
  # canonical parameters use NA both for "inert" (the Poisson and Geometric
  # distributions have no dispersion) and for "the user left the box blank", and
  # only the first of those should be skipped. Skipping both let a blank
  # dispersion reach rnbinom(size = NA), which returns NaN and fails inside
  # ringbp with an opaque message about `community`.
  check_dispersion <- function(distribution, value, setting) {
    if (!identical(distribution, "nbinom")) {
      return(invisible(NULL))
    }
    check(
      dispersion(value),
      sprintf("%s dispersion (k) must be a number greater than 0.", setting)
    )
  }

  check(number(params$community_r0) && params$community_r0 >= 0,
        "Community R0 must be a number and cannot be negative.")
  check(number(params$isolated_r0) && params$isolated_r0 >= 0,
        "Isolated R0 must be a number and cannot be negative.")
  check_dispersion(params$community_distribution, params$community_disp, "Community")
  check_dispersion(params$isolated_distribution, params$isolated_disp, "Isolated")
  if (!is.na(params$asymptomatic_distribution)) {
    check(number(params$asymptomatic_r0) && params$asymptomatic_r0 >= 0,
          "Asymptomatic R0 must be a number and cannot be negative.")
    check_dispersion(
      params$asymptomatic_distribution, params$asymptomatic_disp, "Asymptomatic"
    )
  }
  check(number(params$incubation_par1) && params$incubation_par1 > 0 &&
          number(params$incubation_par2) && params$incubation_par2 > 0,
        "The incubation period parameters must be greater than 0.")
  if (isTRUE(params$isolation_on)) {
    check(number(params$onset_to_isolation_par1) &&
            params$onset_to_isolation_par1 > 0 &&
            number(params$onset_to_isolation_par2) &&
            params$onset_to_isolation_par2 > 0,
          "The onset-to-isolation delay parameters must be greater than 0.")
    check(number(params$symptomatic_traced) && params$symptomatic_traced >= 0 &&
            params$symptomatic_traced <= 100,
          "The percentage of contacts traced must be between 0 and 100.")
    check(number(params$test_sensitivity) && params$test_sensitivity >= 0 &&
            params$test_sensitivity <= 1,
          "Test sensitivity must be between 0 and 1.")
    check(number(params$npi_activation_day) && params$npi_activation_day >= 0,
          "The NPI activation day cannot be negative.")
  }
  check(number(params$asymptomatic_pct) && params$asymptomatic_pct >= 0 &&
          params$asymptomatic_pct <= 100,
        "The percentage of asymptomatic cases must be between 0 and 100.")
  check(number(params$presymptomatic_transmission) &&
          params$presymptomatic_transmission >= 0 &&
          params$presymptomatic_transmission <= 100,
        "The percentage of presymptomatic transmission must be between 0 and 100.")

  problems
}

#' Build a random sampling function for an offspring distribution
#'
#' @inheritParams offspring_fields
#'
#' @return A `function` of a single argument `n`, as expected by
#' [ringbp::offspring_opts()].
#' @keywords internal
offspring_sampler <- function(distribution, r0, disp) {
  # snapshot into the returned closure now rather than capturing promises that
  # resolve lazily on first call, following npi_activation()
  force(r0)
  force(disp)
  switch(
    distribution,
    nbinom = \(n) rnbinom(n = n, mu = r0, size = disp),
    pois = \(n) rpois(n = n, lambda = r0),
    geom = \(n) rgeom(n = n, prob = 1 / (1 + r0))
  )
}

#' Build a random sampling function for a delay distribution
#'
#' @param distribution A `character` string, one of `"lnorm"`, `"gamma"` or
#' `"weibull"`.
#' @param par1,par2 `numeric` scalars. `meanlog`/`sdlog` for the Lognormal,
#' `shape`/`scale` for the Gamma and Weibull.
#'
#' @return A `function` of a single argument `n`, as expected by
#' [ringbp::delay_opts()].
#' @keywords internal
delay_sampler <- function(distribution, par1, par2) {
  force(par1)
  force(par2)
  switch(
    distribution,
    lnorm = \(n) rlnorm(n = n, meanlog = par1, sdlog = par2),
    gamma = \(n) rgamma(n = n, shape = par1, scale = par2),
    weibull = \(n) rweibull(n = n, shape = par1, scale = par2)
  )
}

#' Build the \pkg{ringbp} option objects for a canonical parameter list
#'
#' @description
#' Rebuilds the sampling closures from the snapshotted scalars and assembles
#' the four option objects [ringbp::scenario_sim()] takes. Two conversions
#' happen here, both matching the rest of the app: the UI collects percentages
#' where the model expects proportions, and the time-varying `symptomatic_traced`
#' and `test_sensitivity` arguments are wrapped by [npi_activation()] so they
#' only apply from the NPI activation day onwards.
#'
#' Parameters reported as `NA` (see [canonical]) are substituted with the value
#' that makes them inert: no isolation within the simulation horizon
#' ([NO_ISOLATION_DELAY]), no contact tracing, and no test-driven isolation.
#'
#' @param params A canonical parameter list. See [canonical].
#'
#' @return A `list` with elements `offspring`, `delays`, `event_probs` and
#' `interventions`.
#' @keywords internal
scenario_opts <- function(params) {
  community <- offspring_sampler(
    params$community_distribution, params$community_r0, params$community_disp
  )
  isolated <- offspring_sampler(
    params$isolated_distribution, params$isolated_r0, params$isolated_disp
  )
  offspring <- if (is.na(params$asymptomatic_distribution)) {
    # ringbp::offspring_opts() defaults asymptomatic to community
    offspring_opts(community = community, isolated = isolated)
  } else {
    offspring_opts(
      community = community,
      isolated = isolated,
      asymptomatic = offspring_sampler(
        params$asymptomatic_distribution,
        params$asymptomatic_r0,
        params$asymptomatic_disp
      )
    )
  }

  onset_to_isolation <- if (isTRUE(params$isolation_on)) {
    delay_sampler(
      params$onset_to_isolation_distribution,
      params$onset_to_isolation_par1,
      params$onset_to_isolation_par2
    )
  } else {
    \(n) rep(NO_ISOLATION_DELAY, n)
  }

  # inert when isolation is off: nothing is ever isolated, so nothing is traced
  activation_day <- if (is.na(params$npi_activation_day)) 0 else params$npi_activation_day
  traced <- if (is.na(params$symptomatic_traced)) 0 else params$symptomatic_traced / 100
  sensitivity <- if (is.na(params$test_sensitivity)) 0 else params$test_sensitivity

  list(
    offspring = offspring,
    delays = delay_opts(
      incubation_period = delay_sampler(
        params$incubation_distribution,
        params$incubation_par1,
        params$incubation_par2
      ),
      onset_to_isolation = onset_to_isolation
    ),
    event_probs = event_prob_opts(
      # UI collects percentages; the model expects proportions (0-1)
      asymptomatic = params$asymptomatic_pct / 100,
      presymptomatic_transmission = params$presymptomatic_transmission / 100,
      symptomatic_traced = npi_activation(traced, activation_day)
    ),
    interventions = intervention_opts(
      quarantine = isTRUE(params$quarantine),
      test_sensitivity = npi_activation(sensitivity, activation_day)
    )
  )
}

#' Simulate one outbreak scenario
#'
#' @description
#' Deliberately does **not** set the random seed. The caller does, which is what
#' lets the ***Compare*** page reuse one seed across every scenario in a run and
#' so make a whole comparison reproducible.
#'
#' Note that reusing the seed does **not** pair replicates across scenarios:
#' changing an intervention changes how many random draws each replicate
#' consumes, so the streams diverge immediately and the measured correlation
#' between scenarios is near zero. It buys reproducibility, not the variance
#' reduction that common random numbers would give.
#'
#' @param params A canonical parameter list. See [canonical].
#' @param shared A named `list` of the simulation settings that are held
#' constant across scenarios: `replicates`, `initial_cases`, `cap_max_days` and
#' `cap_cases`.
#'
#' @return A [data.table::data.table], the output of [ringbp::scenario_sim()],
#' carrying its `extinct` and `cap_cases` attributes.
#' @keywords internal
run_scenario <- function(params, shared) {
  opts <- scenario_opts(params)
  scenario_sim(
    n = shared$replicates,
    initial_cases = shared$initial_cases,
    offspring = opts$offspring,
    delays = opts$delays,
    event_probs = opts$event_probs,
    interventions = opts$interventions,
    sim = sim_opts(
      cap_max_days = shared$cap_max_days,
      cap_cases = shared$cap_cases
    )
  )
}

#' Format one canonical parameter value for display
#'
#' @description
#' `NA` renders as an em dash: the parameter cannot affect that scenario (see
#' [canonical]), which is different from it having a value of zero.
#'
#' @param name A `character` string naming the canonical parameter.
#' @param value The value of that parameter.
#'
#' @return A `character` string.
#' @keywords internal
format_param <- function(name, value) {
  if (is.null(value) || length(value) != 1L || is.na(value)) {
    return("—")
  }
  if (is.logical(value)) {
    return(if (value) "Yes" else "No")
  }
  if (is.character(value)) {
    return(unname(DISTRIBUTION_LABELS[value] %|NA|% value))
  }
  pct <- c("asymptomatic_pct", "presymptomatic_transmission", "symptomatic_traced")
  if (name %in% pct) {
    return(paste0(signif(value, digits = 3), "%"))
  }
  if (identical(name, "npi_activation_day")) {
    return(paste0("Day ", signif(value, digits = 3)))
  }
  format(signif(value, digits = 3))
}

#' Replace `NA` with a fallback
#'
#' @description
#' A small infix helper for looking values up in a named vector, where a name
#' that is absent yields `NA` and should fall back to the name itself.
#'
#' @param x,y Vectors of the same length.
#'
#' @return `x`, with any `NA` elements replaced by the corresponding element of
#' `y`.
#' @keywords internal
`%|NA|%` <- function(x, y) {
  ifelse(is.na(x), y, x)
}

#' Split canonical parameters into those that vary between scenarios and those
#' shared by all of them
#'
#' @description
#' Drives the ***Compare*** page's parameter table, whose purpose is to say
#' plainly what is different between the scenarios being compared.
#'
#' `NA` values are excluded from the comparison rather than treated as a value
#' of their own, because `NA` means the parameter cannot affect that scenario
#' (see [canonical]). A parameter is therefore *varying* only when it takes two
#' or more different values among the scenarios it applies to, *shared* when it
#' takes one, and neither when it applies to none of them.
#'
#' @param scenarios A `list` of canonical parameter lists. See [canonical].
#'
#' @return A `list` with two `character` vectors of parameter names, `varying`
#' and `shared`.
#' @keywords internal
param_diff <- function(scenarios) {
  if (length(scenarios) == 0L) {
    return(list(varying = character(), shared = character()))
  }
  names_all <- names(PARAM_LABELS)

  classify <- vapply(names_all, function(nm) {
    values <- lapply(scenarios, `[[`, nm)
    # Compare only the scenarios the parameter actually applies to. `NA` means
    # the parameter cannot affect that scenario (see [canonical]), which says
    # nothing about whether it differs — so an onset-to-isolation delay that is
    # Gamma in every scenario that isolates counts as shared, even when other
    # scenarios switch isolation off. Treating NA as just another value would
    # instead report the delay, its two parameters, the test sensitivity and the
    # NPI activation day as five separate differences, when the single real
    # difference is that isolation is off.
    known <- values[!vapply(
      values, function(x) length(x) != 1L || is.na(x), logical(1)
    )]
    if (length(known) == 0L) {
      return("inert")
    }
    if (all(vapply(known, identical, logical(1), known[[1L]]))) {
      return("shared")
    }
    "varying"
  }, character(1))

  list(
    varying = names_all[classify == "varying"],
    shared = names_all[classify == "shared"]
  )
}

#' Write a snapshot of sidebar input values back into the sidebar
#'
#' @description
#' Restores the scenario a user added so they can edit it, on the ***Compare***
#' page's "Load into sidebar" action. It takes the *raw* input snapshot rather
#' than the canonical parameters (see [canonical]) so the sidebar comes back in
#' the mode the user entered it in, rather than being rewritten into the
#' Advanced mode.
#'
#' @inheritParams shiny::updateSelectInput
#' @param vals A named `list` of raw input values, as captured by
#' [shiny::reactiveValuesToList()] when the scenario was added.
#'
#' @return Nothing, called for side-effects from `shiny::update*()` functions.
#' @keywords internal
apply_params <- function(session, vals) {
  set <- function(ids, update) {
    for (id in ids) {
      if (!is.null(vals[[id]])) update(id, vals[[id]])
    }
  }
  set(RESTORE_SELECT_IDS, function(id, value) {
    updateSelectInput(session, id, selected = value)
  })
  set(RESTORE_RADIO_IDS, function(id, value) {
    updateRadioButtons(session, id, selected = value)
  })
  set(RESTORE_SWITCH_IDS, function(id, value) {
    update_switch(id, value = value, session = session)
  })
  set(RESTORE_CHECKBOX_IDS, function(id, value) {
    updateCheckboxInput(session, id, value = value)
  })
  set(RESTORE_NUMERIC_IDS, function(id, value) {
    updateNumericInput(session, id, value = value)
  })
  invisible(NULL)
}

#' Row label for one canonical parameter, given the scenarios being compared
#'
#' @description
#' Mostly a lookup in [PARAM_LABELS], but the two parameters of each delay
#' distribution mean different things depending on which distribution was
#' chosen. Where every scenario uses the same delay distribution the label names
#' the actual parameter ("Incubation period (meanlog)"); where they differ the
#' generic positional label is kept, because there is no single right name.
#'
#' @param name A `character` string naming the canonical parameter.
#' @param scenarios A `list` of canonical parameter lists. See [canonical].
#'
#' @return A `character` string.
#' @keywords internal
param_label <- function(name, scenarios) {
  generic <- unname(PARAM_LABELS[[name]])
  par_index <- switch(sub(".*_par", "", name), "1" = 1L, "2" = 2L, NULL)
  if (is.null(par_index) || !grepl("_par[12]$", name)) {
    return(generic)
  }
  distributions <- unique(unlist(lapply(
    scenarios, `[[`, sub("_par[12]$", "_distribution", name)
  )))
  distributions <- distributions[!is.na(distributions)]
  if (length(distributions) != 1L || !distributions %in% names(DELAY_PAR_NAMES)) {
    return(generic)
  }
  sprintf(
    "%s (%s)",
    sub(" parameter [12]$", "", generic),
    DELAY_PAR_NAMES[[distributions]][par_index]
  )
}


#' Mean and standard deviation of a delay distribution, in days
#'
#' @description
#' Reduces a delay from the parameters the simulation needs to the two numbers a
#' user thinks in. This is what makes scenario naming independent of how the
#' delay was entered: a Gamma with shape 5 and scale 0.96 and a Lognormal with
#' meanlog 1.39 and sdlog 0.59 are both "about 4.8 days", and only differ in the
#' spread.
#'
#' @inheritParams delay_sampler
#'
#' @return A `list` with elements `mean` and `sd`, both `NA_real_` when the
#' delay does not apply (see [canonical]).
#' @keywords internal
delay_summary <- function(distribution, par1, par2) {
  if (is.null(distribution) || length(distribution) != 1L || is.na(distribution) ||
      is.na(par1) || is.na(par2)) {
    return(list(mean = NA_real_, sd = NA_real_))
  }
  stats <- switch(
    distribution,
    lnorm = epiparameter::convert_params_to_summary_stats(
      "lnorm", meanlog = par1, sdlog = par2
    ),
    gamma = epiparameter::convert_params_to_summary_stats(
      "gamma", shape = par1, scale = par2
    ),
    weibull = epiparameter::convert_params_to_summary_stats(
      "weibull", shape = par1, scale = par2
    )
  )
  list(mean = round(stats$mean, 1), sd = round(stats$sd, 1))
}

#' The parameters a pathogen preset produces when it is selected and nothing
#' else is touched
#'
#' @description
#' The reference every scenario is named against: the pathogen exactly as the
#' app hands it to you. Selecting a pathogen fills the sidebar via
#' [reset_pathogen_params()] and leaves the Basic/Advanced toggles alone, and
#' Basic is what those toggles start on, so this is the Basic rendering of the
#' preset combined with the top-level intervention defaults in
#' [PROPOSE_DEFAULTS].
#'
#' A scenario is described by how it departs from this, which is why a
#' just-selected pathogen is named by its pathogen alone. An Advanced-mode
#' scenario departs from it in the ways the Advanced controls allow — using the
#' pathogen's published dispersion rather than the homogeneous simplification,
#' for instance — and is named for those departures, which is the honest
#' description of what the user did.
#'
#' @param pathogen A `character` string naming a pathogen preset, such as
#' `"sars"`.
#'
#' @return A canonical parameter list. See [canonical].
#' @keywords internal
preset_params <- function(pathogen) {
  defaults <- PROPOSE_DEFAULTS[[pathogen]]
  incubation <- delay_summary(
    defaults$incubation_distribution,
    if (identical(defaults$incubation_distribution, "lnorm")) {
      defaults$incubation_meanlog
    } else {
      defaults$incubation_shape
    },
    if (identical(defaults$incubation_distribution, "lnorm")) {
      defaults$incubation_sdlog
    } else {
      defaults$incubation_scale
    }
  )
  onset_to_isolation <- delay_summary(
    PROPOSE_DEFAULTS$onset_to_isolation_distribution,
    PROPOSE_DEFAULTS$onset_to_isolation_meanlog,
    PROPOSE_DEFAULTS$onset_to_isolation_sdlog
  )
  # built through canonical_params() rather than assembled directly, so the
  # reference is produced by exactly the code path a real scenario goes through
  canonical_params(list(
    transmissibility_ui = "basic",
    basic_community_r0 = defaults$community_r0,
    basic_isolated_r0 = defaults$isolated_r0,
    basic_transmission_variability = "homogeneous",
    asymptomatic_transmissibility_different = FALSE,
    incubation_ui = "basic",
    basic_incubation_mean = incubation$mean,
    basic_incubation_variability = "moderate",
    isolation_on = PROPOSE_DEFAULTS$isolation_on,
    onset_to_isolation_ui = "basic",
    basic_onset_to_isolation_mean = onset_to_isolation$mean,
    basic_onset_to_isolation_variability = "moderate",
    asymptomatic = defaults$asymptomatic,
    presymptomatic_transmission = defaults$presymptomatic_transmission,
    symptomatic_traced = PROPOSE_DEFAULTS$symptomatic_traced,
    quarantine = PROPOSE_DEFAULTS$quarantine,
    test_sensitivity = PROPOSE_DEFAULTS$test_sensitivity,
    npi_activation_day = PROPOSE_DEFAULTS$npi_activation_day
  ))
}

#' Describe an offspring distribution in words
#'
#' @description
#' The three dispersion values the Basic transmissibility control offers are
#' named rather than printed, because "superspreading" says what `k = 0.1` means
#' and is shorter. Any other Negative Binomial dispersion is printed exactly.
#'
#' @inheritParams offspring_fields
#'
#' @return A `character` string, or `NA_character_` when the distribution does
#' not apply.
#' @keywords internal
transmission_summary <- function(distribution, disp) {
  if (is.null(distribution) || length(distribution) != 1L || is.na(distribution)) {
    return(NA_character_)
  }
  if (!identical(distribution, "nbinom")) {
    return(unname(DISTRIBUTION_LABELS[distribution]))
  }
  named <- c(
    homogeneous = "homogeneous",
    moderate = "variable",
    high = "superspreading"
  )
  match <- names(BASIC_K)[vapply(BASIC_K, identical, logical(1), disp)]
  if (length(match) == 1L) {
    return(unname(named[[match]]))
  }
  sprintf("k %s", signif(disp, 3))
}

#' Describe a scenario's contact tracing regime in words
#'
#' @description
#' Collapses the isolation switch and the contact tracing percentage into the
#' one phrase that says what response is in play. Contact tracing is abbreviated
#' to "CT" because this phrase appears in every scenario name, including in plot
#' legends and in a table header with one column per scenario.
#'
#' The percentage is stated only when it is not the default in
#' [PROPOSE_DEFAULTS], so a scenario running the app's standard response reads
#' "CT" rather than "CT 80%".
#'
#' @param isolation_on A `logical` scalar: whether cases are isolated at all.
#' @param tracing A `numeric` scalar: the percentage of contacts traced, or `NA`
#' when isolation is off and tracing therefore cannot apply.
#'
#' @return A `character` string.
#' @keywords internal
intervention_summary <- function(isolation_on, tracing) {
  if (isFALSE(isolation_on)) {
    return("no isolation")
  }
  if (is.na(tracing) || isTRUE(all.equal(tracing, 0))) {
    return("no CT")
  }
  if (isTRUE(all.equal(tracing, PROPOSE_DEFAULTS$symptomatic_traced))) {
    return("CT")
  }
  sprintf("CT %s%%", signif(tracing, 3))
}

#' Reduce a scenario to the quantities its name is built from
#'
#' @description
#' A canonical parameter list (see [canonical]) records what the simulation
#' needs: distribution families and their parameters. Those are the wrong things
#' to name a scenario after, because the same epidemiology is expressed
#' differently depending on whether it was entered through the Basic or the
#' Advanced controls — a pathogen selected in one mode differs from the same
#' pathogen selected in the other in eight or nine canonical parameters, none of
#' which a user would call a difference.
#'
#' This reduces a scenario to quantities that survive that translation: a
#' reproduction number, a word for the transmission heterogeneity, delays as a
#' mean and a spread in days, and the intervention settings, which are already
#' expressed the way users think about them.
#'
#' Fields are listed in the order they earn a place in a name, most
#' decision-relevant first, and every canonical parameter is covered by at least
#' one field so that two different scenarios always differ in at least one.
#'
#' @param params A canonical parameter list. See [canonical].
#'
#' @return A named `list` of scalars.
#' @keywords internal
scenario_summary <- function(params) {
  incubation <- delay_summary(
    params$incubation_distribution, params$incubation_par1, params$incubation_par2
  )
  isolation_delay <- delay_summary(
    params$onset_to_isolation_distribution,
    params$onset_to_isolation_par1,
    params$onset_to_isolation_par2
  )
  list(
    intervention = intervention_summary(
      params$isolation_on, params$symptomatic_traced
    ),
    r0 = params$community_r0,
    quarantine = params$quarantine,
    transmission = transmission_summary(
      params$community_distribution, params$community_disp
    ),
    incubation_mean = incubation$mean,
    delay_mean = isolation_delay$mean,
    asymptomatic = params$asymptomatic_pct,
    presymptomatic = params$presymptomatic_transmission,
    sensitivity = params$test_sensitivity,
    npi_day = params$npi_activation_day,
    isolated_r0 = params$isolated_r0,
    asymptomatic_r0 = params$asymptomatic_r0,
    incubation_sd = incubation$sd,
    delay_sd = isolation_delay$sd,
    isolated_transmission = transmission_summary(
      params$isolated_distribution, params$isolated_disp
    ),
    asymptomatic_transmission = transmission_summary(
      params$asymptomatic_distribution, params$asymptomatic_disp
    ),
    incubation_shape = params$incubation_distribution,
    delay_shape = params$onset_to_isolation_distribution
  )
}

#' The phrases describing how a scenario departs from its pathogen preset
#'
#' @param params A canonical parameter list. See [canonical].
#' @param pathogen A `character` string naming the pathogen preset the scenario
#' was built from.
#' @param fields A `character` vector of [scenario_summary()] fields to consider.
#' Defaults to every field except the [SUMMARY_TIEBREAKERS].
#'
#' @return A `character` vector of phrases, in the order they should appear in a
#' name. Empty when the scenario is the preset untouched.
#' @keywords internal
scenario_terms <- function(params,
                           pathogen,
                           fields = setdiff(names(SUMMARY_TERMS), SUMMARY_TIEBREAKERS)) {
  summary <- scenario_summary(params)
  reference <- scenario_summary(preset_params(pathogen))

  differs <- function(field) {
    a <- summary[[field]]
    b <- reference[[field]]
    if (is.na(a) && is.na(b)) {
      return(FALSE)
    }
    if (is.na(a) || is.na(b)) {
      return(TRUE)
    }
    !isTRUE(all.equal(a, b))
  }
  # The contact tracing regime is the one field named whether or not it differs
  # from the preset: it is what this page exists to compare, so every scenario
  # states which response it is running. Everything else is named only when it
  # departs from the preset, which is what keeps names short.
  changed <- Filter(
    function(field) identical(field, "intervention") || differs(field), fields
  )

  if (isFALSE(params$isolation_on)) {
    changed <- setdiff(changed, NO_ISOLATION_IMPLIES)
  }
  vapply(
    changed,
    function(field) SUMMARY_TERMS[[field]](summary[[field]]),
    character(1),
    USE.NAMES = FALSE
  )
}

#' Name a set of scenarios
#'
#' @description
#' Every scenario is named by the same rule regardless of when it was added:
#' the pathogen it was built from, followed by the ways it departs from that
#' pathogen's preset. Nothing is named relative to another scenario, so adding,
#' removing or reordering scenarios never renames the others, and no scenario is
#' singled out as a baseline.
#'
#' Two names are returned for each scenario. The **full** name lists every
#' departure and is used where there is room for it. The **short** name keeps as
#' many of the highest-priority departures as fit in [SHORT_NAME_CHARS] and is
#' used in plot legends, table headers and value box titles.
#'
#' Short names are made unique by lengthening rather than by numbering: a
#' scenario whose short name collides with another's gets more of its departures
#' until it is distinct. Because scenarios that are identical in every parameter
#' are refused when they are added, this resolves unless two scenarios differ
#' only in something the summary does not describe, and only then does a
#' number appear.
#'
#' @param scenarios A `list` of canonical parameter lists. See [canonical].
#' @param pathogens A `character` vector of pathogen preset keys, one per
#' scenario, as selected in [patho_param_input()].
#'
#' @return A `list` with two `character` vectors, `short` and `full`, one
#' element per scenario.
#' @keywords internal
scenario_names <- function(scenarios, pathogens) {
  if (length(scenarios) == 0L) {
    return(list(short = character(), full = character()))
  }
  shown <- vapply(pathogens, pathogen_label, character(1), USE.NAMES = FALSE)
  described <- lapply(
    seq_along(scenarios),
    function(i) scenario_terms(scenarios[[i]], pathogens[i])
  )
  # scenarios that describe themselves identically need the fields held back for
  # exactly this purpose; everything else is named without them
  signature <- vapply(
    seq_along(scenarios),
    function(i) paste(c(shown[i], described[[i]]), collapse = "|"),
    character(1)
  )
  ambiguous <- signature %in% signature[duplicated(signature)]
  terms <- lapply(seq_along(scenarios), function(i) {
    if (!ambiguous[i]) {
      return(described[[i]])
    }
    c(
      described[[i]],
      scenario_terms(scenarios[[i]], pathogens[i], fields = SUMMARY_TIEBREAKERS)
    )
  })

  available <- vapply(terms, length, integer(1))
  assemble <- function(i, n) {
    base <- if (n == 0L || available[i] == 0L) {
      shown[i]
    } else {
      paste0(shown[i], ", ", paste(utils::head(terms[[i]], n), collapse = ", "))
    }
    # an ellipsis signals that the scenario list and the tooltip say more
    if (n < available[i]) paste0(base, ", …") else base
  }
  full <- vapply(
    seq_along(scenarios), function(i) assemble(i, available[i]), character(1)
  )

  # as many leading phrases as fit, then lengthen only the names that collide
  kept <- vapply(seq_along(scenarios), function(i) {
    n <- 0L
    while (n < available[i] && nchar(assemble(i, n + 1L)) <= SHORT_NAME_CHARS) {
      n <- n + 1L
    }
    n
  }, integer(1))

  repeat {
    short <- vapply(seq_along(scenarios), function(i) assemble(i, kept[i]), character(1))
    clashing <- short %in% short[duplicated(short)]
    extendable <- clashing & kept < available &
      vapply(
        seq_along(scenarios),
        function(i) nchar(assemble(i, kept[i] + 1L)) <= SHORT_NAME_MAX_CHARS,
        logical(1)
      )
    if (!any(extendable)) {
      break
    }
    kept[extendable] <- kept[extendable] + 1L
  }

  # a number only where saying more would make the name unreadable, or where two
  # scenarios differ solely in something no phrase describes
  duplicates <- short %in% short[duplicated(short)]
  if (any(duplicates)) {
    ordinal <- cumsum(duplicates)
    short[duplicates] <- sprintf("%s (%d)", short[duplicates], ordinal[duplicates])
  }
  list(short = short, full = full)
}
