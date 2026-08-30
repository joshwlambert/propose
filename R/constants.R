# Constants shared across the app.
#
# Every value that is fixed at load time lives here rather than beside the
# function that happens to use it first, so the app's tunable quantities --
# model sentinels, pathogen presets, palettes, thresholds and display labels --
# can be found and changed in one place. Functions live in utils.R and in the
# file for the page or input they serve.
#
# Grouped by what they configure. Note that MAX_SCENARIOS is derived from
# COMPARE_PALETTE at load time, so it must stay after it.

# ---- Simulation model -------------------------------------------------

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

# ---- Pathogen presets -------------------------------------------------

#' Constants describing the pathogens the app offers
#'
#' @description
#' The parameter values behind each pathogen preset, and the names users see for
#' them.
#'
#' @details
#' **`PROPOSE_DEFAULTS`** is a list of lists and values. Each inner list contains
#' default pathogen parameters for the outbreak simulation for a specific
#' pathogen. The non-list values in `PROPOSE_DEFAULTS` are the non-pathogen
#' parameters for the outbreak simulation. `replicates` is itself a list, with
#' one entry per page keyed by the page name [replicates_input()] is called with.
#'
#' @name pathogen_constants
#' @keywords internal
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
    # asymptomatic offspring (defaults mirror community)
    asymptomatic_offspring_distribution = "nbinom",
    asymptomatic_r0 = 2,
    asymptomatic_disp = 1,
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
    # asymptomatic offspring (defaults mirror community)
    asymptomatic_offspring_distribution = "nbinom",
    asymptomatic_r0 = 2.66,
    asymptomatic_disp = 0.1,
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
    # asymptomatic offspring (defaults mirror community)
    asymptomatic_offspring_distribution = "nbinom",
    asymptomatic_r0 = 4.5,
    asymptomatic_disp = 0.32,
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
    # asymptomatic offspring (defaults mirror community)
    asymptomatic_offspring_distribution = "nbinom",
    asymptomatic_r0 = 6.5,
    asymptomatic_disp = 0.23,
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
    # asymptomatic offspring (defaults mirror community)
    asymptomatic_offspring_distribution = "nbinom",
    asymptomatic_r0 = 9.5,
    asymptomatic_disp = 0.5,
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
    # asymptomatic offspring (defaults mirror community)
    asymptomatic_offspring_distribution = "nbinom",
    asymptomatic_r0 = 2.7,
    asymptomatic_disp = 0.16,
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
    # asymptomatic offspring (defaults mirror community)
    asymptomatic_offspring_distribution = "nbinom",
    asymptomatic_r0 = 0.93,
    asymptomatic_disp = 0.26,
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
    # asymptomatic offspring (defaults mirror community)
    asymptomatic_offspring_distribution = "nbinom",
    asymptomatic_r0 = 1.75,
    asymptomatic_disp = 0.5,
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
    # asymptomatic offspring (defaults mirror community)
    asymptomatic_offspring_distribution = "nbinom",
    asymptomatic_r0 = 2.5,
    asymptomatic_disp = 0.3,
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
    # asymptomatic offspring (defaults mirror community)
    asymptomatic_offspring_distribution = "nbinom",
    asymptomatic_r0 = 0.8,
    asymptomatic_disp = 0.6,
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
    # asymptomatic offspring (defaults mirror community)
    asymptomatic_offspring_distribution = "nbinom",
    asymptomatic_r0 = 0.15,
    asymptomatic_disp = 0.2,
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
    # asymptomatic offspring (defaults mirror community)
    asymptomatic_offspring_distribution = "nbinom",
    asymptomatic_r0 = 1.44,
    asymptomatic_disp = 0.8,
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
    # asymptomatic offspring (defaults mirror community)
    asymptomatic_offspring_distribution = "nbinom",
    asymptomatic_r0 = 0.08,
    asymptomatic_disp = 0.2,
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
    # asymptomatic offspring (defaults mirror community)
    asymptomatic_offspring_distribution = "nbinom",
    asymptomatic_r0 = 1.36,
    asymptomatic_disp = 1,
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
    # asymptomatic offspring (defaults mirror community)
    asymptomatic_offspring_distribution = "nbinom",
    asymptomatic_r0 = 2.12,
    asymptomatic_disp = 0.15,
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
  test_sensitivity = 1,
  # day of the outbreak on which NPIs activate (0 = active immediately)
  npi_activation_day = 0,
  # sim controls
  cap_max_days = 100,
  cap_cases = 5000,
  # replicates is per page rather than a single value: pages that simulate one
  # scenario at a time can afford a small default that keeps them responsive,
  # while pages that compare several need enough replicates for the intervals
  # around each to separate. Selected by name in replicates_input().
  replicates = list(
    explore = 5,
    compare = 100,
    tracing_effectiveness = 100,
    tracing_strategies = 20,
    outbreak_size = 100
  ),
  initial_cases = 5
)

#' @rdname pathogen_constants
#' @details
#' **`PATHOGEN_LABELS`** maps each key of the pathogen sublists in
#' `PROPOSE_DEFAULTS` to the name shown to users. It is the single source of
#' truth for these names: it fills the [shiny::selectInput()] in
#' [patho_param_input()] and names the scenarios on the ***Compare*** page, so
#' the two can never disagree.
PATHOGEN_LABELS <- c(
  disease_x = "Disease X",
  covid_19_wt = "COVID-19 (Wild-type)",
  covid_19_alpha = "COVID-19 (Alpha)",
  covid_19_delta = "COVID-19 (Delta)",
  covid_19_omicron = "COVID-19 (Omicron)",
  sars = "SARS",
  mers = "MERS",
  ebola_zaire = "Ebola (Zaire)",
  ebola_sudan = "Ebola (Sudan)",
  marburg = "Marburg",
  influenza_h5n1 = "Influenza (H5N1)",
  influenza_h1n1pdm = "Influenza (H1N1pdm)",
  influenza_h7n9 = "Influenza (H7N9)",
  meningitis_b = "Meningitis B",
  andes_hantavirus = "Andes (Hanta)virus"
)

# ---- Sidebar input controls -------------------------------------------

#' Constants behind the sidebar input controls
#'
#' @description
#' What the Basic-mode controls mean in model terms, and the tooltip text the
#' offspring inputs share. These are what let a user who does not want to think
#' in distribution parameters still describe a pathogen.
#'
#' @details
#' **`BASIC_K`** maps the `basic_transmission_variability` radio choices in
#' [offspring_input()] to the Negative Binomial dispersion parameter (`size`
#' in [stats::rnbinom()]). `"homogeneous"` (`k = Inf`) is equivalent to a
#' Poisson offspring distribution, `"moderate"` (`k = 1`) to a Geometric,
#' and `"high"` (`k = 0.1`) gives strong superspreading.
#'
#' @name input_constants
#' @keywords internal
BASIC_K <- c(
  homogeneous = Inf,
  moderate = 1,
  high = 0.1
)

#' @rdname input_constants
#' @details
#' **`BASIC_DELAY_SHAPE`** maps the basic delay "variability" radio choices in
#' [delays_input()] to the shape parameter of a Gamma distribution (`shape` in
#' [stats::rgamma()]). With the mean held fixed (`scale = mean / shape`),
#' `shape` controls the skewness of the tail (skewness = 2 / sqrt(shape)):
#' `"low"` (`shape = 20`) is near-symmetric, `"moderate"` (`shape = 5`) is a
#' classic right skew, and `"high"` (`shape = 2`) has a long, heavy tail.
BASIC_DELAY_SHAPE <- c(
  low = 20,
  moderate = 5,
  high = 2
)

#' @rdname input_constants
#' @details
#' **`OFFSPRING_TIP`** and **`DISP_TIP`** are the tooltip text for the offspring
#' distribution inputs, shared by the Advanced transmissibility controls in
#' [offspring_input()] and by the R0 sweep in [r0_seq_input()], which offer the
#' same choice of distribution and so must explain it the same way.
#' `OFFSPRING_TIP` describes the choice of distribution; `DISP_TIP` describes
#' the Negative Binomial dispersion parameter that governs superspreading.
#' Neither is to be confused with [shape_tip()] and [scale_tip()], which are
#' functions that build tooltip text for a named distribution and delay.
OFFSPRING_TIP <- shiny::HTML(
  "The probability distribution governing the number of
              secondary cases produced by each infected individual. Negative
              Binomial allows for heterogeneity in transmission (superspreading)
              via the dispersion parameter <em>k</em>; Poisson assumes
              homogeneous transmission; Geometric has more heterogeneity in
              transmission than Poisson, and is a special case of the Negative
              Binomial with <em>k</em> = 1."
)

#' @rdname input_constants
DISP_TIP <- shiny::HTML(
  "The dispersion parameter (<em>k</em>) of the Negative Binomial offspring
    distribution. Lower values indicate greater heterogeneity in
    transmission (i.e. more superspreading), while <em>k</em> = 1 is
    equivalent to the Geometric and large values of <em>k</em>
  approximate a Poisson distribution."
)

# ---- Compare page -----------------------------------------------------

#' Constants for the ***Compare*** page
#'
#' @description
#' What the page looks like and where its limits are: the colours that identify
#' scenarios, how many can be compared at once, and when it stops to ask before
#' running.
#'
#' @details
#' **`COMPARE_PALETTE`** is a qualitative palette, one colour per scenario, used
#' consistently for a scenario's swatch, its value box and its series in every
#' plot. The brand primary leads, followed by colours from the Okabe-Ito
#' palette, which is designed to stay distinguishable with colour vision
#' deficiency.
#'
#' @name compare_constants
#' @keywords internal
COMPARE_PALETTE <- c(
  "#333B76", "#D55E00", "#009E73", "#CC79A7", "#0072B2", "#E69F00"
)

#' @rdname compare_constants
#' @details
#' **`MAX_SCENARIOS`** is the greatest number of scenarios that can be compared
#' at once. It is the length of `COMPARE_PALETTE`, because beyond this many
#' series the layered plots stop being readable whatever the colours. Being
#' derived at load time, it must stay after `COMPARE_PALETTE` in this file.
MAX_SCENARIOS <- length(COMPARE_PALETTE)

#' @rdname compare_constants
#' @details
#' **`COMPARE_SIM_WARNING`** is the number of simulations above which the page
#' asks for confirmation before running. A comparison runs `replicates`
#' simulations for each scenario, so the total grows quickly. This threshold
#' matches the ***Outbreak Size & Length*** page, and is set so that ordinary
#' comparisons at the page's default replicates
#' (`PROPOSE_DEFAULTS$replicates$compare`) run without interruption while the
#' slowest ones still warn: a confirmation that appears every time is one users
#' learn to dismiss without reading.
COMPARE_SIM_WARNING <- 500L

#' @rdname compare_constants
#' @details
#' **`RUN_PROMPT`** is shown in place of the page's results before a comparison
#' has been run, in every card that would otherwise be empty.
RUN_PROMPT <- paste(
  "Add at least two scenarios, then press \"Run comparison\" to simulate them."
)

# ---- Describing and naming a scenario ---------------------------------

#' Constants for describing and naming a scenario
#'
#' @description
#' The vocabulary the ***Compare*** page uses to say what a scenario is: the
#' labels its parameters are shown under, the phrases its name is built from,
#' and the limits on how long that name may get.
#'
#' @details
#' **`PARAM_LABELS`** maps each field of a canonical parameter list (see
#' [canonical]) to the label shown in the page's parameter table. Its names must
#' stay in step with the fields [canonical_params()] returns.
#'
#' @name naming_constants
#' @keywords internal
PARAM_LABELS <- c(
  community_distribution = "Community offspring distribution",
  community_r0 = "Community R0",
  community_disp = "Community dispersion (k)",
  isolated_distribution = "Isolated offspring distribution",
  isolated_r0 = "Isolated R0",
  isolated_disp = "Isolated dispersion (k)",
  asymptomatic_distribution = "Asymptomatic offspring distribution",
  asymptomatic_r0 = "Asymptomatic R0",
  asymptomatic_disp = "Asymptomatic dispersion (k)",
  incubation_distribution = "Incubation period distribution",
  incubation_par1 = "Incubation period parameter 1",
  incubation_par2 = "Incubation period parameter 2",
  isolation_on = "Isolate cases",
  onset_to_isolation_distribution = "Onset-to-isolation distribution",
  onset_to_isolation_par1 = "Onset-to-isolation parameter 1",
  onset_to_isolation_par2 = "Onset-to-isolation parameter 2",
  asymptomatic_pct = "Asymptomatic cases",
  presymptomatic_transmission = "Presymptomatic transmission",
  symptomatic_traced = "Contacts traced",
  quarantine = "Quarantine",
  test_sensitivity = "Test sensitivity",
  npi_activation_day = "NPI activation day"
)

#' @rdname naming_constants
#' @details
#' **`DISTRIBUTION_LABELS`** gives the display name for each distribution code
#' used in canonical parameters, so `"nbinom"` is shown as "Negative Binomial".
DISTRIBUTION_LABELS <- c(
  nbinom = "Negative Binomial",
  pois = "Poisson",
  geom = "Geometric",
  lnorm = "Lognormal",
  gamma = "Gamma",
  weibull = "Weibull"
)

#' @rdname naming_constants
#' @details
#' **`DELAY_PAR_NAMES`** names the two parameters of each delay distribution, in
#' canonical order, so that a row of the parameter table can say "(meanlog)"
#' rather than "parameter 1" when every scenario shares a distribution.
DELAY_PAR_NAMES <- list(
  lnorm = c("meanlog", "sdlog"),
  gamma = c("shape", "scale"),
  weibull = c("shape", "scale")
)

#' @rdname naming_constants
#' @details
#' **`SUMMARY_TERMS`** says how each field of a [scenario_summary()] is written
#' into a scenario name: one function per field, taking the field's value and
#' returning the phrase that appears in the name. Phrases are deliberately
#' terse, because they compete for room in a plot legend and in a table header
#' with one column per scenario.
SUMMARY_TERMS <- list(
  intervention = function(v) v,
  r0 = function(v) sprintf("R0 %s", signif(v, 3)),
  quarantine = function(v) if (isTRUE(v)) "quarantine" else "no quarantine",
  transmission = function(v) v,
  incubation_mean = function(v) sprintf("incubation %sd", v),
  delay_mean = function(v) sprintf("isolation delay %sd", v),
  asymptomatic = function(v) sprintf("%s%% asymptomatic", signif(v, 3)),
  presymptomatic = function(v) sprintf("%s%% presymptomatic", signif(v, 3)),
  sensitivity = function(v) sprintf("test sensitivity %s", signif(v, 3)),
  npi_day = function(v) sprintf("NPIs from day %s", signif(v, 3)),
  isolated_r0 = function(v) sprintf("isolated R0 %s", signif(v, 3)),
  asymptomatic_r0 = function(v) {
    if (is.na(v)) "asymptomatic as community" else sprintf("asymptomatic R0 %s", signif(v, 3))
  },
  incubation_sd = function(v) sprintf("incubation spread %sd", v),
  delay_sd = function(v) sprintf("isolation delay spread %sd", v),
  isolated_transmission = function(v) sprintf("isolated %s", v),
  asymptomatic_transmission = function(v) sprintf("asymptomatic %s", v),
  incubation_shape = function(v) sprintf("incubation %s", DISTRIBUTION_LABELS[[v]]),
  delay_shape = function(v) sprintf("isolation delay %s", DISTRIBUTION_LABELS[[v]])
)

#' @rdname naming_constants
#' @details
#' **`SUMMARY_TIEBREAKERS`** are the summary fields held back for separating
#' scenarios rather than describing them. They exist so that two scenarios are
#' never indistinguishable, not because a user would name a scenario after them.
#' The spread of a delay and the family of its distribution are largely an
#' artefact of which controls the scenario was entered through — the same
#' pathogen entered through the Basic and the Advanced controls has almost the
#' same mean delays but a different distribution family and spread — so putting
#' them in every name would bury the change the user actually made under wording
#' they did not choose. [scenario_names()] therefore uses them only for
#' scenarios whose other phrases do not already tell them apart.
SUMMARY_TIEBREAKERS <- c(
  "incubation_sd", "delay_sd", "isolated_transmission",
  "asymptomatic_transmission", "incubation_shape", "delay_shape"
)

#' @rdname naming_constants
#' @details
#' **`NO_ISOLATION_IMPLIES`** are the summary fields made redundant by isolation
#' being switched off. With no isolation there is no onset-to-isolation delay,
#' nothing to trace and nothing to test, so these fields cannot affect the
#' scenario. "no isolation" says all of it, and naming them as well would bury
#' that under five phrases that mean nothing.
NO_ISOLATION_IMPLIES <- c(
  "quarantine", "delay_mean", "sensitivity", "npi_day",
  "delay_sd", "delay_shape"
)

#' @rdname naming_constants
#' @details
#' **`SHORT_NAME_CHARS`** is the greatest length of a scenario's short name, in
#' characters. Short names appear in plot legends, in a table header with one
#' column per scenario, and in value box titles. Measured against the plots the
#' page draws, a legend entry is comfortable up to about this length and
#' overruns the panel well before twice it.
SHORT_NAME_CHARS <- 34L

#' @rdname naming_constants
#' @details
#' **`SHORT_NAME_MAX_CHARS`** is the length a short name may grow to in order to
#' stay unique. [scenario_names()] distinguishes scenarios by saying more about
#' them rather than by numbering them, which means a short name can exceed
#' `SHORT_NAME_CHARS`. This caps how far that can go before a number is used
#' instead: past roughly this length a legend entry starts to overlap the data
#' it labels, and an unreadable legend is worse than a numbered one.
SHORT_NAME_MAX_CHARS <- 48L

# ---- Restoring a scenario into the sidebar ----------------------------

#' Sidebar inputs describing a scenario, grouped by the function that updates
#' them
#'
#' @name restore_constants
#'
#' @description
#' The inputs [apply_params()] writes back when a scenario is loaded into the
#' sidebar for editing, split by which `shiny::update*Input()` function sets
#' them. Together these cover every input [canonical_params()] reads.
#'
#' The pathogen preset (`pathogen_defaults`) is deliberately **not** restorable.
#' It is a shortcut for filling the pathogen parameters, not a parameter itself,
#' and changing it fires the observer that resets all of them — which would
#' immediately overwrite the scenario being loaded.
#'
#' Each constant names the inputs restored by one `update*Input()` function, so
#' adding a new sidebar input means adding its ID to whichever list matches the
#' control it uses.
#'
#' @details
#' **`RESTORE_SELECT_IDS`** are set with [shiny::updateSelectInput()]: the
#' distribution dropdowns.
#'
#' @keywords internal
RESTORE_SELECT_IDS <- c(
  "community_offspring_distribution",
  "isolated_offspring_distribution",
  "asymptomatic_offspring_distribution",
  "incubation_distribution",
  "onset_to_isolation_distribution"
)

#' @rdname restore_constants
#' @details
#' **`RESTORE_RADIO_IDS`** are set with [shiny::updateRadioButtons()]: the
#' Basic/Advanced mode toggles and the Basic-mode variability choices.
RESTORE_RADIO_IDS <- c(
  "transmissibility_ui",
  "basic_transmission_variability",
  "incubation_ui",
  "basic_incubation_variability",
  "onset_to_isolation_ui",
  "basic_onset_to_isolation_variability"
)

#' @rdname restore_constants
#' @details
#' **`RESTORE_SWITCH_IDS`** are set with [bslib::update_switch()]: the isolation
#' master switch and the distinct-asymptomatic-transmissibility toggle.
RESTORE_SWITCH_IDS <- c(
  "asymptomatic_transmissibility_different",
  "isolation_on"
)

#' @rdname restore_constants
#' @details
#' **`RESTORE_CHECKBOX_IDS`** are set with [shiny::updateCheckboxInput()]:
#' currently just the quarantine checkbox.
RESTORE_CHECKBOX_IDS <- "quarantine"

#' @rdname restore_constants
#' @details
#' **`RESTORE_NUMERIC_IDS`** are set with [shiny::updateNumericInput()]:
#' everything else, being the reproduction numbers, dispersions, delay
#' parameters, percentages and intervention values.
RESTORE_NUMERIC_IDS <- c(
  "basic_community_r0",
  "basic_isolated_r0",
  "community_r0",
  "community_disp",
  "isolated_r0",
  "isolated_disp",
  "asymptomatic_r0",
  "asymptomatic_disp",
  "basic_incubation_mean",
  "incubation_meanlog",
  "incubation_sdlog",
  "incubation_shape",
  "incubation_scale",
  "basic_onset_to_isolation_mean",
  "onset_to_isolation_meanlog",
  "onset_to_isolation_sdlog",
  "onset_to_isolation_shape",
  "onset_to_isolation_scale",
  "asymptomatic",
  "presymptomatic_transmission",
  "symptomatic_traced",
  "test_sensitivity",
  "npi_activation_day"
)
