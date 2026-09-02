#' Measure the runtime of one outbreak simulation
#'
#' @description
#' Times 5 simulation replicates with the settings the user has actually
#' chosen, and returns the cost of one. The runtime measurement provides
#' an accurate estimate for the environment (R locally, Shiny server,
#' WebAssembly).
#'
#' Falls back to `RUNTIME_SECONDS_PER_SIM` if the probe fails or is too quick to
#' time. Random number generating seeds are preserved.
#'
#' @param probe A `function` of one argument `n`, which runs `n` outbreak
#' simulations with the page's current settings. Its return value is ignored.
#'
#' @return A `numeric` scalar: estimated seconds per simulation.
#' @keywords internal
measure_seconds_per_sim <- function(probe) {
  # restore seed after consuming random draws
  global <- globalenv()
  had_seed <- exists(".Random.seed", envir = global, inherits = FALSE)
  if (had_seed) {
    old_seed <- get(".Random.seed", envir = global, inherits = FALSE)
  }
  on.exit({
    if (had_seed) {
      assign(".Random.seed", old_seed, envir = global)
    } else if (exists(".Random.seed", envir = global, inherits = FALSE)) {
      rm(".Random.seed", envir = global)
    }
  }, add = TRUE)

  probe_replicates <- 5L
  elapsed <- tryCatch(
    system.time(probe(probe_replicates))[["elapsed"]],
    error = function(e) NA_real_
  )
  if (is.na(elapsed) || elapsed <= 0) {
    return(RUNTIME_SECONDS_PER_SIM)
  }
  elapsed / probe_replicates
}

#' Estimate how long an analysis will take (in seconds)
#'
#' @description
#' The number of outbreak simulations an analysis will run, multiplied by the
#' runtime of one, either measured from a probe or assumed from
#' `RUNTIME_SECONDS_PER_SIM`.
#'
#' @param n_sims A `numeric` scalar: the total number of outbreak simulations the
#' analysis will run.
#' @param probe An optional `function` of one argument `n`, as taken by
#' [measure_seconds_per_sim()]. When `NULL` the fixed constant is used.
#'
#' @return A `numeric` scalar: the estimated duration in seconds.
#' @keywords internal
estimate_runtime <- function(n_sims, probe = NULL) {
  if (is.null(probe)) {
    return(n_sims * RUNTIME_SECONDS_PER_SIM)
  }
  n_sims * measure_seconds_per_sim(probe)
}

#' Build the confirmation shown before a long-running analysis
#'
#' @description
#' The single modal used by every analysis page, so that a confirmation reads the
#' same way wherever it appears. It reports both the size of the job and how long
#' it is expected to take, because the number of simulations alone does not tell a
#' non-coder anything about the wait.
#'
#' It also states that the app is unresponsive while an analysis runs. That is the
#' behaviour the ***Proposing Solutions*** workshop reported as the app appearing
#' broken, and saying so in advance is most of what stops it reading that way.
#'
#' @param ns A namespace created with [shiny::NS()]. The modal's buttons are
#' `cancel` and `ok` within that namespace, which each page already observes.
#' @param n_sims A `numeric` scalar: the number of outbreak simulations to run.
#' @param seconds A `numeric` scalar: the estimated duration (seconds), from
#' [estimate_runtime()].
#'
#' @return A [shiny::modalDialog()] object.
#' @keywords internal
runtime_modal <- function(ns, n_sims, seconds) {

  # round up for conservative runtime estimate
  minutes <- max(1L, as.integer(ceiling(seconds / 60)))
  runtime <- paste(minutes, if (minutes == 1L) "minute" else "minutes")

  modalDialog(
    title = "This analysis will take a while",
    tagList(
      tags$p(
        # scientific = FALSE: format() would otherwise render a round
        # count such as 100000 as "1e+05"
        "This will run ",
        tags$b(format(n_sims, big.mark = ",", scientific = FALSE)),
        " outbreak simulations, which is expected to take ",
        tags$b(runtime), "."
      ),
      tags$p(
        class = "text-muted",
        "The app will not respond until the analysis has finished. The estimate
         is approximate: how long a simulation takes depends on the parameters,
         and on the speed of the device or server running it."
      )
    ),
    footer = tagList(
      actionButton(ns("cancel"), "Cancel"),
      actionButton(ns("ok"), "Run", class = "btn btn-danger")
    )
  )
}
