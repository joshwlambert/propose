#' \pkg{ringbp} simulation defaults used in \pkg{propose}
#'
#' @name defaults
#'
#' @description
#' `PROPOSE_DEFAULTS` is a list with the default outbreak simulation parameters.

#' @rdname defaults
PROPOSE_DEFAULTS <- list(
  # offspring
  community_offspring_distribution = "nbinom",
  community_r0 = 2,
  community_disp = 1,
  isolated_offspring_distribution = "nbinom",
  isolated_r0 = 0,
  isolated_disp = 1,
  # event probs
  asymptomatic = 0.1,
  presymptomatic_transmission = 0.1,
  symptomatic_traced = 0.8,
  # intervention
  quarantine = FALSE
)
