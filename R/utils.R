#' \pkg{ringbp} simulation defaults used in \pkg{propose}
#'
#' @name defaults
#'
#' @description
#' `PROPOSE_DEFAULTS` is a list with the default outbreak simulation parameters.

#' @rdname defaults
PROPOSE_DEFAULTS <- list(
  # event probs
  asymptomatic = 0.1,
  presymptomatic_transmission = 0.1,
  symptomatic_traced = 0.8,
  # intervention
  quarantine = FALSE
)
