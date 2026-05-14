#' Offspring distribution probability mass at `x`
#'
#' Evaluate the offspring distribution probability mass function (PMF) at the
#' non-negative integer values in `x`. Handles the negative binomial, Poisson
#' and geometric parameterisations used in [ringbp::offspring_opts()].
#'
#' @param distribution A `character` string. One of `"nbinom"`, `"pois"` or
#' `"geom"`.
#' @param r0 A `numeric` value. Mean of the offspring distribution (R0).
#' @param disp A `numeric` value. Dispersion parameter (`k`) of the negative
#' binomial distribution. Ignored when `distribution` is `"pois"` or `"geom"`.
#' @param x A `numeric` vector of non-negative integers at which to evaluate
#' the PMF.
#'
#' @return A `numeric` vector of probabilities the same length as `x`.
#' @keywords internal
offspring_pmf <- function(distribution, r0, disp, x) {
  if (distribution == "nbinom") {
    dnbinom(x, mu = r0, size = disp)
  } else if (distribution == "pois") {
    dpois(x, lambda = r0)
  } else if (distribution == "geom") {
    dgeom(x, prob = 1 / (1 + r0))
  }
}

#' 99th percentile of the offspring distribution
#'
#' Compute the 99th percentile of the offspring distribution. Used to choose
#' a sensible upper limit for the x-axis of [offspring_dist_plot()].
#'
#' @inheritParams offspring_pmf
#'
#' @return A single `numeric` value.
#' @keywords internal
offspring_x_max <- function(distribution, r0, disp) {
  if (distribution == "nbinom") {
    qnbinom(0.99, mu = r0, size = disp)
  } else if (distribution == "pois") {
    qpois(0.99, lambda = r0)
  } else if (distribution == "geom") {
    qgeom(0.99, prob = 1 / (1 + r0))
  }
}

#' Render the offspring distribution PMF for community and isolated cases
#'
#' Builds a [shiny::renderPlot()] that draws the offspring probability mass
#' function for the community and isolated settings on a single plot. Intended
#' to be called inside a module [shiny::moduleServer()] block, with the
#' returned renderer assigned to a [shiny::plotOutput()] in that module.
#'
#' @param input The Shiny `input` reactive of the calling module.
#'
#' @return Output from [shiny::renderPlot()].
#' @keywords internal
offspring_dist_plot <- function(input) {
  renderPlot({
    req(input$community_offspring_distribution)
    req(input$isolated_offspring_distribution)
    req(!is.na(input$community_r0), input$community_r0 >= 0)
    req(!is.na(input$isolated_r0), input$isolated_r0 >= 0)
    if (input$community_offspring_distribution == "nbinom") {
      req(!is.na(input$community_disp), input$community_disp > 0)
    }
    if (input$isolated_offspring_distribution == "nbinom") {
      req(!is.na(input$isolated_disp), input$isolated_disp > 0)
    }

    x_max <- max(
      offspring_x_max(
        input$community_offspring_distribution,
        input$community_r0,
        input$community_disp
      ),
      offspring_x_max(
        input$isolated_offspring_distribution,
        input$isolated_r0,
        input$isolated_disp
      ),
      5
    )
    x <- 0:x_max
    df <- rbind(
      data.frame(
        x = x,
        density = offspring_pmf(
          input$community_offspring_distribution,
          input$community_r0,
          input$community_disp,
          x
        ),
        setting = "Community"
      ),
      data.frame(
        x = x,
        density = offspring_pmf(
          input$isolated_offspring_distribution,
          input$isolated_r0,
          input$isolated_disp,
          x
        ),
        setting = "Isolated"
      )
    )
    tinyplot(
      density ~ x | setting,
      data = df,
      type = type_barplot(beside = TRUE),
      ylab = "Probability",
      xlab = "Number of secondary cases",
      theme = "clean",
      legend = legend("top")
    )
  })
}

#' Render the incubation period probability density
#'
#' Builds a [shiny::renderPlot()] that draws the incubation period probability
#' density function under the user-selected parametric distribution
#' (`"lnorm"`, `"gamma"` or `"weibull"`). Intended to be called inside a
#' module [shiny::moduleServer()] block, with the returned renderer assigned
#' to a [shiny::plotOutput()] in that module.
#'
#' @inheritParams offspring_dist_plot
#'
#' @return Output from [shiny::renderPlot()].
#' @keywords internal
incubation_dist_plot <- function(input) {
  renderPlot({
    req(input$incubation_distribution)
    if (input$incubation_distribution == "lnorm") {
      req(!is.na(input$incubation_meanlog), !is.na(input$incubation_sdlog))
      req(input$incubation_sdlog > 0)
      q_max <- qlnorm(
        0.99,
        meanlog = input$incubation_meanlog,
        sdlog = input$incubation_sdlog
      )
      x <- seq(0, q_max, length.out = 200)
      y <- dlnorm(
        x,
        meanlog = input$incubation_meanlog,
        sdlog = input$incubation_sdlog
      )
    } else if (input$incubation_distribution == "gamma") {
      req(!is.na(input$incubation_shape), !is.na(input$incubation_scale))
      req(input$incubation_shape > 0, input$incubation_scale > 0)
      q_max <- qgamma(
        0.99,
        shape = input$incubation_shape,
        scale = input$incubation_scale
      )
      x <- seq(0, q_max, length.out = 200)
      y <- dgamma(
        x,
        shape = input$incubation_shape,
        scale = input$incubation_scale
      )
    } else if (input$incubation_distribution == "weibull") {
      req(!is.na(input$incubation_shape), !is.na(input$incubation_scale))
      req(input$incubation_shape > 0, input$incubation_scale > 0)
      q_max <- qweibull(
        0.99,
        shape = input$incubation_shape,
        scale = input$incubation_scale
      )
      x <- seq(0, q_max, length.out = 200)
      y <- dweibull(
        x,
        shape = input$incubation_shape,
        scale = input$incubation_scale
      )
    }
    tinyplot(
      y ~ x,
      data = data.frame(x = x, y = y),
      type = "l",
      lwd = 3,
      col = "steelblue",
      ylab = "Density",
      xlab = "Incubation period (days)",
      theme = "clean"
    )
  })
}

#' Render the onset-to-isolation delay probability density
#'
#' Builds a [shiny::renderPlot()] that draws the onset-to-isolation delay
#' probability density function under the user-selected parametric
#' distribution (`"lnorm"`, `"gamma"` or `"weibull"`). Intended to be called
#' inside a module [shiny::moduleServer()] block, with the returned renderer
#' assigned to a [shiny::plotOutput()] in that module.
#'
#' @inheritParams offspring_dist_plot
#'
#' @return Output from [shiny::renderPlot()].
#' @keywords internal
onset_to_isolation_dist_plot <- function(input) {
  renderPlot({
    req(input$onset_to_isolation_distribution)
    if (input$onset_to_isolation_distribution == "lnorm") {
      req(
        !is.na(input$onset_to_isolation_meanlog),
        !is.na(input$onset_to_isolation_sdlog)
      )
      req(input$onset_to_isolation_sdlog > 0)
      q_max <- qlnorm(
        0.99,
        meanlog = input$onset_to_isolation_meanlog,
        sdlog = input$onset_to_isolation_sdlog
      )
      x <- seq(0, q_max, length.out = 200)
      y <- dlnorm(
        x,
        meanlog = input$onset_to_isolation_meanlog,
        sdlog = input$onset_to_isolation_sdlog
      )
    } else if (input$onset_to_isolation_distribution == "gamma") {
      req(
        !is.na(input$onset_to_isolation_shape),
        !is.na(input$onset_to_isolation_scale)
      )
      req(input$onset_to_isolation_shape > 0, input$onset_to_isolation_scale > 0)
      q_max <- qgamma(
        0.99,
        shape = input$onset_to_isolation_shape,
        scale = input$onset_to_isolation_scale
      )
      x <- seq(0, q_max, length.out = 200)
      y <- dgamma(
        x,
        shape = input$onset_to_isolation_shape,
        scale = input$onset_to_isolation_scale
      )
    } else if (input$onset_to_isolation_distribution == "weibull") {
      req(
        !is.na(input$onset_to_isolation_shape),
        !is.na(input$onset_to_isolation_scale)
      )
      req(input$onset_to_isolation_shape > 0, input$onset_to_isolation_scale > 0)
      q_max <- qweibull(
        0.99,
        shape = input$onset_to_isolation_shape,
        scale = input$onset_to_isolation_scale
      )
      x <- seq(0, q_max, length.out = 200)
      y <- dweibull(
        x,
        shape = input$onset_to_isolation_shape,
        scale = input$onset_to_isolation_scale
      )
    }
    tinyplot(
      y ~ x,
      data = data.frame(x = x, y = y),
      type = "l",
      lwd = 3,
      col = "steelblue",
      ylab = "Density",
      xlab = "Onset-to-isolation delay (days)",
      theme = "clean"
    )
  })
}

#' Render the presymptomatic transmission probability density
#'
#' Builds a [shiny::renderPlot()] that draws the skew-normal probability
#' density used by [ringbp::scenario_sim()] to model the timing of transmission
#' relative to the infector's symptom onset. The skew-normal shape parameter
#' `alpha` is derived from the proportion of transmission that occurs before
#' symptom onset. Intended to be called inside a module [shiny::moduleServer()]
#' block, with the returned renderer assigned to a [shiny::plotOutput()] in
#' that module.
#'
#' @inheritParams offspring_dist_plot
#'
#' @return Output from [shiny::renderPlot()].
#' @keywords internal
presymptomatic_dist_plot <- function(input) {
  renderPlot({
    req(!is.na(input$presymptomatic_transmission))
    req(
      input$presymptomatic_transmission >= 0,
      input$presymptomatic_transmission <= 1
    )
    # Match ringbp's parameterisation: xi = 0, omega = 2, alpha derived from
    # the proportion of transmission that occurs before symptom onset.
    opts <- ringbp::event_prob_opts(
      asymptomatic = if (!is.na(input$asymptomatic)) input$asymptomatic else 0,
      presymptomatic_transmission = input$presymptomatic_transmission,
      # symptomatic_traced doesn't influence the skew-normal alpha used below;
      # supply any value in [0, 1] to satisfy event_prob_opts()'s checks.
      symptomatic_traced = 0
    )
    alpha <- opts$alpha
    x_min <- sn::qsn(0.001, xi = 0, omega = 2, alpha = alpha)
    x_max <- sn::qsn(0.999, xi = 0, omega = 2, alpha = alpha)
    x <- seq(x_min, x_max, length.out = 200)
    y <- sn::dsn(x, xi = 0, omega = 2, alpha = alpha)
    tinyplot(
      y ~ x,
      data = data.frame(x = x, y = y),
      type = "l",
      lwd = 3,
      col = "steelblue",
      ylab = "Density",
      xlab = "Time since symptom onset (days)",
      theme = "clean"
    )
    abline(v = 0, lty = 2, col = "grey50")
  })
}
