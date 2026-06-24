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
    # handle different parameterisation from basic and advanced UIs
    if (isTRUE(input$transmissibility_ui == "basic")) {
      # guard for startup where input is NULL before the radioButtons registers
      req(input$basic_transmission_variability)
      k <- BASIC_K[[input$basic_transmission_variability]]
      community_distribution <- "nbinom"
      isolated_distribution <- "nbinom"
      community_r0 <- input$basic_community_r0
      isolated_r0 <- input$basic_isolated_r0
      community_disp <- k
      isolated_disp <- k
    } else {
      community_distribution <- input$community_offspring_distribution
      isolated_distribution <- input$isolated_offspring_distribution
      community_r0 <- input$community_r0
      isolated_r0 <- input$isolated_r0
      community_disp <- input$community_disp
      isolated_disp <- input$isolated_disp
    }

    # validate parameters shared by UI modes
    req(community_distribution, isolated_distribution)
    req(!is.na(community_r0), community_r0 >= 0)
    req(!is.na(isolated_r0), isolated_r0 >= 0)
    if (community_distribution == "nbinom") {
      req(!is.na(community_disp), community_disp > 0)
    }
    if (isolated_distribution == "nbinom") {
      req(!is.na(isolated_disp), isolated_disp > 0)
    }

    x_max <- max(
      offspring_x_max(community_distribution, community_r0, community_disp),
      offspring_x_max(isolated_distribution, isolated_r0, isolated_disp),
      5
    )
    x <- 0:x_max
    df <- rbind(
      data.frame(
        x = x,
        density = offspring_pmf(
          community_distribution,
          community_r0,
          community_disp,
          x
        ),
        setting = "Community"
      ),
      data.frame(
        x = x,
        density = offspring_pmf(
          isolated_distribution,
          isolated_r0,
          isolated_disp,
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
    # handle different parameterisation from basic and advanced UIs
    if (isTRUE(input$incubation_ui == "basic")) {
      # guard for startup where input is NULL before the radioButtons registers
      req(input$basic_incubation_variability)
      req(!is.na(input$basic_incubation_mean), input$basic_incubation_mean > 0)
      shape <- BASIC_DELAY_SHAPE[[input$basic_incubation_variability]]
      scale <- input$basic_incubation_mean / shape
      q_max <- qgamma(0.99, shape = shape, scale = scale)
      x <- seq(0, q_max, length.out = 200)
      y <- dgamma(x, shape = shape, scale = scale)
    } else {
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
#' @param id_prefix A `character` string prepended to the onset-to-isolation
#' input IDs read by this plot (e.g. `"dct_"`). Must match the `id_prefix`
#' passed to the corresponding [onset_to_isolation_input()]. Defaults to `""`
#' (no prefix).
#' @inheritParams tinyplot::tinyplot
#'
#' @return Output from [shiny::renderPlot()].
#' @keywords internal
onset_to_isolation_dist_plot <- function(input,
                                         id_prefix = "",
                                         col = "steelblue") {
  renderPlot({
    df <- onset_to_isolation_density(input, id_prefix = id_prefix)
    tinyplot(
      y ~ x,
      data = df,
      type = "l",
      lwd = 3,
      col = col,
      ylab = "Density",
      xlab = "Onset-to-isolation delay (days)",
      theme = "clean"
    )
  })
}

#' Evaluate the onset-to-isolation delay probability density
#'
#' Compute the onset-to-isolation delay probability density under the
#' user-selected parametric distribution (`"lnorm"`, `"gamma"` or `"weibull"`)
#' for a single contact tracing strategy, reading the strategy's prefixed
#' inputs. Reactive guards ([shiny::req()]) hold rendering until the relevant
#' inputs are valid, so this is intended to be called from within a
#' [shiny::renderPlot()] expression.
#'
#' @inheritParams onset_to_isolation_dist_plot
#'
#' @return A `data.frame` with numeric columns `x` (delay in days) and `y`
#' (density).
#' @keywords internal
onset_to_isolation_density <- function(input, id_prefix = "") {
  # Read the prefixed onset-to-isolation inputs for this strategy
  iid <- function(suffix) input[[paste0(id_prefix, suffix)]]
  distribution <- iid("onset_to_isolation_distribution")
  req(distribution)
  if (distribution == "lnorm") {
    meanlog <- iid("onset_to_isolation_meanlog")
    sdlog <- iid("onset_to_isolation_sdlog")
    req(!is.na(meanlog), !is.na(sdlog))
    req(sdlog > 0)
    q_max <- qlnorm(0.99, meanlog = meanlog, sdlog = sdlog)
    x <- seq(0, q_max, length.out = 200)
    y <- dlnorm(x, meanlog = meanlog, sdlog = sdlog)
  } else if (distribution == "gamma") {
    shape <- iid("onset_to_isolation_shape")
    scale <- iid("onset_to_isolation_scale")
    req(!is.na(shape), !is.na(scale))
    req(shape > 0, scale > 0)
    q_max <- qgamma(0.99, shape = shape, scale = scale)
    x <- seq(0, q_max, length.out = 200)
    y <- dgamma(x, shape = shape, scale = scale)
  } else if (distribution == "weibull") {
    shape <- iid("onset_to_isolation_shape")
    scale <- iid("onset_to_isolation_scale")
    req(!is.na(shape), !is.na(scale))
    req(shape > 0, scale > 0)
    q_max <- qweibull(0.99, shape = shape, scale = scale)
    x <- seq(0, q_max, length.out = 200)
    y <- dweibull(x, shape = shape, scale = scale)
  }
  data.frame(x = x, y = y)
}

#' Render onset-to-isolation delay densities for several strategies together
#'
#' Overlays the onset-to-isolation delay probability density of several contact
#' tracing strategies on a single grouped line plot, reusing
#' [onset_to_isolation_density()] to evaluate each strategy's prefixed inputs.
#'
#' @inheritParams onset_to_isolation_dist_plot
#' @param id_prefixes A named `character` vector mapping legend labels (the
#' names) to input `id_prefix`es (the values), e.g.
#' `c(Digital = "dct_", Manual = "mct_", Informal = "ict_")`.
#' @param palette A `character` vector of colours, one per strategy in the same
#' order as `id_prefixes`, used to colour and order the lines. Pass the same
#' colours used for the individual per-strategy plots to keep them consistent.
#' Defaults to `NULL` (every line `"steelblue"`).
#' @param toggles An optional `character` vector, the same length and order as
#' `id_prefixes`, of checkbox input IDs that switch each strategy on. Only
#' strategies whose toggle is `TRUE` are drawn. Defaults to `NULL` (all drawn).
#'
#' @return Output from [shiny::renderPlot()].
#' @keywords internal
onset_to_isolation_dist_plot_combined <- function(input,
                                                  id_prefixes,
                                                  palette = NULL,
                                                  toggles = NULL) {
  renderPlot({
    cols_all <- if (is.null(palette)) {
      rep_len("steelblue", length(id_prefixes))
    } else {
      rep_len(palette, length(id_prefixes))
    }
    # Keep only the strategies whose checkbox is ticked (all, if no toggles)
    keep <- if (is.null(toggles)) {
      seq_along(id_prefixes)
    } else {
      which(vapply(toggles, function(tg) isTRUE(input[[tg]]), logical(1)))
    }
    req(length(keep) > 0)
    id_prefixes <- id_prefixes[keep]
    labels <- names(id_prefixes)
    cols <- cols_all[keep]
    # Density for each kept strategy from its prefixed inputs
    dens <- lapply(id_prefixes, function(p) {
      onset_to_isolation_density(input, id_prefix = p)
    })
    # Shared plotting region spanning every strategy's curve
    xlim <- range(vapply(dens, function(d) range(d$x), numeric(2)))
    ylim <- range(vapply(dens, function(d) range(d$y), numeric(2)))
    # Draw the first strategy, then overlay the rest, each in its own colour.
    # Explicit per-line colours (rather than a `palette` vector) keep this
    # robust across tinyplot versions.
    tinyplot(
      dens[[1]]$y ~ dens[[1]]$x,
      type = "l",
      lwd = 3,
      col = cols[1],
      xlim = xlim,
      ylim = ylim,
      ylab = "Density",
      xlab = "Onset-to-isolation delay (days)",
      theme = "clean"
    )
    for (i in seq_along(dens)[-1]) {
      tinyplot_add(dens[[i]]$y ~ dens[[i]]$x, type = "l", lwd = 3, col = cols[i])
    }
    legend(
      "top",
      legend = labels,
      col = cols,
      lwd = 3,
      bty = "n",
      horiz = TRUE
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
