#' Shiny UI for ***Explore*** page
#'
#' @inheritParams shiny::moduleServer
#'
#' @return Output from [shiny::tagList()].
#' @keywords internal
explore_ui <- function(id) {
  ns <- NS(id)

  tagList(
    useShinyFeedback(),
    use_waiter(),

    # CSS to add margin around accordions
    tags$head(
      tags$style(HTML("
      .accordion-item {
        margin-top: 15px !important;
        margin-bottom: 15px !important;
        border-top-width: 5px !important;
      }

       /* Allow the dropdown to infront of the card */
        .bslib-card.allow-overflow,
        .bslib-card.allow-overflow .card-body {
          overflow: visible !important;
        }
    "))
    ),

    page_title("Explore Outbreak Scenarios"),

    sidebarLayout(
      sidebarPanel(
        replicates_input(ns = ns),
        initial_cases_input(ns = ns),
        div(
          class = "d-flex gap-2 mb-3",
          actionButton(
            ns("simulate"),
            "Simulate outbreak",
            class = "btn-primary flex-fill text-wrap px-2",
            style = "min-width: 0;"
          ),
          actionButton(
            ns("reset"),
            "Reset Defaults",
            class = "btn-outline-secondary flex-fill text-wrap px-2",
            icon = icon("rotate-left"),
            style = "min-width: 0;"
          )
        ),
        tags$b("Pathogen Parameters"),
        patho_param_input(ns = ns),
        offspring_input(ns = ns),
        delays_input(ns = ns, delay_type = "incubation"),
        symptom_event_prob_input(ns = ns),
        tags$b("Intervention Parameters"),
        intervention_input(
          ns = ns,
          isolation_switch = TRUE,
          contact_tracing = TRUE
        ),
        tags$b("Simulation Control Parameters"),
        sim_input(ns = ns)
      ),
      mainPanel(
        accordion(
          open = FALSE,
          accordion_panel(
            title = "Show simulation parameter distributions",
            icon = bs_icon("bar-chart-line"),
            navset_card_underline(
              id = ns("dist_tabs"),
              nav_panel(
                "Offspring distribution",
                plotOutput(ns("offspring_dist_plot"))
              ),
              nav_panel(
                "Incubation period",
                plotOutput(ns("incubation_dist_plot"))
              ),
              nav_panel(
                "Onset-to-isolation",
                plotOutput(ns("onset_to_isolation_dist_plot"))
              ),
              nav_panel(
                "Presymptomatic transmission",
                plotOutput(ns("presymptomatic_dist_plot"))
              )
            )
          )
        ),

        value_box(
          title = "Probability of outbreak control",
          value = uiOutput(ns("extinct")),
          uiOutput(ns("extinct_ci")),
          showcase = bs_icon("virus"),
          theme = "bg-gradient-blue-purple"
        ),

        card(
          card_header(
            class = "d-flex justify-content-between align-items-center",
            "Outbreak Projections",
            div(class = "custom-pill-toggle",
                radioButtons(
                  inputId = ns("plot_style"),
                  label = NULL,
                  choices = c("Trajectories" = "indiv", "Mean & CI" = "summary"),
                  inline = TRUE
                )
            )
          ),
          navset_card_underline(
            nav_panel("Cumulative cases", plotOutput(ns("cumulative_cases"))),
            nav_panel("Weekly cases", plotOutput(ns("weekly_cases")))
          )
        )
      )
    )
  )
}

#' Shiny server for ***Explore*** page
#'
#' @inheritParams shiny::moduleServer
#'
#' @return Output from [shiny::moduleServer()].
#' @keywords internal
explore_server <- function(id) {
  moduleServer(id, function(input, output, session) {

    ns <- session$ns

    observeEvent(input$pathogen_defaults, {
      defaults <- PROPOSE_DEFAULTS[[input$pathogen_defaults]]
      reset_pathogen_params(session = session, defaults = defaults)
    })

    # User-input checking with feedback ---------------------------------------
    offspring_feedback_server(input)
    symptom_event_prob_feedback_server(input)
    contact_tracing_feedback_server(input)
    test_sensitivity_feedback_server(input)
    npi_activation_day_feedback_server(input)
    sim_feedback_server(input)

    # hide the onset-to-isolation distribution tab when isolation is switched
    # off, since the delay is not used by the simulation in that case.
    observeEvent(input$isolation_on, {
      if (isTRUE(input$isolation_on)) {
        nav_show("dist_tabs", target = "Onset-to-isolation")
      } else {
        nav_hide("dist_tabs", target = "Onset-to-isolation")
      }
    })

    community <- reactive({
      if (isTRUE(input$transmissibility_ui == "basic")) {
        req(!is.na(input$basic_community_r0), input$basic_community_r0 >= 0)
        # guard for startup where input is NULL before the radioButtons registers
        req(input$basic_transmission_variability)
        k <- BASIC_K[[input$basic_transmission_variability]]
        \(n) rnbinom(n = n, mu = input$basic_community_r0, size = k)
      } else if (input$community_offspring_distribution == "nbinom") {
        req(input$community_r0 >= 0)
        \(n) rnbinom(n = n, mu = input$community_r0, size = input$community_disp)
      } else if (input$community_offspring_distribution == "pois") {
        req(input$community_r0 >= 0)
        \(n) rpois(n = n, lambda = input$community_r0)
      } else if (input$community_offspring_distribution == "geom") {
        req(input$community_r0 >= 0)
        \(n) rgeom(n = n, prob = 1 / (1 + input$community_r0))
      }
    })

    isolated <- reactive({
      if (isTRUE(input$transmissibility_ui == "basic")) {
        req(!is.na(input$basic_isolated_r0), input$basic_isolated_r0 >= 0)
        # guard for startup where input is NULL before the radioButtons registers
        req(input$basic_transmission_variability)
        k <- BASIC_K[[input$basic_transmission_variability]]
        \(n) rnbinom(n = n, mu = input$basic_isolated_r0, size = k)
      } else if (input$isolated_offspring_distribution == "nbinom") {
        \(n) rnbinom(n = n, mu = input$isolated_r0, size = input$isolated_disp)
      } else if (input$isolated_offspring_distribution == "pois") {
        \(n) rpois(n = n, lambda = input$isolated_r0)
      } else if (input$isolated_offspring_distribution == "geom") {
        \(n) rgeom(n = n, prob = 1 / (1 + input$isolated_r0))
      }
    })

    incubation_period <- reactive({
      if (isTRUE(input$incubation_ui == "basic")) {
        # guard for startup where input is NULL before the radioButtons registers
        req(input$basic_incubation_variability)
        req(!is.na(input$basic_incubation_mean), input$basic_incubation_mean > 0)
        shape <- BASIC_DELAY_SHAPE[[input$basic_incubation_variability]]
        \(n) rgamma(
          n = n,
          shape = shape,
          scale = input$basic_incubation_mean / shape
        )
      } else if (input$incubation_distribution == "lnorm") {
        \(n) rlnorm(
          n = n,
          meanlog = input$incubation_meanlog,
          sdlog = input$incubation_sdlog
        )
      } else if (input$incubation_distribution == "gamma") {
        \(n) rgamma(
          n = n,
          shape = input$incubation_shape,
          scale = input$incubation_scale
        )
      } else if (input$incubation_distribution == "weibull") {
        \(n) rweibull(
          n = n,
          shape = input$incubation_shape,
          scale = input$incubation_scale
        )
      }
    })

    onset_to_isolation <- reactive({
      # isolation switch off: disable isolation entirely, no cases are
      # isolated regardless of the (hidden) contact tracing / quarantine inputs
      if (isFALSE(input$isolation_on)) {
        return(function(n) rep(NO_ISOLATION_DELAY, n))
      }
      if (isTRUE(input$onset_to_isolation_ui == "basic")) {
        # guard for startup where input is NULL before the radioButtons registers
        req(input$basic_onset_to_isolation_variability)
        req(
          !is.na(input$basic_onset_to_isolation_mean),
          input$basic_onset_to_isolation_mean > 0
        )
        shape <- BASIC_DELAY_SHAPE[[input$basic_onset_to_isolation_variability]]
        \(n) rgamma(
          n = n,
          shape = shape,
          scale = input$basic_onset_to_isolation_mean / shape
        )
      } else if (input$onset_to_isolation_distribution == "lnorm") {
        \(n) rlnorm(
          n = n,
          meanlog = input$onset_to_isolation_meanlog,
          sdlog = input$onset_to_isolation_sdlog
        )
      } else if (input$onset_to_isolation_distribution == "gamma") {
        \(n) rgamma(
          n = n,
          shape = input$onset_to_isolation_shape,
          scale = input$onset_to_isolation_scale
        )
      } else if (input$onset_to_isolation_distribution == "weibull") {
        \(n) rweibull(
          n = n,
          shape = input$onset_to_isolation_shape,
          scale = input$onset_to_isolation_scale
        )
      }
    })


    # asymptomatic offspring, only used when the advanced UI toggle is enabled
    # (named to avoid confusion with input$asymptomatic, the % asymptomatic)
    asymptomatic_offspring <- reactive({
      req(input$asymptomatic_offspring_distribution)
      req(!is.na(input$asymptomatic_r0), input$asymptomatic_r0 >= 0)
      if (input$asymptomatic_offspring_distribution == "nbinom") {
        req(!is.na(input$asymptomatic_disp), input$asymptomatic_disp > 0)
        \(n) rnbinom(n = n, mu = input$asymptomatic_r0, size = input$asymptomatic_disp)
      } else if (input$asymptomatic_offspring_distribution == "pois") {
        \(n) rpois(n = n, lambda = input$asymptomatic_r0)
      } else if (input$asymptomatic_offspring_distribution == "geom") {
        \(n) rgeom(n = n, prob = 1 / (1 + input$asymptomatic_r0))
      }
    })

    offspring <- reactive({
      if (isTRUE(input$transmissibility_ui == "advanced") &&
          isTRUE(input$asymptomatic_transmissibility_different)) {
        offspring_opts(
          community = community(),
          isolated = isolated(),
          asymptomatic = asymptomatic_offspring()
        )
      } else {
        offspring_opts(community = community(), isolated = isolated())
      }
    })

    delays <- reactive({
      delay_opts(
        incubation_period = incubation_period(),
        onset_to_isolation = onset_to_isolation()
      )
    })

    simulate <- reactiveVal(0L)

    observeEvent(input$simulate, {
      if (input$replicates > 50) {
        showModal(modalDialog(
          title = "Warning: Running lots of replicates!",
          "This may take a considerable amount of time to simulate.",
          footer = tagList(
            actionButton(ns("cancel"), "Cancel"),
            actionButton(ns("ok"), "Run", class = "btn btn-danger")
          )
        ))
      } else {
        simulate(simulate() + 1L)
      }
    })

    observeEvent(input$ok, {
      simulate(simulate() + 1L)
      removeModal()
    })
    observeEvent(input$cancel, {
      removeModal()
    })

    loading <- tagList(
      spin_hexdots(),
      h3("Simulating Outbreaks.", style = "color: #000080; margin-top: 40px;")
    )

    scenario <- eventReactive(simulate(), {
      req(simulate() > 0)
      waiter_show(
        html = loading,
        color = transparent(0.75)
      )
      on.exit(waiter_hide())
      req(input$asymptomatic >= 0 && input$asymptomatic <= 100)
      req(input$presymptomatic_transmission >= 0 && input$presymptomatic_transmission <= 100)
      req(input$symptomatic_traced >= 0 && input$symptomatic_traced <= 100)
      req(input$test_sensitivity >= 0 && input$test_sensitivity <= 1)
      req(!is.na(input$npi_activation_day), input$npi_activation_day >= 0)
      req(input$cap_max_days >= 1)
      req(input$cap_cases >= 1)

      # default to random seed if not specified by user
      if (is.na(input$seed)) {
        set.seed(runif(n = 1, min = 1, max = 1e5))
      } else {
        set.seed(input$seed)
      }

      scenario_sim(
        n = input$replicates,
        initial_cases = input$initial_cases,
        offspring = offspring(),
        delays = delays(),
        event_probs = event_prob_opts(
          # UI collects percentages; the model expects proportions (0-1)
          asymptomatic = input$asymptomatic / 100,
          presymptomatic_transmission = input$presymptomatic_transmission / 100,
          # UI collects a percentage; the model expects a proportion (0-1).
          # Gate the value so NPIs only activate from `npi_activation_day`.
          symptomatic_traced = npi_activation(
            input$symptomatic_traced / 100, input$npi_activation_day
          )
        ),
        interventions = intervention_opts(
          quarantine = input$quarantine,
          test_sensitivity = npi_activation(
            input$test_sensitivity, input$npi_activation_day
          )
        ),
        sim = sim_opts(cap_max_days = input$cap_max_days, cap_cases = input$cap_cases)
      )
    })

    # capped outbreak to prevent overshooting the cumulative cases cap for
    # plotting, extinct and uncapped replicates are unchanged
    scenario_capped <- reactive({
      dt <- data.table::copy(scenario())
      cap <- input$cap_cases
      dt[, cumulative := pmin(cumulative, cap), by = sim]
      dt[, weekly_cases := cumulative - data.table::shift(cumulative, fill = 0), by = sim]
      dt[]
    })
    # probability of outbreak control (proportion of replicates controlled) with
    # a Clopper-Pearson exact 95% CI
    control_stats <- reactive({
      scen <- scenario()
      n <- max(scen$sim)
      k <- sum(detect_extinct(scen)$extinct)
      ci <- stats::binom.test(k, n)$conf.int
      list(p = k / n, lower = ci[1], upper = ci[2])
    })
    output$extinct <- renderText(signif(control_stats()$p, digits = 2))
    output$extinct_ci <- renderUI({
      s <- control_stats()
      tags$small(
        class = "text-white-50",
        sprintf(
          "95%% CI: %s – %s",
          signif(s$lower, digits = 2),
          signif(s$upper, digits = 2)
        )
      )
    })
    output$cumulative_cases <- renderPlot({
      if (input$plot_style == "indiv") {
        outbreak <- scenario_capped()
        outbreak <- outbreak[, head(.SD, which.max(cumulative)), by = sim]
        outbreak_end <- outbreak[, .SD[.N], by = sim]
        tinyplot(
          cumulative ~ week | as.factor(sim),
          data = outbreak,
          type = "l",
          lwd = 3,
          ylab = "Cumulative number of cases",
          xlab = "Week",
          legend = FALSE,
          theme = "clean"
        )
        tinyplot_add(
          cumulative ~ week | as.factor(sim),
          data = outbreak_end,
          type = "p",
          pch = 19,
          cex = 1.2
        )
      } else {
        # aggregate data: calculate mean, lower CI (2.5%), and upper CI (97.5%)
        summ <- aggregate(cumulative ~ week, data = scenario_capped(), FUN = function(x) {
          c(avg = mean(x), lwr = quantile(x, 0.025), upr = quantile(x, 0.975))
        })
        # convert matrix output to columns
        summ <- do.call(data.frame, summ)
        names(summ) <- c("week", "mean", "lwr", "upr")

        tinyplot(
          mean ~ week,
          data = summ,
          type = "ribbon",
          lwd = 3,
          ymin = summ$lwr, # Define the bottom of the CI ribbon
          ymax = summ$upr, # Define the top of the CI ribbon
          fill = "skyblue",
          col = "steelblue",
          ylab = "Cumulative number of cases (Mean & 95% CI)",
          xlab = "Week",
          theme = "clean"
        )
      }
      abline(v = input$cap_max_days / 7, lty = 2, col = "grey50")
      abline(h = input$cap_cases, lty = 2, col = "grey50")
    }

    )
    output$weekly_cases <- renderPlot({
      if (input$plot_style == "indiv") {
        outbreak <- scenario_capped()
        outbreak <- outbreak[, head(.SD, which.max(cumulative)), by = sim]
        outbreak_end <- outbreak[, .SD[.N], by = sim]
        tinyplot(
          weekly_cases ~ week | as.factor(sim),
          data = outbreak,
          type = "l",
          lwd = 3,
          ylab = "Number of cases per week",
          xlab = "Week",
          legend = FALSE,
          theme = "clean"
        )
        tinyplot_add(
          weekly_cases ~ week | as.factor(sim),
          data = outbreak_end,
          type = "p",
          pch = 19,
          cex = 1.2
        )

      } else {
        # mean & CI
        summ_w <- aggregate(weekly_cases ~ week, data = scenario_capped(), FUN = function(x) {
          c(mean = mean(x), lwr = quantile(x, 0.025), upr = quantile(x, 0.975))
        })
        summ_w <- do.call(data.frame, summ_w)
        names(summ_w) <- c("week", "mean", "lwr", "upr")

        tinyplot(
          mean ~ week,
          data = summ_w,
          type = "ribbon",
          lwd = 3,
          ymin = summ_w$lwr,
          ymax = summ_w$upr,
          fill = "skyblue",
          col = "steelblue",
          ylab = "Weekly cases (Mean & 95% CI)",
          xlab = "Week",
          theme = "clean"
        )
      }
      abline(v = input$cap_max_days / 7, lty = 2, col = "grey50")
    }
    )

    # Parameter distribution plots ------------------------------------------
    output$offspring_dist_plot <- offspring_dist_plot(input)
    output$incubation_dist_plot <- incubation_dist_plot(input)
    output$onset_to_isolation_dist_plot <- onset_to_isolation_dist_plot(input)
    output$presymptomatic_dist_plot <- presymptomatic_dist_plot(input)

    observeEvent(input$reset, {
      # set pathogen_defaults back to default
      updateSelectInput(session, "pathogen_defaults", selected = "disease_x")

      defaults <- PROPOSE_DEFAULTS[[input$pathogen_defaults]]
      reset_pathogen_params(session = session, defaults = defaults)

      # reset non-pathogen parameters
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
      updateNumericInput(session, "cap_max_days", value = PROPOSE_DEFAULTS$cap_max_days)
      updateNumericInput(session, "cap_cases", value = PROPOSE_DEFAULTS$cap_cases)
      updateSliderInput(session, "replicates", value = PROPOSE_DEFAULTS$replicates)
      updateSliderInput(session, "initial_cases", value = PROPOSE_DEFAULTS$initial_cases)
    })
  })
}
