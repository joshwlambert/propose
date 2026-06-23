#' Shiny UI for ***Contact Tracing Strategies*** page
#'
#' @inheritParams shiny::moduleServer
#'
#' @return Output from [shiny::tagList()].
#' @keywords internal
tracing_strategies_ui <- function(id) {
  ns <- NS(id)

  tagList(
    useShinyFeedback(),
    use_waiter(),

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

    page_title("Contact tracing strategies for outbreak control"),

    sidebarLayout(
      sidebarPanel(
        div(
          class = "d-flex gap-2 mb-3",
          actionButton(
            ns("simulate"),
            "Simulate outbreak(s)",
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
        numericInput(
          ns("replicates"),
          label = tagList(
            "Number of simulation replicates",
            tooltip(
              bsicons::bs_icon("info-circle"),
              "This controls the number of independent outbreaks to simulate."
            )
          ),
          value = 20,
          min = 1
        ),
        numericInput(
          ns("initial_cases"),
          label = tagList(
            "Number of initial cases",
            tooltip(
              bsicons::bs_icon("info-circle"),
              "Number of initially infectious individuals at the start of each
              simulated outbreak. They all seed independent transmission chains
              within the same simulation run."
            )
          ),
          value = 5,
          min = 1
        ),
        tags$b("Pathogen Parameters"),
        card(
          class = "allow-overflow",
          card_header(
            "Select Pathogen Parameters:",
            tooltip(
              bsicons::bs_icon("info-circle"),
              "Pathogen parameters based on estimates published in the literature.
              The default scenario is 'Disease X' and has generic pathogen parameters."
            )
          ),
          selectInput(
            ns("pathogen_defaults"),
            label = "",
            choices = list(
              "Disease X" = "disease_x",
              "COVID-19" = "covid_19",
              "Ebola" = "ebola"
            )
          )
        ),
        offspring_input(ns = ns),
        incubation_input(ns = ns),
        symptom_event_prob_input(ns = ns),
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
              nav_panel(
                "Offspring distribution",
                plotOutput(ns("offspring_dist_plot"))
              ),
              nav_panel(
                "Incubation period",
                plotOutput(ns("incubation_dist_plot"))
              ),
              nav_panel(
                "Presymptomatic transmission",
                plotOutput(ns("presymptomatic_dist_plot"))
              )
            )
          )
        ),
        tags$hr(),
        accordion(
          open = FALSE,
          accordion_panel(
            title = "Digital Contact Tracing",
            icon = bs_icon("phone-vibrate"),
            checkboxInput(
              ns("digital_contact_tracing"),
              label = tagList(
                "Digital Contact Tracing",
                tooltip(
                  bs_icon("info-circle"),
                  "Digital contact tracing is a public health strategy that
                  utilises mobile technology (e.g. Bluetooth proximity sensors
                  and GPS location tracking) to automatically identify and
                  notify individuals who have been in close contact with an
                  infected person. It can rapidly alert potentially exposed
                  people so they can self-isolate or get tested."
                )
              ),
              value = FALSE
            ),
            tags$b("Intervention Parameters"),
            # digital contact tracing parameters
            numericInput(
              ns("app_uptake"),
              label = tagList(
                "Proportion of the population using the contact tracing app",
                tooltip(
                  bs_icon("info-circle"),
                  "The proportion of the population actively using the app.
                  Because digital tracing requires both the infected person
                  and the exposed contact to have the app installed to
                  register an interaction, the probability of a contact
                  being successfully logged scales with the square of the
                  adoption rate."
                )
              ),
              value = 0.5,
              min = 0,
              max = 1,
              step = 0.1,
              width = "100%"
            ),
            numericInput(
              ns("app_sensitivity"),
              label = tagList(
                "Sensitivity of contact tracing app to identify contact",
                tooltip(
                  bs_icon("info-circle"),
                  "The system's ability to accurately measure proximity and
                  duration determines the tracing identification. The system's
                  sensitivity is its ability to successfully detect true
                  epidemiological exposures (e.g., within 2 meters for at
                  least 15 minutes). When app sensitivity is less than 100%
                  some contacts will be missed."
                )
              ),
              value = 0.5,
              min = 0,
              max = 1,
              step = 0.1,
              width = "100%"
            ),
            layout_columns(
              col_widths = c(6, 6),
              onset_to_isolation_input(ns = ns, id_prefix = "dct_"),
              plotOutput(ns("dct_onset_to_isolation_dist_plot"))
            ),
            checkboxInput(
              ns("dct_quarantine"),
              label = tagList(
                "Quarantine",
                tooltip(
                  bs_icon("info-circle"),
                  "When quarantine is enabled, traced contacts are isolated as soon
            as they are identified, regardless of whether they are symptomatic
            (presymptomatic isolation). When disabled, traced contacts are only
            isolated once they show symptoms, allowing for transmission during
            the presymptomatic phase."
                )
              ),
              value = PROPOSE_DEFAULTS$quarantine
            )
          )
        ),
        tags$hr(),
        accordion(
          open = FALSE,
          accordion_panel(
            title = "Manual Contact Tracing",
            icon = bs_icon("person-check"),
            checkboxInput(
              ns("manual_contact_tracing"),
              label = tagList(
                "Manual Contact Tracing",
                tooltip(
                  bs_icon("info-circle"),
                  "Traditional manual contact tracing is a core public health
                  strategy where trained investigators interview confirmed
                  infected patients to identify anyone they may have exposed
                  during their infectious period. Once identified, health
                  workers individually reach out to these potentially exposed
                  individuals to notify them of their risk, provide testing
                  resources, and issue isolation/quarantine directives."
                )
              ),
              value = FALSE
            ),
            tags$b("Intervention Parameters"),
            # manual contact tracing parameters
            numericInput(
              ns("contact_known"),
              label = tagList(
                "Proportion of contacts that are known to the infected
                individual.",
                tooltip(
                  bs_icon("info-circle"),
                  "Proportion of contacts known is the fraction of people that
                  the infected individual has interacted with over their
                  infectious period that are known and could be named by the
                  infector. Interactions with an unknown contact cannot be
                  recalled and thus not traced."
                )
              ),
              value = 0.5,
              min = 0,
              max = 1,
              step = 0.1,
              width = "100%"
            ),
            numericInput(
              ns("contact_recall"),
              label = tagList(
                "Proportion of contacts that can be accurately recalled and
                their willingness to share those details.",
                tooltip(
                  bs_icon("info-circle"),
                  "Contact recall is an infected person’s ability and
                  willingness to accurately remember and disclose every
                  individual they were in close proximity to during their
                  infectious window. We assume contacts are randomly recalled
                  and do not differentiate between contact types (e.g. household
                  versus stranger)."
                )
              ),
              value = 0.5,
              min = 0,
              max = 1,
              step = 0.1,
              width = "100%"
            ),
            numericInput(
              ns("system_coverage"),
              label = tagList(
                "Proportion of reported contacts that are successfully traced",
                tooltip(
                  bs_icon("info-circle"),
                  "The centralised contact tracing system will aim to contact
                  trace all reported contacts that are recalled by infected
                  individuals. However, not all contacts will be successfully
                  contacted and informed by the system. When system coverage
                  is less than 100% some reported contacts will be missed."
                )
              ),
              value = 0.5,
              min = 0,
              max = 1,
              step = 0.1,
              width = "100%"
            ),
            layout_columns(
              col_widths = c(6, 6),
              onset_to_isolation_input(ns = ns, id_prefix = "mct_"),
              plotOutput(ns("mct_onset_to_isolation_dist_plot"))
            ),
            checkboxInput(
              ns("mct_quarantine"),
              label = tagList(
                "Quarantine",
                tooltip(
                  bs_icon("info-circle"),
                  "When quarantine is enabled, traced contacts are isolated as soon
            as they are identified, regardless of whether they are symptomatic
            (presymptomatic isolation). When disabled, traced contacts are only
            isolated once they show symptoms, allowing for transmission during
            the presymptomatic phase."
                )
              ),
              value = PROPOSE_DEFAULTS$quarantine
            )
          )
        ),
        tags$hr(),
        accordion(
          open = FALSE,
          accordion_panel(
            title = "Informal Contact Tracing",
            icon = bs_icon("people"),
            checkboxInput(
              ns("informal_contact_tracing"),
              label = tagList(
                "Informal Contact Tracing",
                tooltip(
                  bs_icon("info-circle"),
                  "Informal contact tracing is ..."
                )
              ),
              value = FALSE
            ),
            tags$b("Intervention Parameters"),
            # informal contact tracing parameters
            numericInput(
              ns("ict_contact_known"),
              label = tagList(
                "Proportion of contacts that are known to the infected
                individual.",
                tooltip(
                  bs_icon("info-circle"),
                  "Proportion of contacts known is the fraction of people that
                  the infected individual has interacted with over their
                  infectious period that are known and could be named by the
                  infector. Interactions with an unknown contact cannot be
                  recalled and thus not traced."
                )
              ),
              value = 0.5,
              min = 0,
              max = 1,
              step = 0.1,
              width = "100%"
            ),
            numericInput(
              ns("contact_informed"),
              label = tagList(
                "Proportion of known contacts that are contacted directly by
                the infected individual.",
                tooltip(
                  bs_icon("info-circle"),
                  "Informing contacts is an infected person actively
                  communicating their infection status with recent contacts to
                  alert them that they may be infected."
                )
              ),
              value = 0.5,
              min = 0,
              max = 1,
              step = 0.1,
              width = "100%"
            ),
            layout_columns(
              col_widths = c(6, 6),
              onset_to_isolation_input(ns = ns, id_prefix = "ict_"),
              plotOutput(ns("ict_onset_to_isolation_dist_plot"))
            ),
            checkboxInput(
              ns("ict_quarantine"),
              label = tagList(
                "Quarantine",
                tooltip(
                  bs_icon("info-circle"),
                  "When quarantine is enabled, traced contacts are isolated as soon
            as they are identified, regardless of whether they are symptomatic
            (presymptomatic isolation). When disabled, traced contacts are only
            isolated once they show symptoms, allowing for transmission during
            the presymptomatic phase."
                )
              ),
              value = PROPOSE_DEFAULTS$quarantine
            )
          )
        ),
        tags$hr(),
        accordion(
          open = TRUE,
          accordion_panel(
            title = "Compare Contact Tracing Strategies",
            icon = bs_icon("lightbulb"),
            tags$b("Onset-to-isolation delay by strategy"),
            plotOutput(ns("onset_to_isolation_dist_plot_combined")),
            tags$b("Comparison for outbreak control"),
            layout_columns(
              col_widths = c(4, 4, 4),
              value_box(
                title = "Probability of Outbreak Control with Digital Contact Tracing",
                value = uiOutput(ns("dct_extinct")),
                showcase = bs_icon("virus"),
                theme = "bg-gradient-blue-purple"
              ),
              value_box(
                title = "Probability of Outbreak Control with Manual Contact Tracing",
                value = uiOutput(ns("mct_extinct")),
                showcase = bs_icon("virus"),
                theme = "bg-gradient-green-yellow"
              ),
              value_box(
                title = "Probability of Outbreak Control with Informal Contact Tracing",
                value = uiOutput(ns("ict_extinct")),
                showcase = bs_icon("virus"),
                theme = "bg-gradient-pink-purple"
              )
            ),
            plotOutput(ns("cumulative_outbreak_size"))
          )
        )
      )
    )
  )
}

#' Shiny server for ***Contact Tracing Strategies*** page
#'
#' @inheritParams shiny::moduleServer
#'
#' @return Output from [shiny::moduleServer()].
#' @keywords internal
tracing_strategies_server <- function(id) {
  moduleServer(id, function(input, output, session) {

    ns <- session$ns

    observeEvent(input$pathogen_defaults, {
      defaults <- PROPOSE_DEFAULTS[[input$pathogen_defaults]]
      reset_pathogen_params(session = session, defaults = defaults)
    })

    # User-input checking with feedback ---------------------------------------
    offspring_feedback_server(input)
    symptom_event_prob_feedback_server(input)
    sim_feedback_server(input)

    community <- reactive({
      req(input$community_r0 >= 0)
      req(input$isolated_r0 >= 0)
      if (input$community_offspring_distribution == "nbinom") {
        \(n) rnbinom(n = n, mu = input$community_r0, size = input$community_disp)
      } else if (input$community_offspring_distribution == "pois") {
        \(n) rpois(n = n, lambda = input$community_r0)
      } else if (input$community_offspring_distribution == "geom") {
        \(n) rgeom(n = n, prob = 1 / (1 + input$community_r0))
      }
    })

    isolated <- reactive({
      if (input$isolated_offspring_distribution == "nbinom") {
        \(n) rnbinom(n = n, mu = input$isolated_r0, size = input$isolated_disp)
      } else if (input$isolated_offspring_distribution == "pois") {
        \(n) rpois(n = n, lambda = input$isolated_r0)
      } else if (input$isolated_offspring_distribution == "geom") {
        \(n) rgeom(n = n, prob = 1 / (1 + input$isolated_r0))
      }
    })

    incubation_period <- reactive({
      if (input$incubation_distribution == "lnorm") {
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

    # Setup onset-to-isolation delay for each tracing strategy ----------------
    # digital contact tracing
    dct_onset_to_isolation <- reactive({
      if (input$dct_onset_to_isolation_distribution == "lnorm") {
        \(n) rlnorm(
          n = n,
          meanlog = input$dct_onset_to_isolation_meanlog,
          sdlog = input$dct_onset_to_isolation_sdlog
        )
      } else if (input$dct_onset_to_isolation_distribution == "gamma") {
        \(n) rgamma(
          n = n,
          shape = input$dct_onset_to_isolation_shape,
          scale = input$dct_onset_to_isolation_scale
        )
      } else if (input$dct_onset_to_isolation_distribution == "weibull") {
        \(n) rweibull(
          n = n,
          shape = input$dct_onset_to_isolation_shape,
          scale = input$dct_onset_to_isolation_scale
        )
      }
    })
    # manual contact tracing
    mct_onset_to_isolation <- reactive({
      if (input$mct_onset_to_isolation_distribution == "lnorm") {
        \(n) rlnorm(
          n = n,
          meanlog = input$mct_onset_to_isolation_meanlog,
          sdlog = input$mct_onset_to_isolation_sdlog
        )
      } else if (input$mct_onset_to_isolation_distribution == "gamma") {
        \(n) rgamma(
          n = n,
          shape = input$mct_onset_to_isolation_shape,
          scale = input$mct_onset_to_isolation_scale
        )
      } else if (input$mct_onset_to_isolation_distribution == "weibull") {
        \(n) rweibull(
          n = n,
          shape = input$mct_onset_to_isolation_shape,
          scale = input$mct_onset_to_isolation_scale
        )
      }
    })
    # informal contact tracing
    ict_onset_to_isolation <- reactive({
      if (input$ict_onset_to_isolation_distribution == "lnorm") {
        \(n) rlnorm(
          n = n,
          meanlog = input$ict_onset_to_isolation_meanlog,
          sdlog = input$ict_onset_to_isolation_sdlog
        )
      } else if (input$ict_onset_to_isolation_distribution == "gamma") {
        \(n) rgamma(
          n = n,
          shape = input$ict_onset_to_isolation_shape,
          scale = input$ict_onset_to_isolation_scale
        )
      } else if (input$ict_onset_to_isolation_distribution == "weibull") {
        \(n) rweibull(
          n = n,
          shape = input$ict_onset_to_isolation_shape,
          scale = input$ict_onset_to_isolation_scale
        )
      }
    })

    offspring <- reactive({
      offspring_opts(community = community(), isolated = isolated())
    })

    # Set up delay distributions for each tracing strategy --------------------
    dct_delays <- reactive({
      delay_opts(
        incubation_period = incubation_period(),
        onset_to_isolation = dct_onset_to_isolation()
      )
    })
    mct_delays <- reactive({
      delay_opts(
        incubation_period = incubation_period(),
        onset_to_isolation = mct_onset_to_isolation()
      )
    })
    ict_delays <- reactive({
      delay_opts(
        incubation_period = incubation_period(),
        onset_to_isolation = ict_onset_to_isolation()
      )
    })

    simulate <- reactiveVal(0L)

    observeEvent(input$simulate, {
      # Require at least one contact tracing strategy to be selected
      if (!isTRUE(input$digital_contact_tracing) &&
          !isTRUE(input$manual_contact_tracing) &&
          !isTRUE(input$informal_contact_tracing)) {
        showModal(modalDialog(
          title = "Warning: No contact tracing strategies have been selected.",
          "Please tick the strategy(s) you'd like to evaluate.",
          footer = actionButton(ns("cancel"), "Cancel")
        ))
      } else if (input$replicates > 50) {
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
      req(input$asymptomatic >= 0 && input$asymptomatic <= 1)
      req(input$presymptomatic_transmission >= 0 && input$presymptomatic_transmission <= 1)
      req(input$cap_max_days >= 1)
      req(input$cap_cases >= 1)

      # default to random seed if not specified by user
      if (is.na(input$seed)) {
        set.seed(runif(n = 1, min = 1, max = 1e5))
      } else {
        set.seed(input$seed)
      }

      # only run tracing strategy if ticked by user
      if (isTRUE(input$digital_contact_tracing)) {
        # Digital tracing coverage: both the infector and contact need the app
        # (uptake squared), scaled by the app's sensitivity.
        req(!is.na(input$app_uptake), input$app_uptake >= 0, input$app_uptake <= 1)
        req(
          !is.na(input$app_sensitivity),
          input$app_sensitivity >= 0,
          input$app_sensitivity <= 1
        )
        dct_symptomatic_traced <- input$app_uptake^2 * input$app_sensitivity
        dct_scenario <- scenario_sim(
          n = input$replicates,
          initial_cases = input$initial_cases,
          offspring = offspring(),
          delays = dct_delays(),
          event_probs = event_prob_opts(
            asymptomatic = input$asymptomatic,
            presymptomatic_transmission = input$presymptomatic_transmission,
            symptomatic_traced = dct_symptomatic_traced
          ),
          interventions = intervention_opts(quarantine = input$dct_quarantine),
          sim = sim_opts(cap_max_days = input$cap_max_days, cap_cases = input$cap_cases)
        )
      } else {
        dct_scenario <- NULL
      }

      if (isTRUE(input$manual_contact_tracing)) {
        req(
          !is.na(input$contact_known),
          input$contact_known >= 0,
          input$contact_known <= 1
        )
        req(
          !is.na(input$contact_recall),
          input$contact_recall >= 0,
          input$contact_recall <= 1
        )
        req(
          !is.na(input$system_coverage),
          input$system_coverage >= 0,
          input$system_coverage <= 1
        )

        mct_symptomatic_traced <- input$contact_known * input$contact_recall *
          input$system_coverage
        mct_scenario <- scenario_sim(
          n = input$replicates,
          initial_cases = input$initial_cases,
          offspring = offspring(),
          delays = mct_delays(),
          event_probs = event_prob_opts(
            asymptomatic = input$asymptomatic,
            presymptomatic_transmission = input$presymptomatic_transmission,
            symptomatic_traced = mct_symptomatic_traced
          ),
          interventions = intervention_opts(quarantine = input$mct_quarantine),
          sim = sim_opts(cap_max_days = input$cap_max_days, cap_cases = input$cap_cases)
        )
      } else {
        mct_scenario <- NULL
      }
      if (isTRUE(input$informal_contact_tracing)) {
        req(
          !is.na(input$ict_contact_known),
          input$ict_contact_known >= 0,
          input$ict_contact_known <= 1
        )
        req(
          !is.na(input$contact_informed),
          input$contact_informed >= 0,
          input$contact_informed <= 1
        )
        ict_symptomatic_traced <- input$ict_contact_known * input$contact_informed
        ict_scenario <- scenario_sim(
          n = input$replicates,
          initial_cases = input$initial_cases,
          offspring = offspring(),
          delays = ict_delays(),
          event_probs = event_prob_opts(
            asymptomatic = input$asymptomatic,
            presymptomatic_transmission = input$presymptomatic_transmission,
            symptomatic_traced = ict_symptomatic_traced
          ),
          interventions = intervention_opts(quarantine = input$ict_quarantine),
          sim = sim_opts(cap_max_days = input$cap_max_days, cap_cases = input$cap_cases)
        )
      } else {
        ict_scenario <- NULL
      }
      list(
        dct_scenario = dct_scenario,
        mct_scenario = mct_scenario,
        ict_scenario = ict_scenario
      )
    })
    output$dct_extinct <- renderText(
      if (!is.null(scenario()$dct_scenario)) {
        signif(extinct_prob(scenario()$dct_scenario), digits = 2)
      } else {
        NA
      }
    )
    output$mct_extinct <- renderText(
      if (!is.null(scenario()$mct_scenario)) {
      signif(extinct_prob(scenario()$mct_scenario), digits = 2)
      } else {
        NA
      }
    )
    output$ict_extinct <- renderText(
      if (!is.null(scenario()$ict_scenario)) {
        signif(extinct_prob(scenario()$ict_scenario), digits = 2)
      } else {
        NA
      }
    )

    # Cumulative outbreak size by tracing strategy --------------------------
    output$cumulative_outbreak_size <- renderPlot({
      scenarios <- scenario()
      # All three strategies are always shown on the x-axis, in a fixed order,
      # regardless of which were actually simulated.
      strategy_levels <- c("Digital", "Manual", "Informal")
      scen_list <- list(
        scenarios$dct_scenario,
        scenarios$mct_scenario,
        scenarios$ict_scenario
      )
      # Final cumulative outbreak size for each replicate (`cumulative` is
      # monotonic within a `sim`), summarised to a median and 95% interval
      # across the replicates. Unsimulated strategies contribute NA so their
      # x-axis slot is retained but no point is drawn.
      summ <- lapply(scen_list, function(s) {
        if (is.null(s)) {
          return(c(med = NA_real_, lwr = NA_real_, upr = NA_real_))
        }
        sizes <- s[, max(cumulative), by = sim]$V1
        c(
          med = stats::median(sizes),
          lwr = stats::quantile(sizes, 0.025, names = FALSE),
          upr = stats::quantile(sizes, 0.975, names = FALSE)
        )
      })
      df <- data.frame(
        strategy = factor(strategy_levels, levels = strategy_levels),
        med = vapply(summ, `[`, numeric(1), "med"),
        lwr = vapply(summ, `[`, numeric(1), "lwr"),
        upr = vapply(summ, `[`, numeric(1), "upr")
      )
      tinyplot(
        med ~ strategy,
        data = df,
        ymin = df$lwr,
        ymax = df$upr,
        type = "pointrange",
        pch = 19,
        cex = 1.2,
        lwd = 3,
        col = "steelblue",
        xlab = "Contact tracing strategy",
        ylab = "Cumulative outbreak size (median & 95% CI)",
        theme = "clean"
      )
    })

    # parameter distribution plots ------------------------------------------
    output$offspring_dist_plot <- offspring_dist_plot(input)
    output$incubation_dist_plot <- incubation_dist_plot(input)
    output$presymptomatic_dist_plot <- presymptomatic_dist_plot(input)

    # per-strategy onset-to-isolation delay distribution plots
    output$dct_onset_to_isolation_dist_plot <-
      onset_to_isolation_dist_plot(input, id_prefix = "dct_", col = "steelblue")
    output$mct_onset_to_isolation_dist_plot <-
      onset_to_isolation_dist_plot(input, id_prefix = "mct_", col = "forestgreen")
    output$ict_onset_to_isolation_dist_plot <-
      onset_to_isolation_dist_plot(input, id_prefix = "ict_", col = "red4")

    # All three strategies' onset-to-isolation delays overlaid on one plot,
    # coloured to match the individual per-strategy plots above.
    output$onset_to_isolation_dist_plot_combined <-
      onset_to_isolation_dist_plot_combined(
        input,
        id_prefixes = c(Digital = "dct_", Manual = "mct_", Informal = "ict_"),
        palette = c("steelblue", "forestgreen", "red4"),
        toggles = c(
          "digital_contact_tracing",
          "manual_contact_tracing",
          "informal_contact_tracing"
        )
      )

    observeEvent(input$reset, {
      # set pathogen_defaults back to default
      updateSelectInput(session, "pathogen_defaults", selected = "disease_x")

      defaults <- PROPOSE_DEFAULTS[[input$pathogen_defaults]]
      reset_pathogen_params(session = session, defaults = defaults)

      # reset non-pathogen parameters, per contact tracing strategy
      for (prefix in c("dct_", "mct_", "ict_")) {
        updateSelectInput(
          session,
          paste0(prefix, "onset_to_isolation_distribution"),
          selected = PROPOSE_DEFAULTS$onset_to_isolation_distribution
        )
        updateNumericInput(
          session,
          paste0(prefix, "onset_to_isolation_meanlog"),
          value = PROPOSE_DEFAULTS$onset_to_isolation_meanlog
        )
        updateNumericInput(
          session,
          paste0(prefix, "onset_to_isolation_sdlog"),
          value = PROPOSE_DEFAULTS$onset_to_isolation_sdlog
        )
        updateCheckboxInput(
          session,
          paste0(prefix, "quarantine"),
          value = PROPOSE_DEFAULTS$quarantine
        )
      }
      # Digital tracing coverage is derived from the app parameters
      updateNumericInput(session, "app_uptake", value = 0.5)
      updateNumericInput(session, "app_sensitivity", value = 0.5)
      # Manual tracing coverage is derived from known contacts, recall and
      # system coverage
      updateNumericInput(session, "contact_known", value = 0.5)
      updateNumericInput(session, "contact_recall", value = 0.5)
      updateNumericInput(session, "system_coverage", value = 0.5)
      # Informal tracing coverage is derived from known and informed contacts
      updateNumericInput(session, "ict_contact_known", value = 0.5)
      updateNumericInput(session, "contact_informed", value = 0.5)
      updateNumericInput(session, "cap_max_days", value = PROPOSE_DEFAULTS$cap_max_days)
      updateNumericInput(session, "cap_cases", value = PROPOSE_DEFAULTS$cap_cases)
      updateNumericInput(session, "replicates", value = PROPOSE_DEFAULTS$replicates)
      updateNumericInput(session, "initial_cases", value = PROPOSE_DEFAULTS$initial_cases)
    })
  })
}