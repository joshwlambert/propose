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

    page_title("{propose}: a Shiny app for {ringbp}"),

    sidebarLayout(
      sidebarPanel(
        replicates_input(ns = ns),
        initial_cases_input(ns = ns),
        div(
          class = "d-flex gap-2 mb-3",
          actionButton(ns("simulate"), "Simulate outbreak", class = "btn-primary"),
          actionButton(ns("reset"), "Reset Defaults", class = "btn-outline-secondary", icon = icon("rotate-left"))
        ),
        tags$b("Pathogen Parameters"),
        card(
          class = "allow-overflow",
          card_header(
            "Select Pathogen Parameters:",
            tooltip(
              bsicons::bs_icon("info-circle"),
              "Pathogen parameters based on estimates published in the literature.
              The default scenario is 'Disease X' and has generic pathogen parameters.",
              id = "tooltip"
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
        tags$b("Intervention Parameters"),
        onset_to_isolation_input(ns = ns),
        contact_tracing_input(ns = ns),
        intervention_input(ns = ns),
        tags$b("Simulation Control Parameters"),
        sim_input(ns = ns)
      ),
      mainPanel(
        value_box(
          title = "Probability of outbreak control",
          value = uiOutput(ns("extinct")),
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
    observeEvent(input$community_r0, {
      # prevent crashing when the numericInput is empty (value is NA)
      req(!is.na(input$community_r0))
      if (input$community_r0 < 0) {
        showFeedbackDanger(
          "community_r0",
          text = "Error: Community R0 cannot be negative."
        )
      } else {
        hideFeedback("community_r0")
      }
    })

    observeEvent(input$isolated_r0, {
      # prevent crashing when the numericInput is empty (value is NA)
      req(!is.na(input$isolated_r0))
      if (input$isolated_r0 < 0) {
        showFeedbackDanger(
          "isolated_r0",
          text = "Error: Isolated R0 cannot be negative."
        )
      } else {
        hideFeedback("isolated_r0")
      }
    })

    observeEvent(input$asymptomatic, {
      # prevent crashing when the numericInput is empty (value is NA)
      req(!is.na(input$asymptomatic))
      if (input$asymptomatic < 0 || input$asymptomatic > 1) {
        showFeedbackDanger(
          "asymptomatic",
          text = "Error: Probability of asymptomatic cases must be between 0 and 1."
        )
      } else {
        hideFeedback("asymptomatic")
      }
    })

    observeEvent(input$presymptomatic_transmission, {
      # prevent crashing when the numericInput is empty (value is NA)
      req(!is.na(input$presymptomatic_transmission))
      if (input$presymptomatic_transmission < 0 || input$presymptomatic_transmission > 1) {
        showFeedbackDanger(
          "presymptomatic_transmission",
          text = "Error: Probability of presymptomatic transmission cases must be between 0 and 1."
        )
      } else {
        hideFeedback("presymptomatic_transmission")
      }
    })

    observeEvent(input$symptomatic_traced, {
      # prevent crashing when the numericInput is empty (value is NA)
      req(!is.na(input$symptomatic_traced))
      if (input$symptomatic_traced < 0 || input$symptomatic_traced > 1) {
        showFeedbackDanger(
          "symptomatic_traced",
          text = "Error: Probability of a symptomatic contact being traced must be between 0 and 1."
        )
      } else {
        hideFeedback("symptomatic_traced")
      }
    })

    observeEvent(input$cap_max_days, {
      # prevent crashing when the numericInput is empty (value is NA)
      req(!is.na(input$cap_max_days))
      if (input$cap_max_days < 1) {
        showFeedbackDanger(
          "cap_max_days",
          text = "Error: The maximum number of days in the simulation must be at least 1."
        )
      } else {
        hideFeedback("cap_max_days")
      }
    })

    observeEvent(input$cap_cases, {
      # prevent crashing when the numericInput is empty (value is NA)
      req(!is.na(input$cap_cases))
      if (input$cap_cases < 1) {
        showFeedbackDanger(
          "cap_cases",
          text = "Error: The maximum number of cases in the simulation must be at least 1."
        )
      } else {
        hideFeedback("cap_cases")
      }
    })

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

    onset_to_isolation <- reactive({
      if (input$onset_to_isolation_distribution == "lnorm") {
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


    offspring <- reactive({
      offspring_opts(community = community(), isolated = isolated())
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
      req(input$asymptomatic >= 0 && input$asymptomatic <= 1)
      req(input$presymptomatic_transmission >= 0 && input$presymptomatic_transmission <= 1)
      req(input$symptomatic_traced >= 0 && input$symptomatic_traced <= 1)
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
          asymptomatic = input$asymptomatic,
          presymptomatic_transmission = input$presymptomatic_transmission,
          symptomatic_traced = input$symptomatic_traced
        ),
        interventions = intervention_opts(quarantine = input$quarantine),
        sim = sim_opts(cap_max_days = input$cap_max_days, cap_cases = input$cap_cases)
      )
    })
    output$extinct <- renderText(signif(extinct_prob(scenario()), digits = 2))
    output$cumulative_cases <- renderPlot({
      if (input$plot_style == "indiv") {
        tinyplot(
          cumulative ~ week | as.factor(sim),
          data = scenario(),
          type = "l",
          lwd = 3,
          ylab = "Cumulative number of cases",
          xlab = "Week",
          legend = FALSE,
          theme = "clean"
        )
      } else {
        # aggregate data: calculate mean, lower CI (2.5%), and upper CI (97.5%)
        summ <- aggregate(cumulative ~ week, data = scenario(), FUN = function(x) {
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
    }

    )
    output$weekly_cases <- renderPlot({
      if (input$plot_style == "indiv") {
        tinyplot(
          weekly_cases ~ week | as.factor(sim),
          data = scenario(),
          type = "l",
          lwd = 3,
          ylab = "Number of cases per week",
          xlab = "Week",
          legend = FALSE,
          theme = "clean"
        )
      } else {
        # mean & CI
        summ_w <- aggregate(weekly_cases ~ week, data = scenario(), FUN = function(x) {
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
    }
    )

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
      updateNumericInput(
        session,
        "symptomatic_traced",
        value = PROPOSE_DEFAULTS$symptomatic_traced
        )
      updateCheckboxInput(session, "quarantine", value = PROPOSE_DEFAULTS$quarantine)
      updateNumericInput(session, "cap_max_days", value = PROPOSE_DEFAULTS$cap_max_days)
      updateNumericInput(session, "cap_cases", value = PROPOSE_DEFAULTS$cap_cases)
      updateSliderInput(session, "replicates", value = PROPOSE_DEFAULTS$replicates)
      updateSliderInput(session, "initial_cases", value = PROPOSE_DEFAULTS$initial_cases)
    })
  })
}
