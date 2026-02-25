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

    # CSS to add margin around accordions
    tags$head(
      tags$style(HTML("
      .accordion-item {
        margin-top: 15px !important;
        margin-bottom: 15px !important;
        border-top-width: 5px !important;
      }
    "))
    ),

    page_title("{propose}: a Shiny app for {ringbp}"),

    sidebarLayout(
      sidebarPanel(
        sliderInput(ns("replicates"), "Number of simulation replicates:", min = 1, max = 100, value = 10),
        sliderInput(ns("initial_cases"), "Number of initial cases:", min = 1, max = 50, value = 5),
        actionButton(ns("simulate"), "Simulate outbreak"),
        offspring_input(ns = ns),
        delays_input(ns = ns),
        accordion(
          accordion_panel(
            title = "Event probabilities:",
            icon = bs_icon("person-fill-gear"),
            numericInput(ns("asymptomatic"), "Probability asymptomatic:", value = 0.1),
            numericInput(ns("presymptomatic_transmission"), "Probability of presymptomatic transmission:", value = 0.1),
            numericInput(ns("symptomatic_ascertained"), "Probability of contact traced:", value = 0.8)
          ),
          open = FALSE
        ),
        accordion(
          accordion_panel(
            title = "Interventions:",
            icon = bs_icon("shield-shaded"),
            checkboxInput(ns("quarantine"), "Quarantine", value = FALSE)
          ),
          open = FALSE
        ),
        accordion(
          accordion_panel(
            title = "Simulation controls: ",
            icon = bs_icon("gear-wide-connected"),
            numericInput(ns("cap_max_days"), "Maximum number of days:", value = 100),
            numericInput(ns("cap_cases"), "Maximum number of cases:", value = 5000),
            numericInput(ns("seed"), "Seed for simulation model", value = NA_integer_)
          ),
          open = FALSE
        )
      ),
      mainPanel(
        value_box(
          title = "Probability of outbreak control",
          value = uiOutput(ns("extinct")),
          showcase = bs_icon("virus"),
          theme = "bg-gradient-blue-purple"
        ),
        navset_card_underline(
          nav_panel("Cumulative cases", plotOutput(ns("cumulative_cases"))),
          nav_panel("Weekly cases", plotOutput(ns("weekly_cases")))
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

    observeEvent(input$symptomatic_ascertained, {
      # prevent crashing when the numericInput is empty (value is NA)
      req(!is.na(input$symptomatic_ascertained))
      if (input$symptomatic_ascertained < 0 || input$symptomatic_ascertained > 1) {
        showFeedbackDanger(
          "symptomatic_ascertained",
          text = "Error: Probability of a symptomatic contact being ascertained (traced) must be between 0 and 1."
        )
      } else {
        hideFeedback("symptomatic_ascertained")
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
        \(n) rgeom(n = n, prob = 1 / input$community_r0)
      }
    })

    isolated <- reactive({
      if (input$isolated_offspring_distribution == "nbinom") {
        \(n) rnbinom(n = n, mu = input$isolated_r0, size = input$isolated_disp)
      } else if (input$isolated_offspring_distribution == "pois") {
        \(n) rpois(n = n, lambda = input$isolated_r0)
      } else if (input$isolated_offspring_distribution == "geom") {
        \(n) rgeom(n = n, prob = 1 / input$isolated_r0)
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

    scenario <- eventReactive(input$simulate, {
      req(input$asymptomatic >= 0 && input$asymptomatic <= 1)
      req(input$presymptomatic_transmission >= 0 && input$presymptomatic_transmission <= 1)
      req(input$symptomatic_ascertained >= 0 && input$symptomatic_ascertained <= 1)
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
          symptomatic_ascertained = input$symptomatic_ascertained
        ),
        interventions = intervention_opts(quarantine = input$quarantine),
        sim = sim_opts(cap_max_days = input$cap_max_days, cap_cases = input$cap_cases)
      )
    })
    output$extinct <- renderText(extinct_prob(scenario()))
    output$cumulative_cases <- renderPlot(
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
    )
    output$weekly_cases <- renderPlot(
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
    )
  })
}
