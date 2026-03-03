#' Shiny UI for ***Compare*** page
#'
#' @inheritParams shiny::moduleServer
#'
#' @return Output from [shiny::tagList()].
#' @keywords internal
compare_ui <- function(id) {
  ns <- NS(id)

  tagList(
    page_title("Compare Outbreak Scenarios"),


    tags$div(
      class = "d-flex justify-content-center",
      style = "margin-bottom: 20px;",
      actionButton(
        ns("compare"),
        "Simulate & Compare outbreaks",
        class = "btn-lg",
        style = "background-color: #000080; color: white; border: none; padding: 10px 30px;"
      )
    ),

    card(
      card_header("Outbreak Scenario simulation controls"),
      layout_columns(
        card(
          sliderInput(ns("replicates"), "Number of simulation replicates:", min = 1, max = 100, value = 10),
        ),
        card(
          sliderInput(ns("initial_cases"), "Number of initial cases:", min = 1, max = 50, value = 5)
        )
      ),
      sim_input(ns = ns)
    ),

    layout_columns(
      card(
        card_header("Outbreak Scenario 1"),
        offspring_input(ns = function(id) ns(paste("s1", id, sep = "_"))),
        delays_input(ns = function(id) ns(paste("s1", id, sep = "_"))),
        event_prob_input(ns = function(id) ns(paste("s1", id, sep = "_"))),
        intervention_input(ns = function(id) ns(paste("s1", id, sep = "_")))
      ),
      card(
        card_header("Outbreak Scenario 2"),
        offspring_input(ns = function(id) ns(paste("s2", id, sep = "_"))),
        delays_input(ns = function(id) ns(paste("s2", id, sep = "_"))),
        event_prob_input(ns = function(id) ns(paste("s2", id, sep = "_"))),
        intervention_input(ns = function(id) ns(paste("s2", id, sep = "_")))
      )
    ),
    card(
      card_header("Comparison Results"),
      layout_columns(
        value_box(
          title = "Difference in mean probability of outbreak control (scenario 1 - scenario 2)",
          value = uiOutput(ns("extinct_diff")),
          showcase = bs_icon("virus"),
          theme = "bg-gradient-blue-purple"
        ),
        value_box(
          title = "Difference in mean cumulative cases (scenario 1 - scenario 2)",
          value = uiOutput(ns("cumulative_diff")),
          showcase = bs_icon("graph-up"),
          theme = "bg-gradient-blue-green"
        )
      )
    )
  )
}

#' Shiny server for ***Compare*** page
#'
#' @inheritParams shiny::moduleServer
#'
#' @return Output from [shiny::moduleServer()].
#' @keywords internal
compare_server <- function(id) {
  moduleServer(id, function(input, output, session) {

    # Scenario 1 ------------------------------------------------------------
    community1 <- reactive({
      req(input$s1_community_r0 >= 0)
      req(input$s1_isolated_r0 >= 0)
      if (input$s1_community_offspring_distribution == "nbinom") {
        \(n) rnbinom(n = n, mu = input$s1_community_r0, size = input$s1_community_disp)
      } else if (input$s1_community_offspring_distribution == "pois") {
        \(n) rpois(n = n, lambda = input$s1_community_r0)
      } else if (input$s1_community_offspring_distribution == "geom") {
        \(n) rgeom(n = n, prob = 1 / input$s1_community_r0)
      }
    })

    isolated1 <- reactive({
      if (input$s1_isolated_offspring_distribution == "nbinom") {
        \(n) rnbinom(n = n, mu = input$s1_isolated_r0, size = input$s1_isolated_disp)
      } else if (input$s1_isolated_offspring_distribution == "pois") {
        \(n) rpois(n = n, lambda = input$s1_isolated_r0)
      } else if (input$s1_isolated_offspring_distribution == "geom") {
        \(n) rgeom(n = n, prob = 1 / input$s1_isolated_r0)
      }
    })

    incubation_period1 <- reactive({
      if (input$s1_incubation_distribution == "lnorm") {
        \(n) rlnorm(
          n = n,
          meanlog = input$s1_incubation_meanlog,
          sdlog = input$s1_incubation_sdlog
        )
      } else if (input$s1_incubation_distribution == "gamma") {
        \(n) rgamma(
          n = n,
          shape = input$s1_incubation_shape,
          scale = input$s1_incubation_scale
        )
      } else if (input$s1_incubation_distribution == "weibull") {
        \(n) rweibull(
          n = n,
          shape = input$s1_incubation_shape,
          scale = input$s1_incubation_scale
        )
      }
    })

    onset_to_isolation1 <- reactive({
      if (input$s1_onset_to_isolation_distribution == "lnorm") {
        \(n) rlnorm(
          n = n,
          meanlog = input$s1_onset_to_isolation_meanlog,
          sdlog = input$s1_onset_to_isolation_sdlog
        )
      } else if (input$s1_onset_to_isolation_distribution == "gamma") {
        \(n) rgamma(
          n = n,
          shape = input$s1_onset_to_isolation_shape,
          scale = input$s1_onset_to_isolation_scale
        )
      } else if (input$s1_onset_to_isolation_distribution == "weibull") {
        \(n) rweibull(
          n = n,
          shape = input$s1_onset_to_isolation_shape,
          scale = input$s1_onset_to_isolation_scale
        )
      }
    })


    offspring1 <- reactive({
      offspring_opts(community = community1(), isolated = isolated1())
    })

    delays1 <- reactive({
      delay_opts(
        incubation_period = incubation_period1(),
        onset_to_isolation = onset_to_isolation1()
      )
    })

    # Scenario 2 ------------------------------------------------------------
    community2 <- reactive({
      req(input$s2_community_r0 >= 0)
      req(input$s2_isolated_r0 >= 0)
      if (input$s2_community_offspring_distribution == "nbinom") {
        \(n) rnbinom(n = n, mu = input$s2_community_r0, size = input$s2_community_disp)
      } else if (input$s2_community_offspring_distribution == "pois") {
        \(n) rpois(n = n, lambda = input$s2_community_r0)
      } else if (input$s2_community_offspring_distribution == "geom") {
        \(n) rgeom(n = n, prob = 1 / input$s2_community_r0)
      }
    })

    isolated2 <- reactive({
      if (input$s2_isolated_offspring_distribution == "nbinom") {
        \(n) rnbinom(n = n, mu = input$s2_isolated_r0, size = input$s2_isolated_disp)
      } else if (input$s2_isolated_offspring_distribution == "pois") {
        \(n) rpois(n = n, lambda = input$s2_isolated_r0)
      } else if (input$s2_isolated_offspring_distribution == "geom") {
        \(n) rgeom(n = n, prob = 1 / input$s2_isolated_r0)
      }
    })

    incubation_period2 <- reactive({
      if (input$s2_incubation_distribution == "lnorm") {
        \(n) rlnorm(
          n = n,
          meanlog = input$s2_incubation_meanlog,
          sdlog = input$s2_incubation_sdlog
        )
      } else if (input$s2_incubation_distribution == "gamma") {
        \(n) rgamma(
          n = n,
          shape = input$s2_incubation_shape,
          scale = input$s2_incubation_scale
        )
      } else if (input$s2_incubation_distribution == "weibull") {
        \(n) rweibull(
          n = n,
          shape = input$s2_incubation_shape,
          scale = input$s2_incubation_scale
        )
      }
    })

    onset_to_isolation2 <- reactive({
      if (input$s2_onset_to_isolation_distribution == "lnorm") {
        \(n) rlnorm(
          n = n,
          meanlog = input$s2_onset_to_isolation_meanlog,
          sdlog = input$s2_onset_to_isolation_sdlog
        )
      } else if (input$s2_onset_to_isolation_distribution == "gamma") {
        \(n) rgamma(
          n = n,
          shape = input$s2_onset_to_isolation_shape,
          scale = input$s2_onset_to_isolation_scale
        )
      } else if (input$s2_onset_to_isolation_distribution == "weibull") {
        \(n) rweibull(
          n = n,
          shape = input$s2_onset_to_isolation_shape,
          scale = input$s2_onset_to_isolation_scale
        )
      }
    })


    offspring2 <- reactive({
      offspring_opts(community = community2(), isolated = isolated2())
    })

    delays2 <- reactive({
      delay_opts(
        incubation_period = incubation_period2(),
        onset_to_isolation = onset_to_isolation2()
      )
    })

    scenario_diff <- eventReactive(input$compare, {
      req(input$s1_asymptomatic >= 0 && input$s1_asymptomatic <= 1)
      req(input$s1_presymptomatic_transmission >= 0 && input$s1_presymptomatic_transmission <= 1)
      req(input$s1_symptomatic_ascertained >= 0 && input$s1_symptomatic_ascertained <= 1)

      req(input$s2_asymptomatic >= 0 && input$s2_asymptomatic <= 1)
      req(input$s2_presymptomatic_transmission >= 0 && input$s2_presymptomatic_transmission <= 1)
      req(input$s2_symptomatic_ascertained >= 0 && input$s2_symptomatic_ascertained <= 1)

      req(input$cap_max_days >= 1)
      req(input$cap_cases >= 1)

      scenario1 <- scenario_sim(
        n = input$replicates,
        initial_cases = input$initial_cases,
        offspring = offspring1(),
        delays = delays1(),
        event_probs = event_prob_opts(
          asymptomatic = input$s1_asymptomatic,
          presymptomatic_transmission = input$s1_presymptomatic_transmission,
          symptomatic_ascertained = input$s1_symptomatic_ascertained
        ),
        interventions = intervention_opts(quarantine = input$s1_quarantine),
        sim = sim_opts(cap_max_days = input$cap_max_days, cap_cases = input$cap_cases)
      )

      scenario2 <- scenario_sim(
        n = input$replicates,
        initial_cases = input$initial_cases,
        offspring = offspring2(),
        delays = delays2(),
        event_probs = event_prob_opts(
          asymptomatic = input$s2_asymptomatic,
          presymptomatic_transmission = input$s2_presymptomatic_transmission,
          symptomatic_ascertained = input$s2_symptomatic_ascertained
        ),
        interventions = intervention_opts(quarantine = input$s2_quarantine),
        sim = sim_opts(cap_max_days = input$cap_max_days, cap_cases = input$cap_cases)
      )

      diff_list <- list()

      diff_list$cumulative_diff <- mean(scenario1[week == max(week), cumulative] -
                                          scenario2[week == max(week), cumulative])

      diff_list$extinct_diff <- extinct_prob(scenario1) - extinct_prob(scenario2)
      return(diff_list)
    })
    output$cumulative_diff <- renderText(round(scenario_diff()$cumulative_diff, 2))
    output$extinct_diff <- renderText(scenario_diff()$extinct_diff)
  })
}
