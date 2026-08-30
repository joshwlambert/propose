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
        replicates_input(ns = ns, page = "explore"),
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
                plotOutput(ns("offspring_dist_plot")),
                tags$p(
                  class = "small text-muted mt-2",
                  "The probability distribution of the number of secondary cases
                  produced by a single infectious case, shown for community and
                  isolated cases (and asymptomatic cases when enabled). This is
                  the offspring distribution the branching-process model samples
                  to generate onward transmission."
                )
              ),
              nav_panel(
                "Incubation period",
                plotOutput(ns("incubation_dist_plot")),
                tags$p(
                  class = "small text-muted mt-2",
                  "The probability density of the incubation period — the delay
                  from infection to symptom onset — under the selected
                  distribution. Each simulated case draws its symptom onset time
                  from this distribution."
                )
              ),
              nav_panel(
                "Onset-to-isolation",
                plotOutput(ns("onset_to_isolation_dist_plot")),
                tags$p(
                  class = "small text-muted mt-2",
                  "The probability density of the delay from symptom onset to
                  isolation under the selected distribution. Shorter delays
                  isolate cases sooner and prevent more onward transmission."
                )
              ),
              nav_panel(
                "Presymptomatic transmission",
                plotOutput(ns("presymptomatic_dist_plot")),
                tags$p(
                  class = "small text-muted mt-2",
                  "The probability density of when transmission occurs relative
                  to the infector's own symptom onset. Probability mass before
                  day zero represents transmission that happens before symptoms
                  appear (presymptomatic transmission)."
                )
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
            nav_panel(
              "Cumulative cases",
              plotOutput(ns("cumulative_cases")),
              tags$p(
                class = "small text-muted mt-2",
                "The cumulative number of cases (by date of symptom onset) over
                the course of each outbreak. The Trajectories view shows one line
                per simulated outbreak; the Mean & CI view shows the mean across
                replicates with a 95% interval. Dashed lines mark the maximum
                case and time caps at which the simulation stops."
              )
            ),
            nav_panel(
              "Weekly cases",
              plotOutput(ns("weekly_cases")),
              tags$p(
                class = "small text-muted mt-2",
                "The number of new cases each week, counted by date of symptom
                onset. The Trajectories view shows one line per simulated
                outbreak; the Mean & CI view shows the mean across replicates
                with a 95% interval."
              ),
              div(
                class = "alert alert-warning small mt-2",
                role = "alert",
                bs_icon("exclamation-triangle-fill"),
                "A decline in weekly cases toward the end of an outbreak may
                reflect genuine control, but can also be an artefact of the
                maximum case cap: once an outbreak reaches the cap the simulation
                stops generating new infections, yet cases infected shortly
                beforehand are still counted as their symptoms appear over
                subsequent weeks (delayed by the incubation period)."
              )
            )
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
      req(input$cap_max_days >= 1)
      req(input$cap_cases >= 1)

      # translating the sidebar inputs into the {ringbp} option objects is
      # shared with the Compare page, which needs exactly the same mapping
      params <- collect_params(input)

      # default to random seed if not specified by user
      if (is.na(input$seed)) {
        set.seed(runif(n = 1, min = 1, max = 1e5))
      } else {
        set.seed(input$seed)
      }

      run_scenario(
        params,
        shared = list(
          replicates = input$replicates,
          initial_cases = input$initial_cases,
          cap_max_days = input$cap_max_days,
          cap_cases = input$cap_cases
        )
      )
    })

    # capped outbreak to prevent overshooting the cumulative cases cap for
    # plotting, extinct and uncapped replicates are unchanged
    scenario_capped <- reactive(cap_scenario(scenario(), input$cap_cases))

    # probability of outbreak control (proportion of replicates controlled) with
    # a Clopper-Pearson exact 95% CI
    control <- reactive(control_stats(scenario()))
    output$extinct <- renderText(signif(control()$p, digits = 2))
    output$extinct_ci <- renderUI(control_ci_caption(control()))
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
      reset_intervention_params(session = session)
      updateNumericInput(session, "cap_max_days", value = PROPOSE_DEFAULTS$cap_max_days)
      updateNumericInput(session, "cap_cases", value = PROPOSE_DEFAULTS$cap_cases)
      updateSliderInput(session, "replicates", value = PROPOSE_DEFAULTS$replicates$explore)
      updateSliderInput(session, "initial_cases", value = PROPOSE_DEFAULTS$initial_cases)
    })
  })
}
