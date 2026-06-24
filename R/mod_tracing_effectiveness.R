#' Shiny UI for ***Contact Tracing Effectiveness*** page
#'
#' @inheritParams shiny::moduleServer
#'
#' @return Output from [shiny::tagList()].
#' @keywords internal
tracing_effectiveness_ui <- function(id) {
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

    page_title("Contact tracing effectiveness for outbreak control"),

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
        tags$b("Contact Tracing Sweep"),
        contact_tracing_seq_input(ns = ns, from = 0, to = 1, by = 0.2),
        numericInput(
          ns("replicates"),
          label = tagList(
            "Number of simulation replicates",
            tooltip(
              bsicons::bs_icon("info-circle"),
              "This controls the number of independent outbreaks to simulate."
            )
          ),
          value = 100,
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
        tags$b("Intervention Parameters"),
        onset_to_isolation_input(ns = ns),
        intervention_input(ns = ns),
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

        tags$h4("Outbreak control"),
        plotOutput(ns("outbreak_control_vs_tracing")),
        tags$p(
          class = "small text-muted mt-2",
          "Percentage of simulated outbreaks that were brought under control
          (i.e., became extinct before reaching the case or time caps) at each
          level of contact tracing."
        ),
        tags$hr(),
        tags$h4("Effective reproduction number"),
        plotOutput(ns("reff_vs_tracing")),
        tags$p(
          class = "small text-muted mt-2",
          "Median effective reproduction number across simulated outbreaks at
          each level of contact tracing. Shaded bands show the interquartile
          range (darker) and 95% interval (lighter) across replicates. The
          dashed horizontal line at R = 1 marks the threshold below which an
          outbreak is expected to decline."
        ),
        tags$hr(),
        tags$h4("Maximum weekly cases in controlled outbreaks"),
        plotOutput(ns("max_weekly_vs_tracing")),
        tags$p(
          class = "small text-muted mt-2",
          "Distribution of the maximum number of cases observed in any single
          week, across only those simulated outbreaks that were brought under
          control. Box shading is proportional to the percentage of outbreaks
          controlled, also labelled above each box."
        )
      )
    )
  )
}

#' Shiny server for ***Contact Tracing Effectiveness*** page
#'
#' @inheritParams shiny::moduleServer
#'
#' @return Output from [shiny::moduleServer()].
#' @keywords internal
tracing_effectiveness_server <- function(id) {
  moduleServer(id, function(input, output, session) {

    # User-input checking with feedback ---------------------------------------
    offspring_feedback_server(input)
    symptom_event_prob_feedback_server(input)
    contact_tracing_seq_feedback_server(input)
    sim_feedback_server(input)

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
        \(n) rlnorm(n = n, meanlog = input$incubation_meanlog, sdlog = input$incubation_sdlog)
      } else if (input$incubation_distribution == "gamma") {
        \(n) rgamma(n = n, shape = input$incubation_shape, scale = input$incubation_scale)
      } else if (input$incubation_distribution == "weibull") {
        \(n) rweibull(n = n, shape = input$incubation_shape, scale = input$incubation_scale)
      }
    })

    onset_to_isolation <- reactive({
      if (input$onset_to_isolation_distribution == "lnorm") {
        \(n) rlnorm(n = n, meanlog = input$onset_to_isolation_meanlog, sdlog = input$onset_to_isolation_sdlog)
      } else if (input$onset_to_isolation_distribution == "gamma") {
        \(n) rgamma(n = n, shape = input$onset_to_isolation_shape, scale = input$onset_to_isolation_scale)
      } else if (input$onset_to_isolation_distribution == "weibull") {
        \(n) rweibull(n = n, shape = input$onset_to_isolation_shape, scale = input$onset_to_isolation_scale)
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

    loading <- tagList(
      spin_hexdots(),
      h3("Running contact tracing sweep.", style = "color: #000080; margin-top: 40px;")
    )

    sweep_results <- eventReactive(input$simulate, {
      req(!is.na(input$symptomatic_traced_from), !is.na(input$symptomatic_traced_to), !is.na(input$symptomatic_traced_by))
      req(input$symptomatic_traced_from >= 0, input$symptomatic_traced_to <= 1)
      req(input$symptomatic_traced_from <= input$symptomatic_traced_to)
      req(input$symptomatic_traced_by > 0)
      req(input$replicates >= 1, input$initial_cases >= 1)
      req(input$cap_max_days >= 1, input$cap_cases >= 1)
      req(input$asymptomatic >= 0, input$asymptomatic <= 1)
      req(input$presymptomatic_transmission >= 0, input$presymptomatic_transmission <= 1)

      tracing_seq <- seq(
        from = input$symptomatic_traced_from,
        to = input$symptomatic_traced_to,
        by = input$symptomatic_traced_by
      )

      waiter_show(html = loading, color = transparent(0.75))
      on.exit(waiter_hide())

      per_scenario <- lapply(tracing_seq, function(p) {
        sim <- scenario_sim(
          n = input$replicates,
          initial_cases = input$initial_cases,
          offspring = offspring(),
          delays = delays(),
          event_probs = event_prob_opts(
            asymptomatic = input$asymptomatic,
            presymptomatic_transmission = input$presymptomatic_transmission,
            symptomatic_traced = p
          ),
          interventions = intervention_opts(quarantine = input$quarantine),
          sim = sim_opts(cap_max_days = input$cap_max_days, cap_cases = input$cap_cases)
        )
        # `effective_r0` is constant across weeks within each replicate;
        # take it once per `sim` then compute quantile summaries.
        eff <- sim[, effective_r0[1], by = sim]$V1
        # max weekly cases across extinct (= controlled) replicates only
        extinct_sims <- detect_extinct(sim)[extinct == 1L, sim]
        max_weekly <- if (length(extinct_sims) > 0L) {
          sim[sim %in% extinct_sims, max(weekly_cases), by = sim]$V1
        } else {
          numeric(0)
        }
        list(
          pcontrol = extinct_prob(sim),
          median_reff = median(eff, na.rm = TRUE),
          lwr_reff = stats::quantile(eff, 0.025, na.rm = TRUE),
          iqr_lwr_reff = stats::quantile(eff, 0.25, na.rm = TRUE),
          iqr_upr_reff = stats::quantile(eff, 0.75, na.rm = TRUE),
          upr_reff = stats::quantile(eff, 0.975, na.rm = TRUE),
          max_weekly_cases = max_weekly
        )
      })

      df <- data.frame(
        tracing_pct = tracing_seq * 100,
        control_pct = vapply(per_scenario, `[[`, numeric(1), "pcontrol") * 100,
        median_reff = vapply(per_scenario, `[[`, numeric(1), "median_reff"),
        lwr_reff = vapply(per_scenario, `[[`, numeric(1), "lwr_reff"),
        iqr_lwr_reff = vapply(per_scenario, `[[`, numeric(1), "iqr_lwr_reff"),
        iqr_upr_reff = vapply(per_scenario, `[[`, numeric(1), "iqr_upr_reff"),
        upr_reff = vapply(per_scenario, `[[`, numeric(1), "upr_reff")
      )
      df$max_weekly_cases <- I(lapply(per_scenario, `[[`, "max_weekly_cases"))
      df
    })

    output$outbreak_control_vs_tracing <- renderPlot({
      df <- sweep_results()
      tinyplot(
        control_pct ~ tracing_pct,
        data = df,
        type = "b",
        pch = 19,
        lwd = 3,
        cex = 1.2,
        col = "steelblue",
        xlab = "Contacts traced (%)",
        ylab = "Simulated outbreaks controlled (%)",
        xlim = c(0, 100),
        ylim = c(0, 100),
        theme = "clean"
      )
    })

    output$reff_vs_tracing <- renderPlot({
      df <- sweep_results()
      y_max <- max(df$upr_reff, 1.5, na.rm = TRUE)
      tinyplot(
        median_reff ~ tracing_pct,
        data = df,
        type = "ribbon",
        ymin = df$lwr_reff,
        ymax = df$upr_reff,
        fill = adjustcolor("steelblue", alpha.f = 0.15),
        col = NA,
        xlab = "Contacts traced (%)",
        ylab = "Median effective reproduction number",
        xlim = c(0, 100),
        ylim = c(0, y_max),
        theme = "clean"
      )
      tinyplot_add(
        median_reff ~ tracing_pct,
        data = df,
        type = "ribbon",
        ymin = df$iqr_lwr_reff,
        ymax = df$iqr_upr_reff,
        fill = adjustcolor("steelblue", alpha.f = 0.35),
        col = NA
      )
      tinyplot_add(
        median_reff ~ tracing_pct,
        data = df,
        type = "b",
        pch = 21,
        bg = "steelblue",
        col = "steelblue",
        lwd = 3,
        cex = 1.4
      )
      abline(h = 1, lty = 2, lwd = 1, col = "grey30")
    })

    output$max_weekly_vs_tracing <- renderPlot({
      df <- sweep_results()
      mw_list <- df$max_weekly_cases
      # colour each box on a white-blue gradient by extinction probability
      grad <- grDevices::colorRampPalette(c("white", "steelblue"))(101)
      box_cols <- grad[1 + round(df$control_pct)]
      op <- par(mar = c(5, 5, 3, 2))
      on.exit(par(op))
      boxplot(
        mw_list,
        names = paste0(df$tracing_pct, "%"),
        col = box_cols,
        border = "grey30",
        xlab = "Contacts traced (%)",
        ylab = "Maximum weekly cases in controlled outbreaks",
        las = 1,
        outline = FALSE
      )
      # Per-box "% controlled" labels, positioned just above each upper whisker
      stats_mat <- vapply(mw_list, function(v) {
        if (length(v) == 0) c(NA_real_, NA_real_) else boxplot.stats(v)$stats[c(1, 5)]
      }, numeric(2))
      upper_whisker <- stats_mat[2, ]
      y_pad <- diff(range(unlist(mw_list), 0, finite = TRUE, na.rm = TRUE)) * 0.05
      text(
        x = seq_along(mw_list),
        y = upper_whisker + y_pad,
        labels = paste0(round(df$control_pct), "%"),
        cex = 0.9,
        col = "black"
      )
    })

    # Parameter distribution plots ------------------------------------------
    output$offspring_dist_plot <- offspring_dist_plot(input)
    output$incubation_dist_plot <- incubation_dist_plot(input)
    output$onset_to_isolation_dist_plot <- onset_to_isolation_dist_plot(input)
    output$presymptomatic_dist_plot <- presymptomatic_dist_plot(input)
  })
}
