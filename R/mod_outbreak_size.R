#' Shiny UI for ***Outbreak Size & Length*** page
#'
#' @inheritParams shiny::moduleServer
#'
#' @return Output from [shiny::tagList()].
#' @keywords internal
outbreak_size_ui <- function(id) {
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

    page_title("Outbreak Size & Length"),

    sidebarLayout(
      sidebarPanel(
        div(
          class = "d-flex gap-2 mb-3",
          actionButton(
            ns("simulate"),
            "Simulate outbreaks",
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
              "This controls the number of independent outbreaks to simulate.
              More replicates produce smoother outbreak size and length
              distributions but take longer to run."
            )
          ),
          value = 100,
          min = 1
        ),
        initial_cases_input(ns = ns, value = 1),
        tags$b("Pathogen Parameters"),
        patho_param_input(ns = ns),
        r0_seq_input(ns = ns, from = 0.1, to = 1.1, by = 0.1),
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

        card(
          card_header("Outbreak size by R0"),
          plotOutput(ns("size_dist")),
          tags$p(
            class = "small text-muted mt-2",
            "Outbreak size is the total number of cases across all generations of
            transmission. For each community R0 the simulated outbreaks are
            binned into size categories and shown as the proportion of outbreaks
            in each category (stacked to 1). Outbreaks that reach the case or
            time caps are truncated at the cap. The effective reproduction number
            realised in each outbreak may be lower than the basic community R0
            shown on the x-axis when interventions (e.g. contact tracing,
            isolation or quarantine) are active."
          )
        ),

        card(
          card_header("Outbreak length by R0"),
          navset_card_underline(
            nav_panel(
              "Generations",
              plotOutput(ns("length_generations"))
            ),
            nav_panel(
              "Time (weeks)",
              plotOutput(ns("length_time"))
            )
          ),
          tags$p(
            class = "small text-muted mt-2",
            "For each community R0 the simulated outbreaks are binned by length
            and shown as the proportion of outbreaks in each category (stacked to
            1). Use the tabs to switch between the number of branching-process
            generations until the outbreak ends and the outbreak duration in
            weeks. The effective reproduction number realised in each outbreak
            may be lower than the basic community R0 shown on the x-axis when
            interventions (e.g. contact tracing, isolation or quarantine) are
            active."
          )
        )
      )
    )
  )
}

#' Shiny server for ***Outbreak Size & Length*** page
#'
#' @inheritParams shiny::moduleServer
#'
#' @return Output from [shiny::moduleServer()].
#' @keywords internal
outbreak_size_server <- function(id) {
  moduleServer(id, function(input, output, session) {

    ns <- session$ns

    observeEvent(input$pathogen_defaults, {
      defaults <- PROPOSE_DEFAULTS[[input$pathogen_defaults]]
      reset_pathogen_params(session = session, defaults = defaults)
    })

    # User-input checking with feedback ---------------------------------------
    offspring_feedback_server(input)
    r0_seq_feedback_server(input)
    symptom_event_prob_feedback_server(input)
    contact_tracing_feedback_server(input)
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

    # sequence of community R0 values to sweep over.
    r0_seq <- reactive({
      req(
        !is.na(input$community_r0_from),
        !is.na(input$community_r0_to),
        !is.na(input$community_r0_by)
      )
      req(
        input$community_r0_from >= 0,
        input$community_r0_by > 0,
        input$community_r0_from <= input$community_r0_to
      )
      seq(
        from = input$community_r0_from,
        to = input$community_r0_to,
        by = input$community_r0_by
      )
    })

    # build ringbp::offspring_opts() for a single community R0, holding the
    # offspring distribution, dispersions and isolated R0 fixed across the sweep
    offspring <- function(r0, distribution, community_k, isolated_r0, isolated_k) {
      community <- switch(
        distribution,
        nbinom = \(n) rnbinom(n = n, mu = r0, size = community_k),
        pois = \(n) rpois(n = n, lambda = r0),
        geom = \(n) rgeom(n = n, prob = 1 / (1 + r0))
      )
      isolated <- switch(
        distribution,
        nbinom = \(n) rnbinom(n = n, mu = isolated_r0, size = isolated_k),
        pois = \(n) rpois(n = n, lambda = isolated_r0),
        geom = \(n) rgeom(n = n, prob = 1 / (1 + isolated_r0))
      )
      offspring_opts(community = community, isolated = isolated)
    }

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

    delays <- reactive({
      delay_opts(
        incubation_period = incubation_period(),
        onset_to_isolation = onset_to_isolation()
      )
    })

    simulate <- reactiveVal(0L)

    observeEvent(input$simulate, {
      req(!is.na(input$replicates))
      # one simulation run per replicate per swept R0 value
      total_sims <- input$replicates * length(r0_seq())
      if (total_sims > 500) {
        showModal(modalDialog(
          title = "Warning: Running lots of simulations!",
          paste0(
            "This will run ", total_sims, " outbreak simulations (",
            input$replicates, " replicates × ", length(r0_seq()),
            " R0 values) and may take a considerable amount of time."
          ),
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

    # sweep over R0: reduced to one summary row per replicate.
    # `cases_per_gen` is a list-column repeated across the
    # weekly rows of each replicate, so its first element holds the full
    # per-generation case counts. `scenario_sim()` pads every replicate to a
    # common week range with trailing zero-case weeks, so outbreak duration is
    # the last week with new cases (not `max(week)`).
    sweep <- eventReactive(simulate(), {
      req(simulate() > 0)
      waiter_show(
        html = loading,
        color = transparent(0.75)
      )
      on.exit(waiter_hide())
      req(input$replicates >= 1)
      req(input$initial_cases >= 1)
      req(input$asymptomatic >= 0 && input$asymptomatic <= 100)
      req(input$presymptomatic_transmission >= 0 && input$presymptomatic_transmission <= 100)
      req(input$symptomatic_traced >= 0 && input$symptomatic_traced <= 100)
      req(input$cap_max_days >= 1)
      req(input$cap_cases >= 1)

      # default to random seed if not specified by user
      if (is.na(input$seed)) {
        set.seed(runif(n = 1, min = 1, max = 1e5))
      } else {
        set.seed(input$seed)
      }

      r0s <- r0_seq()
      distribution <- input$community_offspring_distribution
      community_k <- input$community_disp
      isolated_r0 <- input$isolated_r0
      isolated_k <- input$isolated_disp
      delays_opt <- delays()
      event_probs <- event_prob_opts(
        # UI collects percentages; the model expects proportions (0-1)
        asymptomatic = input$asymptomatic / 100,
        presymptomatic_transmission = input$presymptomatic_transmission / 100,
        # UI collects a percentage; the model expects a proportion (0-1)
        symptomatic_traced = input$symptomatic_traced / 100
      )
      interventions <- intervention_opts(quarantine = input$quarantine)
      sim <- sim_opts(cap_max_days = input$cap_max_days, cap_cases = input$cap_cases)

      per_r0 <- lapply(r0s, function(r0) {
        out <- scenario_sim(
          n = input$replicates,
          initial_cases = input$initial_cases,
          offspring = offspring(r0, distribution, community_k, isolated_r0, isolated_k),
          delays = delays_opt,
          event_probs = event_probs,
          interventions = interventions,
          sim = sim
        )
        summ <- out[, list(
          size = max(cumulative),
          generations = length(cases_per_gen[[1L]]),
          weeks = max(week[weekly_cases > 0L])
        ), by = sim]
        summ$r0 <- r0
        summ
      })
      do.call(rbind, per_r0)
    })

    # stacked bar of binned outbreak metric (`value`), one stacked column per R0,
    # showing the proportion of outbreaks in each category.
    stacked_bar <- function(df, value, breaks, labels, legend_title, palette) {
      df$cat <- cut(value, breaks = breaks, labels = labels, right = TRUE)
      # proportion within each R0 column (factor keeps R0 columns ordered)
      props <- prop.table(
        table(df$cat, factor(df$r0, levels = sort(unique(df$r0)))),
        margin = 2L
      )
      cols <- grDevices::colorRampPalette(palette)(length(labels))
      op <- par(mar = c(5, 5, 2, 8), xpd = TRUE)
      on.exit(par(op))
      barplot(
        props,
        col = cols,
        border = "white",
        xlab = expression("Community " * R[0]),
        ylab = "Proportion of outbreaks",
        ylim = c(0, 1),
        las = 1
      )
      legend(
        x = par("usr")[2] + 0.2,
        y = 1,
        legend = rev(labels),
        fill = rev(cols),
        border = NA,
        bty = "n",
        title = legend_title
      )
    }

    output$size_dist <- renderPlot({
      df <- sweep()
      stacked_bar(
        df,
        value = df$size,
        breaks = c(-Inf, 5, 10, 50, 100, Inf),
        labels = c("1-5", "6-10", "11-50", "51-100", ">100"),
        legend_title = "Outbreak size",
        palette = c("#deebf7", "#08306b")
      )
    })

    output$length_generations <- renderPlot({
      df <- sweep()
      stacked_bar(
        df,
        value = df$generations,
        breaks = c(-Inf, 1, 2, 5, 10, Inf),
        labels = c("1", "2", "3-5", "6-10", ">10"),
        legend_title = "Generations",
        palette = c("#e5f5e0", "#00441b")
      )
    })

    output$length_time <- renderPlot({
      df <- sweep()
      stacked_bar(
        df,
        value = df$weeks,
        breaks = c(-Inf, 0, 1, 2, 4, Inf),
        labels = c("0", "1", "2", "3-4", ">4"),
        legend_title = "Duration (weeks)",
        palette = c("#e5f5e0", "#00441b")
      )
    })

    # Parameter distribution plots ------------------------------------------
    output$incubation_dist_plot <- incubation_dist_plot(input)
    output$onset_to_isolation_dist_plot <- onset_to_isolation_dist_plot(input)
    output$presymptomatic_dist_plot <- presymptomatic_dist_plot(input)

    observeEvent(input$reset, {
      # set pathogen_defaults back to default
      updateSelectInput(session, "pathogen_defaults", selected = "disease_x")

      defaults <- PROPOSE_DEFAULTS[[input$pathogen_defaults]]
      reset_pathogen_params(session = session, defaults = defaults)

      # reset the R0 sweep inputs (not covered by reset_pathogen_params)
      updateNumericInput(session, "community_r0_from", value = 0.1)
      updateNumericInput(session, "community_r0_to", value = 1.1)
      updateNumericInput(session, "community_r0_by", value = 0.1)

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
      updateNumericInput(session, "cap_max_days", value = PROPOSE_DEFAULTS$cap_max_days)
      updateNumericInput(session, "cap_cases", value = PROPOSE_DEFAULTS$cap_cases)
      updateNumericInput(session, "replicates", value = 100)
      updateSliderInput(session, "initial_cases", value = 1)
    })
  })
}
