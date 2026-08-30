#' Shiny UI for ***Compare*** page
#'
#' @description
#' The sidebar is deliberately the ***Explore*** page's sidebar, built from the
#' same input functions in the same order, so the two pages read the same way.
#' It is split into two parts:
#'
#' * a **shared settings** card holding the simulation settings that are held
#'   constant across scenarios ([replicates_input()], [initial_cases_input()]
#'   and [sim_input()]), and
#' * everything below it, which describes the **scenario currently being
#'   built** and is snapshotted by the "Add scenario" button.
#'
#' Scenarios are added one at a time and simulated together by a single "Run
#' comparison". Because nothing is simulated until then, every scenario in a
#' comparison is necessarily run under the same shared settings, which is what
#' makes the probabilities of outbreak control comparable between them.
#'
#' @inheritParams shiny::moduleServer
#'
#' @return Output from [shiny::tagList()].
#' @keywords internal
compare_ui <- function(id) {
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

      /* colour key linking a scenario to its series in every plot */
      .scenario-swatch {
        display: inline-block;
        width: 0.85rem;
        height: 0.85rem;
        border-radius: 50%;
        margin-right: 0.5rem;
        vertical-align: baseline;
      }

      /* the scenario's colour identifies the box without tinting the value */
      .scenario-box {
        border-left-width: 6px !important;
        border-radius: var(--bs-border-radius);
      }

      /* highlight the cells of the parameter table that actually differ */
      .compare-table td.differs {
        font-weight: 600;
      }
      .compare-table th, .compare-table td {
        padding: 0.4rem 0.6rem;
      }
    "))
    ),

    page_title("Compare Outbreak Scenarios"),

    sidebarLayout(
      sidebarPanel(
        card(
          card_header(
            "Shared across all scenarios",
            tooltip(
              bs_icon("info-circle"),
              "These settings are applied to every scenario in the comparison.
              They are held constant so that the scenarios differ only in the
              parameters you change: the maximum number of cases in particular
              defines when an outbreak counts as controlled, so probabilities of
              outbreak control are only comparable between scenarios that share
              it. The seed is drawn once per comparison and reused for every
              scenario, which makes a comparison reproducible: the same settings
              give the same answer every time."
            )
          ),
          # numeric rather than a slider, and defaulting higher than the
          # Explore page: a comparison is only as good as its confidence
          # intervals, and separating scenarios needs more replicates than
          # looking at one
          replicates_input(ns = ns, page = "compare", numeric = TRUE),
          initial_cases_input(ns = ns),
          sim_input(ns = ns)
        ),
        div(
          class = "d-flex gap-2 mb-2",
          actionButton(
            ns("add_scenario"),
            "Add scenario",
            class = "btn-primary flex-fill text-wrap px-2",
            icon = icon("plus"),
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
        div(
          class = "d-flex gap-2 mb-3",
          actionButton(
            ns("run_comparison"),
            "Run comparison",
            class = "btn-primary flex-fill text-wrap px-2",
            style = "min-width: 0;"
          ),
          actionButton(
            ns("clear_all"),
            "Clear all",
            class = "btn-outline-secondary flex-fill text-wrap px-2",
            icon = icon("trash-can"),
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
        )
      ),
      mainPanel(
        accordion(
          open = FALSE,
          accordion_panel(
            title = "Show simulation parameter distributions",
            icon = bs_icon("bar-chart-line"),
            tags$p(
              class = "small text-muted",
              "These show the parameters currently set in the sidebar, which are
              the parameters of the scenario you are about to add."
            ),
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

        # full-width banner to notify the simulation controls have changed
        uiOutput(ns("stale_banner")),

        card(
          card_header(
            class = "d-flex justify-content-between align-items-center",
            "Scenarios in this comparison",
            uiOutput(ns("scenarios_badge"), inline = TRUE)
          ),
          uiOutput(ns("scenario_list"))
        ),

        card(
          card_header(
            class = "d-flex justify-content-between align-items-center",
            "What differs between the scenarios",
            uiOutput(ns("diff_badge"), inline = TRUE)
          ),
          uiOutput(ns("diff_table"))
        ),

        card(
          card_header(
            class = "d-flex justify-content-between align-items-center",
            "Probability of outbreak control",
            uiOutput(ns("results_badge"), inline = TRUE)
          ),
          uiOutput(ns("control_boxes")),
          plotOutput(ns("control_plot")),
          tags$p(
            class = "small text-muted mt-2",
            "The proportion of simulated outbreaks that were controlled under
            each scenario, with a 95% confidence interval. Intervals that
            overlap indicate the scenarios cannot be clearly separated at this
            number of replicates; increasing the replicates in the shared
            settings narrows them."
          )
        ),

        card(
          card_header(
            class = "d-flex justify-content-between align-items-center",
            "Outbreak Projections",
            div(
              class = "d-flex align-items-center gap-3",
              uiOutput(ns("projections_badge"), inline = TRUE),
              div(class = "custom-pill-toggle",
                  radioButtons(
                    inputId = ns("plot_style"),
                    label = NULL,
                    choices = c("Mean & CI" = "summary", "Trajectories" = "indiv"),
                    inline = TRUE
                  )
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
                the course of each outbreak, with one colour per scenario. The
                Mean & CI view shows the mean across replicates with a 95%
                interval; the Trajectories view shows every simulated outbreak
                individually. Dashed lines mark the maximum case and time caps
                at which the simulation stops."
              )
            ),
            nav_panel(
              "Weekly cases",
              plotOutput(ns("weekly_cases")),
              tags$p(
                class = "small text-muted mt-2",
                "The number of new cases each week, counted by date of symptom
                onset, with one colour per scenario."
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
        ),

        card(
          card_header(
            class = "d-flex justify-content-between align-items-center",
            uiOutput(ns("size_title"), inline = TRUE),
            uiOutput(ns("size_badge"), inline = TRUE)
          ),
          uiOutput(ns("size_boxes")),
          tags$p(
            class = "small text-muted mt-2",
            "The total number of cases each scenario reached by the end of the
            simulation, as the middle outbreak of those simulated with a 95%
            interval around it. Half of simulated outbreaks fall below the
            headline figure and half above; the upper end of the interval is the
            reasonable worst case to plan against. The horizon is the maximum
            number of days set in the shared settings, so lengthening it lets
            outbreaks that are still growing accumulate more cases."
          )
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

    # Building the set of scenarios -------------------------------------------
    # each element is a record of the canonical parameters (which define the
    # simulation, and by which scenarios are compared and deduplicated) and the
    # raw sidebar values (kept only so a scenario can be loaded back into the
    # sidebar in the mode it was entered in). Labels and colours are derived
    # from the set rather than stored, so they stay consistent as scenarios are
    # added and removed.
    scenarios <- reactiveVal(list())

    scenario_params <- reactive(lapply(scenarios(), `[[`, "params"))
    # the pathogen preset is not a model parameter, so it is not part of the
    # canonical parameters; it is read back off the raw snapshot to name the
    # scenario
    scenario_pathogens <- reactive(vapply(
      scenarios(), function(s) as_chr_scalar(s$raw$pathogen_defaults), character(1)
    ))
    # short names go where space is scarce (plot legends, the parameter table
    # header, value box titles); full names go in the scenario list, and on the
    # value boxes as a tooltip
    names_ <- reactive(scenario_names(scenario_params(), scenario_pathogens()))
    labels <- reactive(names_()$short)
    colours <- reactive(COMPARE_PALETTE[seq_along(scenarios())])

    observeEvent(input$add_scenario, {
      current <- scenarios()
      if (length(current) >= MAX_SCENARIOS) {
        showNotification(
          sprintf(
            "At most %d scenarios can be compared at once. Remove one first.",
            MAX_SCENARIOS
          ),
          type = "error"
        )
        return()
      }

      raw <- reactiveValuesToList(input)
      params <- canonical_params(raw)

      problems <- validate_params(params)
      if (length(problems) > 0) {
        showNotification(
          tagList(
            tags$b("This scenario cannot be added:"),
            tags$ul(lapply(problems, tags$li))
          ),
          type = "error",
          duration = 10
        )
        return()
      }

      duplicate <- which(vapply(
        current, function(s) identical(s$params, params), logical(1)
      ))
      if (length(duplicate) > 0) {
        showNotification(
          sprintf(
            "These parameters are identical to \"%s\", which is already in the
             comparison. Change a parameter before adding another scenario.",
            names_()$full[duplicate[1]]
          ),
          type = "error",
          duration = 10
        )
        return()
      }

      scenarios(c(current, list(list(params = params, raw = raw))))
    })

    observeEvent(input$clear_all, {
      scenarios(list())
      # discard the previous run too, rather than leaving results on screen for
      # scenarios that no longer exist
      run(0L)
      results_key(NULL)
    })

    # Remove and load buttons are rendered per scenario, but the number of
    # scenarios is capped, so the observers can be registered once here rather
    # than accumulating a new one each time the list is re-rendered.
    lapply(seq_len(MAX_SCENARIOS), function(i) {
      observeEvent(input[[paste0("remove_", i)]], {
        # re-rendering the scenario list recreates these buttons, which resets
        # their click count to zero; that is a change, so guard against acting
        # on it as though it were a click
        req(input[[paste0("remove_", i)]] > 0)
        current <- scenarios()
        req(length(current) >= i)
        scenarios(current[-i])
      })
      observeEvent(input[[paste0("load_", i)]], {
        req(input[[paste0("load_", i)]] > 0)
        current <- scenarios()
        req(length(current) >= i)
        apply_params(session = session, vals = current[[i]]$raw)
        showNotification(
          sprintf("Loaded \"%s\" into the sidebar.", names_()$full[i]),
          type = "message"
        )
      })
    })

    output$scenario_list <- renderUI({
      current <- scenarios()
      if (length(current) == 0) {
        return(
          div(
            class = "text-center text-muted py-3",
            bs_icon("layers", size = "2rem"),
            tags$p(
              class = "mb-0 mt-2",
              "No scenarios yet. Set the parameters in the sidebar and press
              \"Add scenario\", then change a parameter and add another."
            )
          )
        )
      }
      scenario_names_now <- names_()
      scenario_colours <- colours()
      tagList(
        lapply(seq_along(current), function(i) {
          div(
            class = "d-flex justify-content-between align-items-center border-bottom py-2",
            div(
              tags$span(
                class = "scenario-swatch",
                style = sprintf("background-color: %s;", scenario_colours[i])
              ),
              # the full name here, where there is room for every departure
              tags$b(scenario_names_now$full[i])
            ),
            div(
              class = "d-flex gap-2",
              actionButton(
                ns(paste0("load_", i)),
                "Load into sidebar",
                class = "btn-sm btn-outline-secondary",
                icon = icon("pen-to-square")
              ),
              actionButton(
                ns(paste0("remove_", i)),
                "Remove",
                class = "btn-sm btn-outline-danger",
                icon = icon("xmark")
              )
            )
          )
        }),
        tags$p(
          class = "small text-muted mt-3 mb-0",
          sprintf(
            "%d of %d scenarios. Press \"Run comparison\" to simulate them.",
            length(current), MAX_SCENARIOS
          )
        )
      )
    })

    # Running the comparison --------------------------------------------------
    shared <- reactive({
      # the numeric replicates box has no upper bound and can be cleared, so it
      # is guarded here rather than relying on the browser honouring `min`
      req(!is.na(input$replicates), input$replicates >= 1)
      req(!is.na(input$initial_cases), input$initial_cases >= 1)
      req(input$cap_max_days >= 1, input$cap_cases >= 1)
      list(
        replicates = input$replicates,
        initial_cases = input$initial_cases,
        cap_max_days = input$cap_max_days,
        cap_cases = input$cap_cases
      )
    })

    run <- reactiveVal(0L)

    observeEvent(input$run_comparison, {
      if (length(scenarios()) < 2) {
        showNotification(
          "Add at least two scenarios before running a comparison.",
          type = "error"
        )
        return()
      }
      if (is.na(input$replicates) || input$replicates < 1) {
        showNotification(
          "The number of simulation replicates must be at least 1.",
          type = "error"
        )
        return()
      }
      total <- input$replicates * length(scenarios())
      if (total > COMPARE_SIM_WARNING) {
        showModal(modalDialog(
          title = "Warning: Running lots of simulations!",
          sprintf(
            "This will run %d simulations (%d replicates for each of %d
             scenarios) and may take a considerable amount of time.",
            total, input$replicates, length(scenarios())
          ),
          footer = tagList(
            actionButton(ns("cancel"), "Cancel"),
            actionButton(ns("ok"), "Run", class = "btn btn-danger")
          )
        ))
      } else {
        run(run() + 1L)
      }
    })

    observeEvent(input$ok, {
      run(run() + 1L)
      removeModal()
    })
    observeEvent(input$cancel, {
      removeModal()
    })

    loading <- tagList(
      spin_hexdots(),
      h3("Simulating Outbreaks.", style = "color: #000080; margin-top: 40px;")
    )

    results <- eventReactive(run(), {
      req(run() > 0)
      current <- scenario_params()
      req(length(current) >= 2)
      settings <- shared()

      waiter_show(html = loading, color = transparent(0.75))
      on.exit(waiter_hide())

      # one seed for the whole comparison, reapplied before every scenario, so
      # all scenarios share the same random number stream (common random
      # numbers) and differences between them are driven by their parameters
      seed <- if (is.na(input$seed)) {
        as.integer(runif(n = 1, min = 1, max = 1e5))
      } else {
        input$seed
      }

      simulations <- lapply(current, function(params) {
        set.seed(seed)
        run_scenario(params, settings)
      })

      list(
        scenarios = current,
        # the names as they stood at run time, so the figures keep naming the
        # scenarios they were actually built from
        labels = names_()$short,
        full_labels = names_()$full,
        colours = COMPARE_PALETTE[seq_along(current)],
        settings = settings,
        simulations = simulations,
        # computed before the simulations are combined for plotting, while each
        # still carries the `extinct` attribute detect_extinct() reads
        control = lapply(simulations, control_stats),
        # taken from the raw simulations rather than the capped copies used for
        # plotting, so the reported size is the one the model produced
        size = lapply(simulations, outbreak_size_stats, cap_cases = settings$cap_cases)
      )
    })

    # a comparison is out of date once the scenarios or the shared settings have
    # changed since the run that produced the results on screen
    current_key <- reactive(list(scenarios = scenarios(), settings = shared()))
    results_key <- reactiveVal(NULL)
    observeEvent(results(), {
      results_key(isolate(current_key()))
    })

    stale <- reactive({
      if (is.null(results_key())) {
        return(FALSE)
      }
      !identical(current_key(), results_key())
    })

    # Shown on every card of the comparison, so the warning is visible wherever
    # the user happens to be looking when they change something. The wording
    # says "results" rather than "this", because on the scenario list and the
    # parameter table it is the results elsewhere that have fallen behind: those
    # two cards always reflect the current scenarios.
    stale_badge <- reactive({
      if (!isTRUE(stale())) {
        return(NULL)
      }
      tags$span(
        class = "badge bg-warning text-dark",
        bs_icon("exclamation-triangle-fill"),
        " Results are out of date — press Run comparison"
      )
    })
    output$scenarios_badge <- renderUI(stale_badge())
    output$diff_badge <- renderUI(stale_badge())
    output$results_badge <- renderUI(stale_badge())
    output$projections_badge <- renderUI(stale_badge())
    output$size_badge <- renderUI(stale_badge())

    # the same condition said once, loudly, at the top of the results. `role`
    # makes a screen reader announce it when it appears, which matters for a
    # message that arrives in response to an edit elsewhere on the page.
    output$stale_banner <- renderUI({
      if (!isTRUE(stale())) {
        return(NULL)
      }
      div(
        class = "alert alert-warning d-flex align-items-center",
        role = "alert",
        bs_icon("exclamation-triangle-fill"),
        tags$span(
          class = "ms-2",
          tags$b("Results are out of date."),
          " The scenarios or the shared settings have changed since these
          results were produced. Press \"Run comparison\" to update them."
        )
      )
    })

    # What differs between the scenarios --------------------------------------
    output$diff_table <- renderUI({
      current <- scenario_params()
      if (length(current) < 2) {
        return(
          div(
            class = "text-muted",
            "Add a second scenario to see what differs between them."
          )
        )
      }
      differences <- param_diff(current)
      scenario_names_now <- names_()
      scenario_colours <- colours()

      header <- tags$thead(tags$tr(
        tags$th("Parameter"),
        lapply(seq_along(current), function(i) {
          # short names in the header, which has one column per scenario; the
          # full name is a tooltip rather than a wider column
          tags$th(
            title = scenario_names_now$full[i],
            tags$span(
              class = "scenario-swatch",
              style = sprintf("background-color: %s;", scenario_colours[i])
            ),
            scenario_names_now$short[i]
          )
        })
      ))

      param_rows <- function(names, highlight) {
        lapply(names, function(nm) {
          tags$tr(
            tags$td(param_label(nm, current)),
            lapply(current, function(s) {
              value <- s[[nm]]
              # "not applicable" is a statement about this scenario, not a
              # repeat-of-the-cell-above mark, so it is spelled out
              if (length(value) != 1L || is.na(value)) {
                tags$td(
                  class = "text-muted fst-italic",
                  title = paste(
                    "Not applicable: this parameter cannot affect this",
                    "scenario, so it has no value here."
                  ),
                  "n/a"
                )
              } else {
                tags$td(
                  class = if (highlight) "differs" else NULL,
                  format_param(nm, value)
                )
              }
            })
          )
        })
      }

      varying <- if (length(differences$varying) == 0) {
        div(
          class = "alert alert-info small mb-0",
          bs_icon("info-circle-fill"),
          " These scenarios have identical parameters."
        )
      } else {
        tags$table(
          class = "table table-sm compare-table",
          header,
          tags$tbody(param_rows(differences$varying, highlight = TRUE))
        )
      }

      tagList(
        varying,
        if (length(differences$shared) > 0) {
          accordion(
            open = FALSE,
            accordion_panel(
              title = sprintf(
                "%d parameters do not differ between the scenarios",
                length(differences$shared)
              ),
              icon = bs_icon("check2-all"),
              tags$table(
                class = "table table-sm compare-table",
                tags$tbody(param_rows(differences$shared, highlight = FALSE))
              )
            )
          )
        },
        tags$p(
          class = "small text-muted mt-2 mb-0",
          tags$b("n/a"),
          " marks a parameter that cannot affect that scenario — the contact
          tracing settings of a scenario where cases are never isolated, for
          example. It does not mean the value is the same as another scenario's.
          Parameters are only listed as differing when they take two or more
          different values among the scenarios they do apply to."
        )
      )
    })

    # Probability of outbreak control -----------------------------------------
    output$control_boxes <- renderUI({
      if (run() == 0) {
        return(div(class = "text-muted", RUN_PROMPT))
      }
      res <- results()
      boxes <- lapply(seq_along(res$labels), function(i) {
        # The scenario's colour is carried by the left edge and the icon rather
        # than by the box background. Filling the box would tie the reading of
        # the headline probability to a categorical palette that carries no
        # meaning about it: a poorly controlled scenario would show up green and
        # a well controlled one orange. It would also be unreadable — only two
        # of the six palette colours have enough contrast for white text.
        tagAppendAttributes(
          value_box(
            title = tags$span(title = res$full_labels[i], res$labels[i]),
            value = signif(res$control[[i]]$p, digits = 2),
            control_ci_caption(res$control[[i]], class = "text-muted"),
            showcase = tags$div(
              bs_icon("virus"),
              style = sprintf("color: %s;", res$colours[i])
            )
          ),
          class = "scenario-box",
          style = sprintf("border-left: 6px solid %s;", res$colours[i])
        )
      })
      do.call(layout_column_wrap, c(list(width = 1 / 2), boxes))
    })

    # Total outbreak size ------------------------------------------------------
    output$size_title <- renderUI({
      if (run() == 0) {
        return("Total outbreak size")
      }
      sprintf("Total outbreak size after %s days", results()$settings$cap_max_days)
    })

    output$size_boxes <- renderUI({
      if (run() == 0) {
        return(div(class = "text-muted", RUN_PROMPT))
      }
      res <- results()
      boxes <- lapply(seq_along(res$labels), function(i) {
        size <- res$size[[i]]
        tagAppendAttributes(
          value_box(
            title = tags$span(title = res$full_labels[i], res$labels[i]),
            value = format_cases(size$median),
            tags$small(
              class = "text-muted",
              sprintf(
                "95%% interval: %s – %s cases",
                format_cases(size$lower), format_cases(size$upper)
              )
            ),
            # an outbreak that reached the cap was still growing when counting
            # stopped, so its size is a floor rather than a result
            if (size$capped > 0) {
              tags$small(
                class = "text-warning",
                bs_icon("exclamation-triangle-fill"),
                sprintf(
                  " %s%% of outbreaks reached the %s-case cap and were still growing",
                  signif(100 * size$capped, digits = 2),
                  format_cases(res$settings$cap_cases)
                )
              )
            },
            showcase = tags$div(
              bs_icon("people-fill"),
              style = sprintf("color: %s;", res$colours[i])
            )
          ),
          class = "scenario-box",
          style = sprintf("border-left: 6px solid %s;", res$colours[i])
        )
      })
      do.call(layout_column_wrap, c(list(width = 1 / 2), boxes))
    })

    output$control_plot <- renderPlot({
      validate(need(run() > 0, RUN_PROMPT))
      res <- results()
      # reversed so the first scenario appears at the top of the flipped axis
      order <- rev(seq_along(res$labels))
      df <- data.frame(
        scenario = factor(res$labels[order], levels = res$labels[order]),
        p = vapply(res$control[order], `[[`, numeric(1), "p"),
        lwr = vapply(res$control[order], `[[`, numeric(1), "lower"),
        upr = vapply(res$control[order], `[[`, numeric(1), "upper")
      )
      tinyplot(
        p ~ scenario | scenario,
        data = df,
        ymin = df$lwr,
        ymax = df$upr,
        type = "pointrange",
        pch = 19,
        cex = 1.2,
        lwd = 3,
        col = res$colours[order],
        ylim = c(0, 1),
        xlab = "",
        ylab = "Probability of outbreak control (95% CI)",
        legend = FALSE,
        theme = "clean",
        flip = TRUE
      )
    })

    # Layered outbreak projections --------------------------------------------
    # one row per (scenario, sim, week), with the scenario factor levels pinned
    # to the order scenarios were added so each keeps its colour
    combined <- reactive({
      res <- results()
      dt <- data.table::rbindlist(lapply(
        seq_along(res$simulations),
        function(i) {
          capped <- cap_scenario(res$simulations[[i]], res$settings$cap_cases)
          capped[, scenario := res$labels[i]][]
        }
      ))
      dt[, scenario := factor(scenario, levels = res$labels)]
      dt[]
    })

    # Draw one layered projection plot. `column` is the column of the combined
    # simulations to plot on the y axis; `ylab` is its axis label. Both views
    # colour by scenario, using the same palette as the scenario list and the
    # value boxes.
    projection_plot <- function(column, ylab) {
      res <- results()
      dt <- combined()

      if (input$plot_style == "summary") {
        summ <- dt[
          ,
          list(
            mean = mean(.SD[[1L]]),
            lwr = stats::quantile(.SD[[1L]], 0.025, names = FALSE),
            upr = stats::quantile(.SD[[1L]], 0.975, names = FALSE)
          ),
          by = list(scenario, week),
          .SDcols = column
        ]
        data.table::setorder(summ, scenario, week)
        tinyplot(
          mean ~ week | scenario,
          data = summ,
          ymin = summ$lwr,
          ymax = summ$upr,
          type = "ribbon",
          lwd = 3,
          # explicit colours rather than `palette`, which tinyplot evaluates in
          # an environment where local variables are not visible
          col = res$colours,
          ylab = paste(ylab, "(Mean & 95% CI)"),
          xlab = "Week",
          theme = "clean"
        )
      } else {
        # One line per replicate, coloured by its scenario. The grouping that
        # gives each replicate its own line is (scenario, sim), so the palette
        # is expanded to repeat each scenario's colour across its replicates,
        # and the per-group legend is replaced by a per-scenario one.
        trajectories <- dt[
          , head(.SD, which.max(cumulative)), by = list(scenario, sim)
        ]
        data.table::setorder(trajectories, scenario, sim, week)
        trajectories[, key := factor(
          paste(scenario, sim), levels = unique(paste(scenario, sim))
        )]
        replicates_per_scenario <- trajectories[
          , data.table::uniqueN(sim), by = scenario
        ]$V1
        tinyplot(
          stats::as.formula(paste(column, "~ week | key")),
          data = trajectories,
          type = "l",
          lwd = 2,
          col = rep(res$colours, times = replicates_per_scenario),
          legend = FALSE,
          ylab = ylab,
          xlab = "Week",
          theme = "clean"
        )
        legend(
          "topleft",
          legend = res$labels,
          col = res$colours,
          lwd = 2,
          bty = "n",
          cex = 0.9
        )
      }
      abline(v = res$settings$cap_max_days / 7, lty = 2, col = "grey50")
      invisible(NULL)
    }

    output$cumulative_cases <- renderPlot({
      validate(need(run() > 0, RUN_PROMPT))
      projection_plot("cumulative", "Cumulative number of cases")
      abline(h = results()$settings$cap_cases, lty = 2, col = "grey50")
    })

    output$weekly_cases <- renderPlot({
      validate(need(run() > 0, RUN_PROMPT))
      projection_plot("weekly_cases", "Number of cases per week")
    })

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
      updateNumericInput(session, "replicates", value = PROPOSE_DEFAULTS$replicates$compare)
      updateSliderInput(session, "initial_cases", value = PROPOSE_DEFAULTS$initial_cases)
    })
  })
}
