library(shiny)
library(bslib)
library(bsicons)
library(ringbp)
library(tinyplot)
library(bibtex)

ui <- page_navbar(
  title = "{propose}",
  id = "navbarid",
  nav_panel(
    title = "Home",
    icon = bs_icon("house"),

    # Hero Section
    div(class = "bg-light p-5 rounded-lg m-3 text-center",
        h1("Control Pandemics Early", class = "display-4"),
        p(
          "{propose} is a web interface for modelling targeted individual-level interventions for outbreak control.",
          class = "lead"
        ),
        hr(class = "my-4"),
        actionButton("explore", "Start Exploring", class = "btn-primary btn-lg")
    ),
    # Feature Cards
    layout_column_wrap(
      width = 1/2,
      card(
        card_header(bs_icon("shield-check"), "Built by epidemiologists"),
        tags$p("Built by epidemiologists with experience working on: COVID-19, Ebola, Mpox and more."),
        tags$p("The simulation model has been peer-reviewed and was used to inform the COVID-19 response.")
      ),
      card(
        card_header(bs_icon("graph-up"), "Gain Insights"),
        "Out of the box structured analyses and visualisations provided in {propose} enable
        quick scientific and policy-relevant insights"
      )
    )
  ),
  nav_panel(
    title = "Explore",
    icon = bs_icon("sliders"),

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

    tags$div(
      style = "
      display: flex;
      justify-content: space-between;
      align-items: center;
      margin-bottom: 20px;
    ",

      # Title (styled like titlePanel)
      tags$h2(
        class = "title",
        "{propose}: a Shiny app for {ringbp}",
        style = "margin: 0;"
      ),

      # Logo
      tags$img(
        src = "logo.svg",
        style = "width: 110px;"
      )
    ),


    sidebarLayout(
      sidebarPanel(
        sliderInput("replicates", "Number of simulation replicates:", min = 1, max = 100, value = 10),
        sliderInput("initial_cases", "Number of initial cases:", min = 1, max = 50, value = 5),
        actionButton("simulate", "Simulate outbreak"),
        accordion(
          accordion_panel(
            title = "Offspring distribution parameters:",
            icon = bs_icon("diagram-3-fill"),
            selectInput(
              inputId = "community_offspring_distribution",
              label = "Community Offspring Distribution",
              choices = list(
                "Negative Binomial" = "nbinom",
                "Poisson" = "pois",
                "Geometric" = "geom",
                "Custom" = "custom"
              )
            ),
            conditionalPanel(
              condition = "input.community_offspring_distribution == 'nbinom'",
              numericInput("community_r0", "Community R0:", value = 2),
              numericInput("community_disp", "Community Dispersion:", value = 1),
            ),
            conditionalPanel(
              condition = "input.community_offspring_distribution == 'pois'",
              numericInput("community_r0", "Community R0:", value = 2)
            ),
            selectInput(
              inputId = "isolated_offspring_distribution",
              label = "Isolated Offspring Distribution",
              choices = list(
                "Negative Binomial" = "nbinom",
                "Poisson" = "pois",
                "Geometric" = "geom",
                "Custom" = "custom"
              )
            ),
            conditionalPanel(
              condition = "input.isolated_offspring_distribution == 'nbinom'",
              numericInput("isolated_r0", "Isolated R0:", value = 0),
              numericInput("isolated_disp", "Isolated Dispersion:", value = 1),
            ),
            conditionalPanel(
              condition = "input.isolated_offspring_distribution == 'pois'",
              numericInput("isolated_r0", "Isolated R0:", value = 0)
            ),
            conditionalPanel(
              condition = "input.isolated_offspring_distribution == 'geom'",
              numericInput("isolated_r0", "Isolated R0:", value = 0)
            ),
            conditionalPanel(
              condition = "input.isolated_offspring_distribution == 'custom'",
              textInput("isolated_offspring", "Isolated Offspring Distribution:")
            )
          ),
          open = FALSE
        ),
        accordion(
          accordion_panel(
            title = "Delay distribution parameters:",
            icon = bs_icon("hourglass-split"),
            selectInput(
              inputId = "incubation_distribution",
              label = "Incubation Period Distribution",
              choices = list(
                "Lognormal" = "lnorm",
                "Gamma" = "gamma",
                "Weibull" = "weibull",
                "Custom" = "custom"
              )
            ),
            conditionalPanel(
              condition = "input.incubation_distribution == 'lnorm'",
              numericInput("incubation_meanlog", "Incubation period meanlog:", value = 1.5),
              numericInput("incubation_sdlog", "Incubation period sdlog:", value = 0.4),
            ),
            conditionalPanel(
              condition = "input.incubation_distribution == 'gamma'",
              numericInput("incubation_shape", "Incubation period shape:", value = 2),
              numericInput("incubation_scale", "Incubation period scale:", value = 1),
            ),
            conditionalPanel(
              condition = "input.incubation_distribution == 'weibull'",
              numericInput("incubation_shape", "Incubation period shape:", value = 2),
              numericInput("incubation_scale", "Incubation period scale:", value = 1),
            ),
            selectInput(
              inputId = "onset_to_isolation_distribution",
              label = "Onset-to-isolation Distribution",
              choices = list(
                "Lognormal" = "lnorm",
                "Gamma" = "gamma",
                "Weibull" = "weibull",
                "Custom" = "custom"
              )
            ),
            conditionalPanel(
              condition = "input.onset_to_isolation_distribution == 'lnorm'",
              numericInput("onset_to_isolation_meanlog", "Onset-to-isolation meanlog:", value = 2),
              numericInput("onset_to_isolation_sdlog", "Onset-to-isolation sdlog:", value = 0.5)
            ),
            conditionalPanel(
              condition = "input.onset_to_isolation_distribution == 'gamma'",
              numericInput("onset_to_isolation_shape", "Onset-to-isolation shape:", value = 2),
              numericInput("onset_to_isolation_scale", "Onset-to-isolation scale:", value = 1),
            ),
            conditionalPanel(
              condition = "input.onset_to_isolation_distribution == 'weibull'",
              numericInput("onset_to_isolation_shape", "Onset-to-isolation shape:", value = 2),
              numericInput("onset_to_isolation_scale", "Onset-to-isolation scale:", value = 1),
            )
          ),
          open = FALSE
        ),
        accordion(
          accordion_panel(
            title = "Event probabilities:",
            icon = bs_icon("person-fill-gear"),
            numericInput("asymptomatic", "Probability asymptomatic:", value = 0.1),
            numericInput("presymptomatic_transmission", "Probability of presymptomatic transmission:", value = 0.1),
            numericInput("symptomatic_ascertained", "Probability of contact traced:", value = 0.8)
          ),
          open = FALSE
        ),
        accordion(
          accordion_panel(
            title = "Interventions:",
            icon = bs_icon("shield-shaded"),
            checkboxInput("quarantine", "Quarantine", value = FALSE)
          ),
          open = FALSE
        ),
        accordion(
          accordion_panel(
            title = "Simulation controls: ",
            icon = bs_icon("gear-wide-connected"),
            numericInput("cap_max_days", "Maximum number of days:", value = 100),
            numericInput("cap_cases", "Maximum number of cases:", value = 5000)
          ),
          open = FALSE
        )
      ),
      mainPanel(
        value_box(
          title = "Probability of outbreak control",
          value = uiOutput("extinct"),
          showcase = bs_icon("virus"),
          theme = "bg-gradient-blue-purple"
        ),
        navset_card_underline(
          nav_panel("Cumulative cases", plotOutput("cumulative_cases")),
          nav_panel("Weekly cases", plotOutput("weekly_cases"))
        )
      )
    )
  ),
  nav_panel(
    title = "Citation",
    icon = bs_icon("feather"),
    titlePanel("Citation"),
    tags$div(
      markdown(
        "When using the `{propose}` Shiny app please cite the work using:"
      )
    ),
    verbatimTextOutput("propose_citation"),
    tags$div(
      markdown(
        "If you are additionally using the `{ringbp}` R package or would also
        like to cite the package with the epidemiological model powering
        `{propose}`, please use:"
      )
    ),
    verbatimTextOutput("ringbp_citation"),
    tags$div(
      tags$h3("Papers using {ringbp}")
    ),
    verbatimTextOutput("paper_citations"),
  ),
  nav_panel(
    title = "Funding",
    icon = bs_icon("globe2"),
    titlePanel("Funding"),
    tags$div(
      style = "padding-top: 30px; padding-bottom: 30px;",
      markdown(
        "`{propose}` is developed at the [Centre for the Mathematical
        Modelling of Infectious Diseases](https://www.lshtm.ac.uk/research/centres/centre-mathematical-modelling-infectious-diseases)
        at the [London School of Hygiene and Tropical Medicine](https://www.lshtm.ac.uk/)
        and is funded by [ESCAPE](https://www.escapepandemics.com/) and
        [HPRU-HAM](https://www.lshtm.ac.uk/research/centres-projects-groups/hpru-ham)."
      )
    ),

    tags$div(
      style = "display: flex; justify-content: center; align-items: center; gap: 20px; padding-top: 30px; padding-bottom: 30px;",
      tags$img(
        src = "ESCAPE_logo.avif",
        style = "width: 250px;"
      ),
      tags$img(
        src = "EU_logo.avif",
        style = "width: 250px;"
      )
    ),

    tags$div(
      style = "padding-top: 30px; padding-bottom: 30px;",
      tags$p(
        "The ESCAPE project is an EU-funded research initiative
    focused on improving how governments and public health agencies prepare for
    and respond to future pandemics, including emerging “Pathogen X” threats.
    It is a consortium of 8 European institutions. Building on lessons from
    COVID-19, the project aims to develop a science-based preparedness
    blueprint that supports faster, evidence-informed decision-making during
    outbreaks.",
      ),
      tags$p(
        "Through improved data readiness, modelling tools, and decision-support
    frameworks, ESCAPE seeks to strengthen coordination between researchers,
    policymakers, and the public. By identifying what worked — and what did
    not — in past responses, the project aims to reduce the health, societal,
    and economic impacts of future pandemics across Europe and beyond."
      )
    ),

    tags$div(
      style = "display: flex; justify-content: center; align-items: center; gap: 20px; padding-top: 30px; padding-bottom: 30px;",
      tags$img(
        src = "NIHR_logo.png",
        style = "width: 250px;"
      ),
      tags$img(
        src = "NIHR_hpru_ham_logo.webp",
        style = "width: 250px;"
      )
    ),

    tags$div(
      tags$p(
        "The NIHR Health Protection Research Unit in Health & Analytics
        Modelling (HPRU-HAM) is a strategic research partnership between
        the London School of Hygiene & Tropical Medicine (LSHTM), the UK
        Health Security Agency (UKHSA) and Imperial College London, funded
        by the National Institute for Health and Care Research (NIHR) to
        strengthen UK health protection research and practice. The unit
        conducts cutting-edge analytical research to improve the way diverse
        health data are integrated, interpreted, and used to inform
        real-time decision-making during public health emergencies. Its
        work spans developing statistical and mathematical methods, building
        modelling and simulation tools, and translating research into
        practical software and training to boost UKHSA’s capacity to analyse
        complex surveillance data and forecast emerging threats."
      ),

      tags$p(
        "HPRU-HAM’s research is organised around connected themes including
      health equity and inclusion, health forecasting, and pandemic
      preparedness and preparedness evaluation. These themes aim to
      elucidate drivers of outbreak spread and healthcare demand, synthesise
      evidence from large and varied data sources, assess cost and impact
      of interventions, and enhance analytical methods that support outbreak
      control and health system resilience. The unit also emphasises
      engaging with stakeholders to ensure its work informs public health
      policies, improves forecasting accuracy, and supports equitable and
      effective responses to current and future health threats.",
      )
    )
  ),
  nav_panel(
    title = "Contact Us",
    icon = bs_icon("envelope"),
    titlePanel("Contact Us"),

    card(
      card_header(
        class = "bg-light",
        bs_icon("info-square"),
        "Directions"
      ),
      card_body(
        markdown(
          "* If you have general question about `{propose}` please email (<font color='orange'>left</font>).
          * If you have a problem to report about `{propose}` please raise an
          issue on GitHub (<font color='green'>centre</font>).
          * If you would like to contribute to the project see instructions
          (<font color='blue'>right</font>)"
        )
      )
    ),

    layout_column_wrap(
      width = "200px", height = 300,
      card(
        card_header(
          class = "bg-warning",
          bs_icon("envelope"),
          "Email us"
        ),
        card_body(
          markdown(
            "To contact the lead developer of the `{propose}` app please email:
        joshua.lambert@lshtm.ac.uk"
          )
        )
      ),
      card(
        card_header(
          class = "bg-success",
          bs_icon("github"),
          "Raise an issue on GitHub"
        ),
        card_body(
          markdown(
            "If you have an issue with {propose} please file an issue on the
        GitHub repository at the link below:

        [`{propose}` Issue tracker](https://github.com/joshwlambert/propose/issues/new).

        You will need a GitHub account to do this."
          )
        )
      ),
      card(
        card_header(
          class = "bg-primary",
          bs_icon("file-plus"),
          "Contribute to the project"
        ),
        card_body(
          markdown(
            "If you have any questions about how to contribute to the project
        please see our [Contributing guidelines](https://github.com/joshwlambert/propose/blob/main/.github/CONTRIBUTING.md).

        `{propose}` is an open-source project, and contributions to the project
        will be fairly recognised.

        `{propose}` has an MIT license, therefore, you may copy and modify the
        app for your own purposes, however, we encourage you to contribute your
        changes to this project as they may benefit other users."
          )
        )
      )
    )
  )
)

server <- function(input, output, session) {

  # Logic to switch tabs when the Hero button is clicked
  observeEvent(input$explore, {
    updateTabsetPanel(session, "navbarid", selected = "Explore")
  })

  community <- reactive({
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
  output$propose_citation <- renderPrint(citation(package = "propose"))
  output$ringbp_citation <- renderPrint(citation(package = "ringbp"))
  output$paper_citations <- renderPrint(bibtex::read.bib(file.path("www", "references.bib")))
}

shinyApp(ui = ui, server = server)