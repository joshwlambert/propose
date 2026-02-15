library(shiny)
library(bslib)
library(bsicons)
library(ringbp)
library(tinyplot)
library(bibtex)

ui <- page_navbar(
  title = "{propose}",
  nav_panel(
    "Home",

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
        actionButton("simulate", "Simulate outbreak")
      ),
      mainPanel(
        fluidRow(
          column(
            4,
            "Offspring distribution parameters: ",
            numericInput("community_r0", "Community R0:", value = 2),
            numericInput("community_disp", "Community Dispersion:", value = 1),
            numericInput("isolated_r0", "Isolated R0:", value = 0),
            numericInput("isolated_disp", "Isolated Dispersion:", value = 1)
          ),
          column(
            4,
            "Delay distribution parameters: ",
            numericInput("incubation_meanlog", "Incubation period meanlog:", value = 1.5),
            numericInput("incubation_sdlog", "Incubation period sdlog:", value = 0.4),
            numericInput("onset_to_isolation_meanlog", "Onset-to-isolation meanlog:", value = 2),
            numericInput("onset_to_isolation_sdlog", "Onset-to-isolation sdlog:", value = 0.5)
          ),
          column(
            4,
            "Event probabilities: ",
            numericInput("asymptomatic", "Probability asymptomatic:", value = 0.1),
            numericInput("presymptomatic_transmission", "Probability of presymptomatic transmission:", value = 0.1),
            numericInput("symptomatic_ascertained", "Probability of contact traced:", value = 0.8)
          )
        ),
        fluidRow(
          column(
            6,
            "Interventions: ",
            textInput("quarantine", "Quarantine:", value = "FALSE")
          ),
          column(
            6,
            "Simulation controls: ",
            numericInput("cap_max_days", "Maximum number of days:", value = 100),
            numericInput("cap_cases", "Maximum number of cases:", value = 5000)
          )
        ),
        value_box(
          title = "Probability of outbreak control",
          value = uiOutput("extinct"),
          showcase = bs_icon("virus"),
          theme = "bg-gradient-blue-purple"
        ),
        plotOutput("cumulative_cases")
      )
    )
  ),
  nav_panel(
    title = "Citation",
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
  )
)

server <- function(input, output, session) {
  scenario <- eventReactive(input$simulate, {
    scenario_sim(
      n = input$replicates,
      initial_cases = input$initial_cases,
      offspring = offspring_opts(
        community = \(n) rnbinom(n = n, mu = input$community_r0, size = input$community_disp),
        isolated = \(n) rnbinom(n = n, mu = input$isolated_r0, size = input$isolated_disp)
      ),
      delays = delay_opts(
        incubation_period = \(n) rlnorm(
          n = n,
          meanlog = input$incubation_meanlog,
          sdlog = input$incubation_sdlog
        ),
        onset_to_isolation = \(n) rlnorm(
          n = n,
          meanlog = input$onset_to_isolation_meanlog,
          sdlog = input$onset_to_isolation_sdlog
        )
      ),
      event_probs = event_prob_opts(
        asymptomatic = input$asymptomatic,
        presymptomatic_transmission = input$presymptomatic_transmission,
        symptomatic_ascertained = input$symptomatic_ascertained
      ),
      interventions = intervention_opts(quarantine = as.logical(input$quarantine)),
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
  output$propose_citation <- renderPrint(citation(package = "propose"))
  output$ringbp_citation <- renderPrint(citation(package = "ringbp"))
  output$paper_citations <- renderPrint(bibtex::read.bib(file.path("www", "references.bib")))
}

shinyApp(ui = ui, server = server)