library(shiny)
library(shinyFeedback)
library(bslib)
library(bsicons)
library(ringbp)
library(tinyplot)
library(bibtex)

ui <- page_navbar(
  title = actionLink("go_home", "{propose}", style = "color: inherit; text-decoration: none;"),
  id = "navbarid",
  fillable = FALSE,
  nav_panel(
    title = "Home",
    icon = bs_icon("house"),

    # hero Section
    div(
      class = "bg-light p-5 rounded-lg m-3 text-center",
      img(
        src = "logo.png",
        style = "height: 180px; margin-bottom: 2rem; filter: drop-shadow(0 5px 15px rgba(0,0,0,0.1));"
      ),
      h1("Control Pandemics Early", class = "display-4"),
      p(
        "{propose} is a web interface for modelling targeted individual-level interventions for infectious disease outbreak control.",
        class = "lead"
      ),
      hr(class = "my-4"),
      br(),
      actionButton(
        "go_explore",
        "Start Exploring",
        class = "btn-lg",
        style = "background-color: #000080; color: white; border: none; padding: 10px 30px;"
      )
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
    explore_ui("explore")
  ),
  nav_panel(
    title = "Compare",
    icon = bs_icon("circle-square"),

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
        "About",
        style = "margin: 0;"
      ),

      # Logo
      tags$img(
        src = "logo.svg",
        style = "width: 110px;"
      )
    ),

    actionButton(
      "add_scenario",
      "Add Scenario",
      icon = icon("plus"),
      class = "btn-lg",
      style = "background-color: #000080; color: white; border: none; padding: 10px 30px;"
    ),

    uiOutput("compare_ui")
  ),
  nav_panel(
    title = "About",
    icon = bs_icon("info-square"),
    about_ui("about")
  ),
  nav_panel(
    title = "Citation",
    icon = bs_icon("feather"),
    citation_ui("citation")
  ),
  nav_panel(
    title = "Funding",
    icon = bs_icon("globe2"),
    funding_ui("funding")
  ),
  nav_panel(
    title = "Contact Us",
    icon = bs_icon("envelope"),
    contact_ui("contact")
  )
)

server <- function(input, output, session) {

  # make Shiny name a link to homepage
  observeEvent(input$go_home, {
    updateTabsetPanel(session, "navbarid", selected = "Home")
  })

  # logic to switch tabs when the Hero button is clicked
  observeEvent(input$go_explore, {
    updateTabsetPanel(session, "navbarid", selected = "Explore")
  })

  explore_server("explore")

  # track scenarios being compared
  scenarios <- reactiveValues(s1 = NULL, s2 = NULL, count = 0)

  # We use eventReactive so the UI only generates when the button is clicked
  output$compare_ui <- renderUI({

    # Check if the button has been clicked at least once
    if (input$add_scenario == 0) return(NULL)

    # Wrap multiple inputs in a tagList
    tagList(

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
      ),
      actionButton(
        "submit_scenario",
        "Submit Scenario",
        class = "btn-lg",
        style = "background-color: #000080; color: white; border: none; padding: 10px 30px;"
      )
    )
  })

  about_server("about")
  citation_server("citation")
}

shinyApp(ui = ui, server = server)