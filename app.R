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
    compare_ui("compare")
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
  compare_server("compare")
  about_server("about")
  citation_server("citation")
}

shinyApp(ui = ui, server = server)