library(shiny)
library(shinyFeedback)
library(bslib)
library(bsicons)
library(ringbp)
library(tinyplot)
library(bibtex)
library(waiter)

ui <- page_navbar(
  title = actionLink("go_home", "{propose}", style = "color: inherit; text-decoration: none;"),
  theme = bs_theme(brand = TRUE),
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
    ),
    # Docs banner
    tags$div(
      style = "background: linear-gradient(90deg, #000080 0%, #3949ab 50%, #5c6bc0 100%);
               color: white;
               padding: 3rem 2rem;
               margin-top: 3rem;
               display: flex;
               align-items: center;
               justify-content: center;
               gap: 1.5rem;
               flex-wrap: wrap;
               text-align: center;",
      bs_icon("book", size = "3rem"),
      tags$div(
        style = "text-align: left;",
        tags$p(
          "New to {propose}?",
          class = "mb-1",
          style = "font-size: 1.25rem; font-weight: 300;"
        ),
        actionLink(
          "go_manual",
          tagList("Read the manual ", bs_icon("arrow-right")),
          style = "color: white; font-weight: 600; font-size: 1.1rem; text-decoration: underline;"
        )
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
  nav_menu(
    title = "Docs",
    icon = bs_icon("book"),
    nav_item(tags$h6(tags$b("{propose} documentation"), class = "dropdown-header")),
    nav_item(tags$hr(class = "dropdown-divider")),
    nav_panel(
      title = "{propose} manual",
      manual_ui("manual")
    ),
    nav_item(tags$hr(class = "dropdown-divider")),
    nav_item(tags$h6(tags$b("{ringbp} documentation"), class = "dropdown-header")),
    nav_item(tags$hr(class = "dropdown-divider")),
    nav_panel(
      title = "Getting Started with {ringbp}",
      docs_ui("docs_main", "ringbp.html")
    ),
    nav_panel(
      title = "{ringbp} Model Description",
      docs_ui("docs_model", "ringbp-model.html")
    ),
    nav_panel(
      title = "Parameter Sweep with {ringbp}",
      docs_ui("docs_sweep", "parameter-sweep.html")
    )
  ),
  nav_panel(
    title = "About",
    icon = bs_icon("info-square"),
    about_ui("about")
  ),
  nav_panel(
    title = "FAQs",
    icon = bs_icon("question-square"),
    faq_ui("faq")
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
  ),
  footer = tags$div(
    style = "padding: 20px; border-top: 1px solid #eee; margin-top: 50px; text-align: center; color: #888;",
    tags$p("© 2026 {propose} project."),
    tags$p("Powered by {ringbp}"),
    tags$a(
      href = "https://github.com/joshwlambert/propose",
      bs_icon("github", size = "2rem", title = "View source on GitHub"),
      style = "color: #24292e; text-decoration: none;"
    )
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

  # logic to jump to the manual from the Home page docs banner
  observeEvent(input$go_manual, {
    updateTabsetPanel(session, "navbarid", selected = "{propose} manual")
  })

  # file path for {ringbp} vignettes rendered in docs UI
  addResourcePath(
    prefix = 'ringbp_docs',
    directoryPath = system.file("doc", package = "ringbp")
  )

  explore_server("explore")
  compare_server("compare")
  about_server("about")
  citation_server("citation")
  manual_server("manual", session)
}

shinyApp(ui = ui, server = server)
