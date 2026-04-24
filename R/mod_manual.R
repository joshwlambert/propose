#' Shiny UI for ***Manual*** page
#'
#' @inheritParams shiny::moduleServer
#'
#' @return Output from [shiny::tagList()].
#' @keywords internal
manual_ui <- function(id) {
  ns <- NS(id)

  tagList(
    page_title("Manual"),
    # This creates the vertical sidebar layout
    navset_pill_list(
      widths = c(3, 9), # Left column width (3) and right column width (9)

      "Overview",
      nav_panel("Overview",
                h2("Overview"),
                p("{propose} is a public health decision support tool for
                  early infectious disease outbreak response."),
                p("It runs an epidemic model where individuals are either in
                  the ", tags$em("community"), " or ", tags$em("isolated"), ".")
      ),

      "Getting Started",
      nav_panel("Quick Start",
                h2("Getting Started"),
                p("When you first open {propose} you land on the home page.
                  From here you can jump straight to the ", tags$em("Explore"),
                  " page by clicking on the 'Start Exploring' button, or you
                  can navigate the app using the tabs at the top of the page.
                  If these tabs are not shown, you can click the navbar toggler
                  (☰) to show the other pages.")


      ),
      nav_panel("Explore outbreak scenarios",
                h2("Using the Explore page"),
                h5("Understand how to explore outbreak scenarios in {propose}"),
                p("Once you are on the Explore page you can set up outbreak
                  scenarios by specifying the characteristics (parameters) of
                  the pathogen and the intervention, and then simulate
                  outbreaks and explore the results."),
                tags$figure(
                  style = "text-align: center; margin: 1.5rem 0;",
                  img(
                    src = "explore_workflow.png",
                    style = "max-width: 80%; height: auto; padding: 1rem;"
                  ),
                  tags$figcaption(
                    "Typical workflow for outbreak exploration.",
                    style = "font-size: 0.875rem; color: #6c757d; margin-top: 0.5rem;"
                  )
                ),
                h3("Select Pathogen", style = "margin-top: 2.5rem;"),
                p("A good way to start using the Explore page is to select a
                  Pathogen from the 'Select Pathogen Parameters'. This populates
                  the pathogen parameters with values representative of the
                  chosen pathogen. Currently, {propose} contains parameters for:"),
                tags$ul(
                  tags$li("COVID-19"),
                  tags$li("Ebola")
                ),
                p("By default, the chosen pathogen is Disease X, and the default
                  pathogen parameters are sensible initial values that act as a
                  starting point from which to modify and simulate outbreaks."),
                p("The pathogen and intervention parameters are grouped. By
                  clicking on, for example, 'Offspring distributions parameters'
                  it will expand and allow you to change the offspring distribution
                  type and distribution parameters. All groups of parameters expand
                  once clicked, and will minimise if clicked again."),
                p("If you want to reset the parameters in the Explore page back
                  to their default values you can click the 'Reset Defaults' button.
                  This resets the chosen pathogen to 'Disease X' and resets the
                  pathogen, intervention, and simulation control parameters back
                  to their default values."),
                h3("Toggle Parameters", style = "margin-top: 2.5rem;"),
                p("The Explore page sidebar is set up to let you modify every
                  aspect of the epidemic model. The top of the sidebar has two
                  always-visible sliders (number of replicates and initial cases),
                  and the rest of the sidebar is organised into three collapsible
                  groups: ", tags$em("Pathogen Parameters"), ", ",
                  tags$em("Intervention Parameters"), ", and ",
                  tags$em("Simulation Control Parameters"), ". Each panel within
                  a group expands when clicked and minimises when clicked again,
                  so you can focus on the parameters you're actively editing."),
                p(tags$b("A note on inline validation."), " Some inputs (for example
                  R0 values, probabilities, and simulation caps) are checked as
                  you type. If you enter a value that isn't allowed (e.g. a
                  negative R0, or a probability outside [0, 1]) the field is
                  highlighted and a short error message appears beneath it. Fix
                  the value before clicking 'Simulate outbreak'."),

                h4("Top of sidebar: replicates and initial cases", style = "margin-top: 1.5rem;"),
                p(tags$em("Number of simulation replicates"), " controls how many
                  independent outbreaks are simulated. More replicates give a
                  smoother estimate of the probability of outbreak control and
                  tighter confidence intervals, at the cost of longer run times.
                  ", tags$em("Number of initial cases"), " controls how many
                  infectious individuals seed each outbreak."),

                h4("Pathogen Parameters", style = "margin-top: 1.5rem;"),
                p("These describe the disease being modelled. They're what change
                  when you pick a preset from 'Select Pathogen Parameters'."),
                tags$ul(
                  tags$li(tags$b("Offspring distribution parameters."),
                          " Controls how many secondary cases each infectious
                          individual produces. You set this separately for people
                          in the ", tags$em("community"), " and for people who
                          are ", tags$em("isolated"), ". For each, choose a
                          distribution (Negative Binomial, Poisson, or Geometric)
                          and set R0. Negative Binomial also exposes a dispersion
                          parameter (often written as ", tags$em("k"),
                          " in the literature) — smaller values mean more
                          superspreading."),
                  tags$li(tags$b("Incubation period distribution parameters."),
                          " The delay between infection and symptom onset.
                          Choose a distribution (Lognormal, Gamma, or Weibull)
                          and set its parameters (meanlog/sdlog for Lognormal;
                          shape/scale for Gamma and Weibull)."),
                  tags$li(tags$b("Symptom event probabilities."),
                          " The probability that a case is asymptomatic, and the
                          probability that transmission happens before symptom
                          onset. Both values must be between 0 and 1.")
                ),

                h4("Intervention Parameters", style = "margin-top: 1.5rem;"),
                p("These describe how the public health response tries to control
                  transmission."),
                tags$ul(
                  tags$li(tags$b("Onset-to-isolation distribution parameters."),
                          " The delay between a person developing symptoms and
                          being isolated. As with the incubation period, you pick
                          a distribution family (Lognormal, Gamma, or Weibull)
                          and set its parameters. Shorter delays mean faster
                          isolation and typically better control."),
                  tags$li(tags$b("Contact tracing."),
                          " The probability that a contact of a symptomatic case
                          is successfully traced. Values closer to 1 represent
                          highly effective contact tracing systems."),
                  tags$li(tags$b("Interventions."),
                          " Tick the 'Quarantine' box to quarantine traced
                          contacts (preventing onward transmission before symptom
                          onset). Leave it unticked to model isolation of
                          symptomatic cases only.")
                ),

                h4("Simulation Control Parameters", style = "margin-top: 1.5rem;"),
                p("These control how each simulation is run, independent of the
                  disease or intervention."),
                tags$ul(
                  tags$li(tags$b("Maximum number of days."),
                          " A simulation stops once it reaches this many days.
                          The default is 100. Increase this if you want to
                          simulate longer outbreaks."),
                  tags$li(tags$b("Maximum number of cases."),
                          " A simulation stops once it reaches this many cases.
                          This prevents a single runaway outbreak from taking
                          a long time to finish."),
                  tags$li(tags$b("Seed for simulation model."),
                          " Leave blank for a different random seed each run, or
                          set a number to make the result reproducible — running
                          the same parameters with the same seed will produce
                          the same outbreaks.")
                ),
                h3("Simulate Outbreak", style = "margin-top: 2.5rem;"),
                p("The simulation is not automatically run when the parameter
                  values are changed. Once all the parameter values are set to
                  desired values, click 'Simulate outbreak' to run the
                  simulation model and produce results."),
                p("When the simulation is running a loading screen will appear
                  on the page. If the number of replicates is low (e.g. 5 (default))
                  then this will only briefly appear. However, as the number of
                  simulation replicates increases, it will require more time to
                  complete the simulation and produce the results."),
                p("If the number of simulation replicates exceeds 50 then a warning
                  box will pop up to explain that the simulation may take a long
                  time to complete. If you click 'Run' then the outbreak will be
                  simulated. If you click 'Cancel' then the outbreak will not be
                  simulated, and you'll be returned to the Explore page."),
                h3("Explore results", style = "margin-top: 2.5rem;"),
                p("Currently {propose} visualises two aspects of the outbreaks:"),
                tags$ul(
                  tags$li("Cumulative cases"),
                  tags$li("Weekly cases")
                ),
                p("and two types of data visualisations for each aspect of outbreak
                  data:"),
                tags$ul(
                  tags$li("Individual outbreak trajectories"),
                  tags$li("Mean and CI across outbreak replicates")
                ),
                p("The results from an outbreak simulation can be visualised using
                  each setting without having to rerun the simulation. Simply click
                  on the 'Trajectories' (default) or 'Mean & CI' button, and the
                  'Cumulative' (default) or 'Weekly cases' tab to explore each
                  data visualisation."),
                p("The probability of outbreak control (extinction) is also calculated, and
                  shown in the value box above the figure. This tells you what proportion
                  of the simulated outbreak replicates did not sustain human-to-human
                  transmission until the end of the simulation. By default the simulation
                  is set to run for 100 days, but this can be changed in the 'Simulation
                  Control parameters'.")
      ),
      nav_panel("Compare",
                h2("Comparing outbreak scenarios"),
                p("The Compare page is still a ", tags$em("work-in-progress."),
                  " This page will contain information on how to use the Compare
                  page once it is finished.")
      ),
      "Help & Support",
      nav_panel("Info",
                h2("Information about {propose}"),
                p("See ", actionLink(ns("go_about"), "the About page"), " for general information on {propose}."),
                h2("Frequently Asked Questions", style = "margin-top: 2.5rem;"),
                p("See ", actionLink(ns("go_faq"), "the FAQ page"), " for answers to common questions."),
                h2("Citing {propose}", style = "margin-top: 2.5rem;"),
                p(
                  "To cite {propose}, or the underlying epidemiological model, {ringbp}, or see
                  a list of papers that use the {ringbp} model, see",
                  actionLink(ns("go_citation"), "the Citation page.")
                ),
                h2("Funding", style = "margin-top: 2.5rem;"),
                p("See ", actionLink(ns("go_funding"), "the funding page"), " for information on the project funders."),
                h2("Report an issue or contribute to the project", style = "margin-top: 2.5rem;"),
                p("If you experience an issue that you'd like to report, or
                  would like to contribute to the development of {propose},
                  see the ", actionLink(ns("go_contact"), "the contact page.")),
      )
    )
  )
}

manual_server <- function(id, parent_session) {
  moduleServer(id, function(input, output, session) {
    observeEvent(input$go_about, {
      updateTabsetPanel(parent_session, "navbarid", selected = "About")
    })
    observeEvent(input$go_faq, {
      updateTabsetPanel(parent_session, "navbarid", selected = "FAQs")
    })
    observeEvent(input$go_citation, {
      updateTabsetPanel(parent_session, "navbarid", selected = "Citation")
    })
    observeEvent(input$go_funding, {
      updateTabsetPanel(parent_session, "navbarid", selected = "Funding")
    })
    observeEvent(input$go_contact, {
      updateTabsetPanel(parent_session, "navbarid", selected = "Contact Us")
    })
  })
}
