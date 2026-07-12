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
                div(
                  class = "alert alert-info d-flex align-items-center",
                  role = "alert",
                  bs_icon("info-circle-fill"),
                  tags$span(
                    class = "ms-2",
                    propose_name(), " is currently in development. New features
                    and improvements are being implemented continuously, so some
                    pages and options may change between releases."
                  )
                ),
                p(propose_name(), " is a public health decision support tool for
                  early infectious disease outbreak response. It lets you explore
                  how the characteristics of a pathogen and the public health
                  response together determine whether an emerging outbreak can be
                  brought under control."),
                p("The rest of this manual walks through each page in turn."),
                tags$hr(style = "margin-top: 2.5rem;"),
                h3("The epidemic model"),
                p("Under the hood, ", propose_name(), " runs the ", ringbp_name(),
                  " branching process model, which simulates outbreaks as chains
                  of transmission between individuals. Each infectious individual
                  is either in the ", HTML("<em>community</em>,"), " where they
                  transmit freely, or ", HTML("<em>isolated</em>,"), " where
                  onward transmission
                  is reduced. Interventions such as case isolation, contact tracing
                  and quarantine act to move individuals into isolation, and
                  thereby interrupt transmission."),
                tags$figure(
                  style = "text-align: center; margin: 1.5rem 0;",
                  div(
                    style = "display: flex; align-items: center;
                      justify-content: center; gap: 1.5rem; flex-wrap: wrap;",
                    img(
                      src = "hex_logo.svg",
                      alt = "propose hex logo",
                      style = "width: 130px; height: auto;"
                    ),
                    bs_icon("link", style = "font-size: 3.5rem; color: #6c757d;"),
                    img(
                      src = "ringbp_hex_logo.svg",
                      alt = "ringbp hex logo",
                      style = "width: 130px; height: auto;"
                    )
                  ),
                  tags$figcaption(
                    "Powered by the ", ringbp_name(), " epidemic model,
                    implemented as an ",
                    tags$a(
                      href = "https://epiforecasts.io/ringbp/",
                      target = "_blank",
                      rel = "noopener noreferrer",
                      "R package"
                    ), ".",
                    style = "font-size: 0.875rem; color: #6c757d; margin-top: 0.5rem;"
                  )
                ),
                p("The parameterisation of the epidemic model in ", propose_name(),
                  " is deliberately simple. Alongside the pathogen's
                  transmissibility and the delays in the response, you can control
                  the proportion of cases that are asymptomatic and how much they
                  transmit, and the proportion of transmission that happens before
                  symptom onset (presymptomatic transmission). These matter
                  because asymptomatic and presymptomatic transmission can make an
                  outbreak difficult to control with targeted interventions such
                  as case isolation and contact tracing, which rely on identifying
                  cases through their symptoms."),
                tags$hr(style = "margin-top: 2.5rem;"),
                h3("What you can do with ", propose_name()),
                p("There are currently two main features:"),
                tags$ul(
                  tags$li(tags$b("Explore"), " a single outbreak scenario in
                          detail — setting pathogen and intervention parameters,
                          simulating outbreaks and visualising the results (the ",
                          tags$em("Explore"), " page)."),
                  tags$li(tags$b("Analyse"), " how outbreak outcomes change across
                          a range of conditions — the effectiveness of contact
                          tracing, different contact tracing strategies, and the
                          distribution of outbreak size and length (the ",
                          tags$em("Analyses"), " menu).")
                ),
                tags$hr(style = "margin-top: 2.5rem;"),
                h3("Open source and collaborative"),
                p("Both ", propose_name(), " and ", ringbp_name(), " are
                  open-source projects, developed collaboratively with academics
                  and public health professionals. Being open-source means the
                  underlying code and epidemic model are freely available to
                  inspect, use and build on, which makes the results transparent
                  and reproducible and allows the methods to be independently
                  scrutinised. Developing the projects collaboratively keeps them
                  grounded in the real needs of outbreak response: input from
                  public health professionals helps ensure the tools answer
                  practically useful questions, while academic collaboration keeps
                  the science rigorous and up to date. Together this builds trust
                  in the tools and lets the wider community contribute
                  improvements. Both projects are hosted on GitHub, where you can
                  browse the source code, report issues and contribute.")
      ),

      "Getting Started",
      nav_panel("Quick Start",
                h2("Quick Start"),
                p("When you first open ", propose_name(), " you land on the ",
                  tags$em("Home"), " page. From here you can jump straight into
                  the ", tags$em("Explore"), " page by clicking the ",
                  tags$b("'Start Exploring'"), " button, or use the navigation
                  bar at the top of the page to move around the app."),

                h3("Finding your way around", style = "margin-top: 2.5rem;"),
                p("The navigation bar at the top of the page groups the app into
                  a few areas:"),
                tags$ul(
                  tags$li(tags$b("Home"), " — the landing page, with the
                          'Start Exploring' button and shortcut tiles into the
                          rest of the app."),
                  tags$li(tags$b("Explore"), " — the main page for setting up,
                          simulating and visualising a single outbreak scenario."),
                  tags$li(tags$b("Analyses"), " — a drop-down menu of pre-packaged
                          analyses that each tackle a specific outbreak-response
                          question: ",
                          HTML("<em>Tracing Effectiveness</em>, <em>Tracing
                          Strategies</em>, and <em>Outbreak Size &amp;
                          Length</em>.")),
                  tags$li(tags$b("Docs"), " — a drop-down menu containing this
                          manual, the ", ringbp_name(), " documentation, and
                          real-world COVID-19 case studies."),
                  tags$li(tags$b("About, FAQs, Citation, Funding"), " and ",
                          tags$b("Contact Us"), " — background on the project,
                          answers to common questions, how to cite the tool,
                          details of the funders, and how to get in touch.")
                ),
                p("If the navigation bar items are not shown (for example on a
                  narrow screen), click the navbar toggler (☰) to reveal them."),

                h3("Shortcuts from the Home page", style = "margin-top: 2.5rem;"),
                p("The ", tags$em("Home"), " page also has shortcuts into the rest
                  of the app: tiles that launch each of the pre-packaged analyses,
                  links to the COVID-19 case studies, and a banner linking to this
                  manual. These lead to the same pages as the navigation-bar
                  menus."),

                h3("Where to go next", style = "margin-top: 2.5rem;"),
                p("If you are new to ", propose_name(), ", a good place to start
                  is the ", tags$em("Explore"), " page, described in the next
                  section of this manual. Once you are comfortable setting up and
                  simulating outbreaks there, the pre-packaged ",
                  tags$em("Analyses"), " are a natural next step.")
      ),
      nav_panel("Explore outbreak scenarios",
                h2("Using the Explore page"),
                h5("Understand how to explore outbreak scenarios in ", propose_name()),
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
                p("A good way to start using the Explore page is to choose a
                  preset from the ", tags$em("Select Pathogen Parameters"),
                  " menu. This populates the pathogen parameters with values
                  representative of the chosen pathogen, drawn from estimates
                  published in the literature. ", propose_name(), " currently
                  includes presets for:"),
                tags$ul(
                  tags$li(tags$b("Disease X"), " — a generic pathogen used as the
                          default starting point."),
                  tags$li(tags$b("COVID-19"), " — Wild-type, Alpha, Delta and
                          Omicron variants."),
                  tags$li(HTML("<b>SARS</b>.")),
                  tags$li(HTML("<b>MERS</b>.")),
                  tags$li(tags$b("Ebola"), " — Zaire and Sudan species."),
                  tags$li(HTML("<b>Marburg</b>.")),
                  tags$li(tags$b("Influenza"), " — H5N1, H1N1pdm and H7N9
                          subtypes."),
                  tags$li(HTML("<b>Meningitis B</b>.")),
                  tags$li(HTML("<b>Andes (Hanta)virus</b>."))
                ),
                p("By default, the chosen pathogen is ",
                  HTML("<em>Disease X</em>,"), " whose parameters are sensible
                  initial values that act as a starting point from which to
                  modify and simulate outbreaks."),
                p("The pathogen and intervention parameters are grouped. By
                  clicking on, for example, 'Pathogen transmissibility'
                  it will expand and allow you to change those parameters. All
                  groups of parameters expand once clicked, and will minimise if
                  clicked again."),
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
                  R0 values, percentages, and simulation caps) are checked as
                  you type. If you enter a value that isn't allowed (e.g. a
                  negative R0, or a percentage outside [0, 100]) the field is
                  highlighted and a short error message appears beneath it. Fix
                  the value before clicking 'Simulate outbreak'."),

                h4("Basic and Advanced modes", style = "margin-top: 1.5rem;"),
                p("Some parameter panels — pathogen transmissibility and the delay
                  distributions (incubation period and onset-to-isolation) — offer
                  a ", tags$em("User mode"), " toggle with ", tags$b("Basic"),
                  " and ", tags$b("Advanced"), " options."),
                tags$ul(
                  tags$li(tags$b("Basic"), " mode asks for a small number of
                          intuitive quantities. For transmissibility you set an
                          average number of secondary infections (R0), an R0 for
                          isolated cases, and a transmission-variability level
                          (homogeneous, moderate or high) that controls how much
                          superspreading occurs. For a delay you set an average
                          number of days and a variability level (low, moderate or
                          high)."),
                  tags$li(tags$b("Advanced"), " mode exposes the full underlying
                          distributions. For transmissibility you choose an
                          offspring distribution (Negative Binomial, Poisson or
                          Geometric) and its parameters separately for community
                          and isolated cases, and can optionally give asymptomatic
                          cases a different transmissibility. For a delay you choose
                          a distribution family (Lognormal, Gamma or Weibull) and
                          set its parameters.")
                ),
                p("Basic mode is a good starting point; switch to Advanced when you
                  need finer control over the shape of a distribution."),

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
                  tags$li(tags$b("Pathogen transmissibility."),
                          " Controls how many secondary cases each infectious
                          individual produces, set separately for cases in the ",
                          tags$em("community"), " and cases that are ",
                          HTML("<em>isolated</em>."), " In Basic mode you set an R0
                          and a transmission-variability level; in Advanced mode
                          you choose an offspring distribution (Negative Binomial,
                          Poisson or Geometric) and, for the Negative Binomial, a
                          dispersion parameter ", HTML("(<em>k</em>)"), " where
                          smaller values mean more superspreading."),
                  tags$li(tags$b("Incubation period."),
                          " The delay between infection and symptom onset. In Basic
                          mode you set an average and a variability level; in
                          Advanced mode you choose a distribution (Lognormal, Gamma
                          or Weibull) and set its parameters."),
                  tags$li(tags$b("Symptom event probabilities."),
                          " The percentage of cases that are asymptomatic, and the
                          percentage of transmission that happens before symptom
                          onset (presymptomatic transmission). Both are entered as
                          percentages between 0 and 100.")
                ),

                h4("Intervention Parameters", style = "margin-top: 1.5rem;"),
                p("These describe how the public health response tries to control
                  transmission. They sit behind an ", tags$em("Isolate cases"),
                  " switch: when it is off no cases are isolated and no
                  intervention is active; turn it on to reveal the controls
                  below."),
                tags$ul(
                  tags$li(tags$b("Onset-to-isolation delay."),
                          " The delay between a person developing symptoms and
                          being isolated. As with the incubation period, Basic mode
                          takes an average and a variability level, while Advanced
                          mode takes a distribution family (Lognormal, Gamma or
                          Weibull) and its parameters. Shorter delays mean faster
                          isolation and typically better control."),
                  tags$li(tags$b("Contact tracing."),
                          " The percentage of a symptomatic case's contacts that
                          are successfully traced (0–100). Higher values represent
                          more effective contact tracing systems."),
                  tags$li(tags$b("Quarantine."),
                          " Tick this box to quarantine traced contacts, preventing
                          onward transmission before symptom onset. Leave it
                          unticked to model isolation of symptomatic cases only."),
                  tags$li(tags$b("Test sensitivity."),
                          " The proportion of symptomatic cases that test positive
                          and therefore isolate (0–1). At the default of 1 every
                          symptomatic case that is tested isolates; lower values
                          mean some cases return a false negative and are not
                          isolated through testing."),
                  tags$li(tags$b("NPI activation day."),
                          " The day of the outbreak on which interventions become
                          active. Before this day no cases are isolated through
                          testing and no contacts are traced, representing the
                          delay before a response can be mounted. A value of 0
                          means interventions are active immediately.")
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
                h3("Preview parameter distributions", style = "margin-top: 2.5rem;"),
                p("Above the results, the ", tags$em("Show simulation parameter
                  distributions"), " panel lets you preview the distributions
                  implied by your current parameter choices before you simulate.
                  It has tabs for the offspring distribution, the incubation
                  period, the onset-to-isolation delay and presymptomatic
                  transmission, so you can check that each looks sensible."),
                p("These distribution previews update in real time as you change
                  the parameters, so you do not need to click 'Simulate outbreak'
                  to refresh them. The 'Simulate outbreak' button is only needed to
                  update the outbreak plots and metrics."),
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
                p("Currently ", propose_name(), " visualises two aspects of the outbreaks:"),
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
                  transmission until the end of the simulation. A 95% confidence interval
                  is shown beneath the value, reflecting the uncertainty in this estimate
                  from the finite number of replicates — running more replicates gives a
                  narrower interval. By default the simulation
                  is set to run for 100 days, but this can be changed in the 'Simulation
                  Control parameters'.")
      ),

      "Analyses",
      nav_panel("Tracing Effectiveness",
                h2("Tracing Effectiveness"),
                h5("How effective is contact tracing at controlling an outbreak?"),
                p("The ", tags$em("Tracing Effectiveness"), " page asks a single
                  question: how does the chance of controlling an outbreak change
                  as contact tracing improves? Rather than simulating one scenario
                  like the ", tags$em("Explore"), " page, it sweeps across a range
                  of contact tracing levels, simulates many outbreaks at each
                  level, and plots how the outcomes change."),

                h3("Where this analysis comes from", style = "margin-top: 2.5rem;"),
                p("This analysis reproduces the approach of the study ",
                  tags$a(
                    href = "https://www.thelancet.com/article/S2214-109X(20)30074-7/fulltext",
                    target = "_blank",
                    rel = "noopener noreferrer",
                    "Hellewell et al. (2020)"
                  ),
                  " that the ", ringbp_name(), " model was originally built to
                  implement. In their 2020 study in ",
                  tags$em("The Lancet Global Health"), " — ",
                  tags$em("Feasibility of controlling COVID-19 outbreaks by
                  isolation of cases and contacts"), " — the authors used the same
                  stochastic branching process model to ask whether isolating cases
                  and tracing their contacts could control emerging COVID-19
                  outbreaks."),
                p("That study swept the percentage of contacts traced from 0% to
                  100% across a range of scenarios — varying the reproduction
                  number, the delay from symptom onset to isolation, the amount of
                  transmission occurring before symptom onset, and the proportion
                  of subclinical (asymptomatic) cases — and measured the
                  probability that an outbreak was brought under control. An
                  outbreak was counted as controlled if transmission ended within
                  12 weeks or before reaching a large cumulative case count, which
                  corresponds to the time and case caps used here."),
                p("The headline finding was that feasibility depends strongly on
                  how much transmission happens before symptoms appear: for a
                  reproduction number around 2.5, roughly 80% of contacts needed to
                  be traced to give a high chance of control, and higher
                  transmissibility or more presymptomatic transmission made control
                  substantially harder. These results helped inform the role of
                  contact tracing in the COVID-19 pandemic response. The plots on
                  this page let you explore the same relationships for the
                  parameters you choose."),

                h3("Setting up the sweep", style = "margin-top: 2.5rem;"),
                p("The sidebar uses the same pathogen, intervention and simulation
                  control parameters as the ", tags$em("Explore"), " page, with one
                  key difference: instead of a single contact tracing value you
                  specify a ", tags$em("range"), " of values to sweep over."),
                tags$ul(
                  tags$li(tags$b("Contact tracing sweep."),
                          HTML(" The <em>From</em>, <em>To</em> and <em>By</em>
                          inputs define the range of contact tracing coverage to
                          test, as percentages of contacts traced (0–100). For
                          example, from 0 to 100 in steps of 20 simulates outbreaks
                          at 0%, 20%, 40%, 60%, 80% and 100% tracing.")),
                  tags$li(tags$b("Replicates and initial cases."),
                          " As on the ", tags$em("Explore"), " page, these set how
                          many independent outbreaks are simulated at ",
                          tags$em("each"), " tracing level, and how many infectious
                          individuals seed each outbreak.")
                ),
                p("All the other pathogen and intervention parameters — pathogen
                  transmissibility, incubation period, symptom event probabilities,
                  onset-to-isolation delay, quarantine, test sensitivity and NPI
                  activation day — work as they do on the ",
                  tags$em("Explore"), " page, and are held fixed across the sweep.
                  The single contact tracing input is the only one replaced by the
                  sweep. Note that, unlike the ", tags$em("Explore"), " page, there
                  is no ", tags$em("Isolate cases"), " switch here: case isolation
                  and the interventions built on it are always active, because the
                  purpose of this page is to measure how their effectiveness changes
                  as contact tracing improves."),
                p("As on the ", tags$em("Explore"), " page, the ",
                  tags$em("Show simulation parameter distributions"), " panel lets
                  you preview the offspring, incubation, onset-to-isolation and
                  presymptomatic distributions before you run the sweep."),

                h3("Running the sweep", style = "margin-top: 2.5rem;"),
                p("Click ", tags$b("'Simulate outbreak(s)'"), " to run the sweep. A
                  loading screen appears while it runs. Because a full set of
                  replicates is simulated at every tracing level, the sweep takes
                  longer than a single ", tags$em("Explore"), " run — increasing
                  the number of replicates or the number of tracing levels will
                  increase the run time. Click ", tags$b("'Reset Defaults'"), " to
                  restore all parameters to their default values."),

                h3("Reading the results", style = "margin-top: 2.5rem;"),
                p("The sweep produces three plots, each with contact tracing
                  coverage (%) on the horizontal axis. Outbreak control is the
                  primary feasibility metric from the Hellewell et al. study, while
                  the effective reproduction number and maximum weekly cases add
                  supporting context on how much transmission is still occurring
                  and how large the controlled outbreaks become:"),
                tags$ul(
                  tags$li(tags$b("Outbreak control."),
                          " The percentage of simulated outbreaks that were brought
                          under control (became extinct before reaching the case or
                          time caps) at each tracing level. A higher percentage,
                          rising with tracing, means tracing is helping to control
                          the outbreak."),
                  tags$li(tags$b("Effective reproduction number."),
                          " The median effective reproduction number across
                          replicates at each tracing level. The shaded bands show
                          the interquartile range (darker) and 95% interval
                          (lighter) across replicates, and a dashed line marks ",
                          HTML("<em>R</em> = 1 (the threshold below which an
                          outbreak is expected to decline).")),
                  tags$li(tags$b("Maximum weekly cases in controlled outbreaks."),
                          " A box plot of the largest number of cases seen in any
                          single week, across only the outbreaks that were
                          controlled, at each tracing level. Uncontrolled outbreaks
                          are excluded because they simply grow until they reach the
                          case cap, so their weekly peak reflects the cap rather
                          than being informative. Each box is shaded in
                          proportion to the percentage of outbreaks controlled at
                          that level (also labelled above the box), so you can see
                          both the size of the controlled outbreaks and how often
                          control was achieved.")
                ),

                h3("References", style = "margin-top: 2.5rem;"),
                HTML(
                  format(
                    bibtex::read.bib(
                      file.path("www", "references.bib")
                    )["Hellewell2020"],
                    style = "html"
                  )
                )
      ),

      "Help & Support",
      nav_panel("Info",
                h2("Information about ", propose_name()),
                p("See ", actionLink(ns("go_about"), "the About page"), " for general information on ", propose_name(".")),
                h2("Frequently Asked Questions", style = "margin-top: 2.5rem;"),
                p("See ", actionLink(ns("go_faq"), "the FAQ page"), " for answers to common questions."),
                h2("Citing ", propose_name(), style = "margin-top: 2.5rem;"),
                p(
                  "To cite ", propose_name(","), " or the underlying epidemiological model, ", ringbp_name(","), " or see
                  a list of papers that use the ", ringbp_name(), " model, see",
                  actionLink(ns("go_citation"), "the Citation page.")
                ),
                h2("Funding", style = "margin-top: 2.5rem;"),
                p("See ", actionLink(ns("go_funding"), "the funding page"), " for information on the project funders."),
                h2("Report an issue or contribute to the project", style = "margin-top: 2.5rem;"),
                p("If you experience an issue that you'd like to report, or
                  would like to contribute to the development of ", propose_name(","), "
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
