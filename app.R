library(shiny)
library(ringbp)
library(tinyplot)

ui <- fluidPage(
  titlePanel("{propose}: a Shiny app for {ringbp}"),

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
  "Probability of outbreak extinction: ",
  verbatimTextOutput("extinct"),
  plotOutput("cumulative_cases")
)

server <- function(input, output, session) {
  scenario <- reactive({
    scenario_sim(
      n = 1,
      initial_cases = 5,
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
  output$extinct <- renderPrint(extinct_prob(scenario()))
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
}

shinyApp(ui = ui, server = server)