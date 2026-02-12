library(shiny)
library(ringbp)

ui <- fluidPage(
  titlePanel("{propose}: a Shiny app for {ringbp}"),
  "Probability of outbreak extinction: ",
  verbatimTextOutput("extinct")
)

server <- function(input, output, session) {
  scenario <- scenario_sim(
    n = 5,
    initial_cases = 5,
    offspring = offspring_opts(
      community = \(n) rnbinom(n = n, mu = 2, size = 1),
      isolated = \(n) rnbinom(n = n, mu = 0, size = 1)
    ),
    delays = delay_opts(
      incubation_period = \(n) rlnorm(n = n, meanlog = 1.5, sdlog = 0.4),
      onset_to_isolation = \(n) rlnorm(n = n, meanlog = 2, sdlog = 0.5)
    ),
    event_probs = event_prob_opts(
      asymptomatic = 0.1,
      presymptomatic_transmission = 0.1,
      symptomatic_ascertained = 0.8
    ),
    interventions = intervention_opts(quarantine = FALSE),
    sim = sim_opts(cap_max_days = 100, cap_cases = 5000)
  )
  output$extinct <- renderPrint(extinct_prob(scenario))
}

shinyApp(ui = ui, server = server)