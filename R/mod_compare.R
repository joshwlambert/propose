#' Shiny UI for ***Compare*** page
#'
#' @inheritParams shiny::moduleServer
#'
#' @return Output from [shiny::tagList()].
#' @keywords internal
compare_ui <- function(id) {
  ns <- NS(id)

  tagList(
    page_title("Compare Outbreak Scenarios"),

    actionButton(
      ns("add_scenario"),
      "Add Scenario",
      icon = icon("plus"),
      class = "btn-lg",
      style = "background-color: #000080; color: white; border: none; padding: 10px 30px;"
    ),

    uiOutput(ns("compare_ui"))
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
  })
}
