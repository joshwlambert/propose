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

    # extract namespace from session in dynamic UI
    ns <- session$ns

    # track scenarios being compared
    scenarios <- reactiveValues(s1 = NULL, s2 = NULL, count = 0)

    # We use eventReactive so the UI only generates when the button is clicked
    output$compare_ui <- renderUI({

      # Check if the button has been clicked at least once
      if (input$add_scenario == 0) return(NULL)

      # Wrap multiple inputs in a tagList
      tagList(
        offspring_input(ns = ns),
        delays_input(ns = ns),
        event_prob_input(ns = ns),
        intervention_input(ns = ns),
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
