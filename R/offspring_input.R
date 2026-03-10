#' Generate [bslib::accordion()] with inputs for parameterising `community` and
#' `isolated` arguments in [ringbp::offspring_opts()]
#'
#' @param ns A namespace created with [shiny::NS()].
#' @param ... [dots] Not used, will throw a warning if arguments are supplied.
#'
#' @return A [bslib::accordion()] object.
#' @keywords internal
offspring_input <- function(ns, ...) {
  chkDots(...)
  accordion(
    accordion_panel(
      title = "Offspring distribution parameters:",
      icon = bs_icon("diagram-3-fill"),
      selectInput(
        inputId = ns("community_offspring_distribution"),
        label = "Community Offspring Distribution",
        choices = list(
          "Negative Binomial" = "nbinom",
          "Poisson" = "pois",
          "Geometric" = "geom"
        )
      ),
      conditionalPanel(
        condition = "input.community_offspring_distribution == 'nbinom'",
        numericInput(ns("community_r0"), "Community R0:", value = 2),
        numericInput(ns("community_disp"), "Community Dispersion:", value = 1),
        ns = ns
      ),
      conditionalPanel(
        condition = "input.community_offspring_distribution == 'pois'",
        numericInput(ns("community_r0"), "Community R0:", value = 2),
        ns = ns
      ),
      conditionalPanel(
        condition = "input.community_offspring_distribution == 'geom'",
        numericInput(ns("community_r0"), "Community R0:", value = 2),
        ns = ns
      ),
      selectInput(
        inputId = ns("isolated_offspring_distribution"),
        label = "Isolated Offspring Distribution",
        choices = list(
          "Negative Binomial" = "nbinom",
          "Poisson" = "pois",
          "Geometric" = "geom"
        )
      ),
      conditionalPanel(
        condition = "input.isolated_offspring_distribution == 'nbinom'",
        numericInput(ns("isolated_r0"), "Isolated R0:", value = 0),
        numericInput(ns("isolated_disp"), "Isolated Dispersion:", value = 1),
        ns = ns
      ),
      conditionalPanel(
        condition = "input.isolated_offspring_distribution == 'pois'",
        numericInput(ns("isolated_r0"), "Isolated R0:", value = 0),
        ns = ns
      ),
      conditionalPanel(
        condition = "input.isolated_offspring_distribution == 'geom'",
        numericInput(ns("isolated_r0"), "Isolated R0:", value = 0),
        ns = ns
      )
    ),
    open = FALSE
  )
}
