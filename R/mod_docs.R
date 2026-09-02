#' Generate a card linking to one \pkg{ringbp} vignette
#'
#' @param title A `character` string naming the vignette.
#' @param description A `character` string summarising what the vignette covers.
#' @param href A `character` string with the URL of the vignette on the
#' \pkg{ringbp} package website.
#'
#' @return A [bslib::card()] object.
#' @keywords internal
docs_link_card <- function(title, description, href) {
  card(
    card_header(tags$b(title)),
    card_body(
      tags$p(description, class = "text-muted"),
      tags$a(
        href = href,
        target = "_blank",
        rel = "noopener noreferrer",
        class = "btn btn-primary",
        "Read on the ", ringbp_name(), " website ",
        bs_icon("box-arrow-up-right")
      )
    )
  )
}

#' Shiny UI for ***Docs*** page
#'
#' @details
#' Links to the \pkg{ringbp} vignettes. Links open in a new tab, so that a
#' simulation in progress is not lost.
#'
#' \pkg{ringbp} vignettes used to be embedded in ***propose*** using `iframe`s,
#' however there were issues with rendering these across the multiple
#' ***propose*** deployments.
#'
#' @inheritParams shiny::moduleServer
#'
#' @return Output from [shiny::tagList()].
#' @keywords internal
docs_ui <- function(id) {
  ns <- NS(id)

  tagList(
    page_title("Documentation"),

    tags$div(
      class = "alert alert-info d-flex align-items-center",
      role = "alert",
      bs_icon("info-circle", class = "me-2 fs-1"),
      tags$div(
        tags$b("Notice:"),
        "These are the vignettes for the ", ringbp_name(), " package, which
        provides the epidemiological model behind ", propose_name(), ". The
        vignettes describe the model, and may help you better understand the
        simulation model used by ", propose_name(), ", and how to use the ",
        ringbp_name(), " package from R. The articles linked are not a
        manual for ", propose_name(), ". For guidance on using this app, see
        the ", tags$b(tagList(propose_name(), " manual")), " in this menu."
      )
    ),

    layout_columns(
      col_widths = c(4, 4, 4),
      docs_link_card(
        title = tagList("Getting Started with ", ringbp_name()),
        description = "An introduction to running outbreak simulations with the
          package, covering the offspring, delay, intervention and simulation
          options that the app's sidebar controls set.",
        href = "https://epiforecasts.io/ringbp/articles/ringbp.html"
      ),
      docs_link_card(
        title = tagList(ringbp_name(), " Model Description"),
        description = "A description of the branching process model itself: how
          cases generate secondary cases, how contact tracing and isolation act
          on transmission, and the assumptions behind them.",
        href = "https://epiforecasts.io/ringbp/articles/ringbp-model.html"
      ),
      docs_link_card(
        title = tagList("Parameter Sweep with ", ringbp_name()),
        description = "How to run a scenario across a range of parameter values,
          the approach used by the app's Tracing Effectiveness page.",
        href = "https://epiforecasts.io/ringbp/articles/parameter-sweep.html"
      )
    ),

    tags$div(
      class = "mt-4",
      tags$p(
        "The full ", ringbp_name(), " documentation, including the reference for
        every function, is available at ",
        tags$a(
          href = "https://epiforecasts.io/ringbp/",
          target = "_blank",
          rel = "noopener noreferrer",
          "epiforecasts.io/ringbp",
          bs_icon("box-arrow-up-right")
        ),
        "."
      )
    )
  )
}

# The Documentation page currently has no server logic
# docs_server <- function(id) {
#   moduleServer(id, function(input, output, session) {
#     # Insert docs server logic here
#   })
# }
