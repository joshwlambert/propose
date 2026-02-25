#' Shiny UI for ***Citation*** page
#'
#' @inheritParams shiny::moduleServer
#'
#' @return Output from [shiny::tagList()].
#' @keywords internal
citation_ui <- function(id) {
  ns <- NS(id)

  tagList(
    page_title("Citation"),

    tags$div(
      markdown(
        "When using the `{propose}` Shiny app please cite the work using:"
      )
    ),
    verbatimTextOutput(ns("propose_citation")),
    tags$div(
      markdown(
        "If you are additionally using the `{ringbp}` R package or would also
        like to cite the package with the epidemiological model powering
        `{propose}`, please use:"
      )
    ),
    verbatimTextOutput(ns("ringbp_citation")),
    tags$div(
      tags$h3("Papers using {ringbp}")
    ),
    verbatimTextOutput(ns("paper_citations"))
  )
}

#' Shiny server for ***Citation*** page
#'
#' @inheritParams shiny::moduleServer
#'
#' @return Output from [shiny::moduleServer()].
#' @keywords internal
citation_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    output$propose_citation <- renderPrint(citation(package = "propose"))
    output$ringbp_citation <- renderPrint(citation(package = "ringbp"))
    output$paper_citations <- renderPrint(bibtex::read.bib(file.path("www", "references.bib")))
  })
}
