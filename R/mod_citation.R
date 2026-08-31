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
        "When using the ***propose*** web app please cite the work using:"
      )
    ),
    verbatimTextOutput(ns("propose_citation")),
    tags$div(
      markdown(
        "If you are additionally using the `{ringbp}` R package or would also
        like to cite the package with the epidemiological model powering
        ***propose***, please use:"
      )
    ),
    verbatimTextOutput(ns("ringbp_citation")),
    tags$div(
      tags$h3("Papers using ", ringbp_name())
    ),
    verbatimTextOutput(ns("paper_citations"))
  )
}

#' Build the ***propose*** citation from `DESCRIPTION`
#'
#' @description
#' ***propose*** is never installed as a package alongside the app, so
#' `citation(package = "propose")` has nothing to find. Passing the app's own
#' `DESCRIPTION` to [utils::citation()] through `auto` produces exactly the
#' auto-generated citation R would build for an installed package -- without
#' needing one, and without a second copy of the title, authors or URL to drift
#' out of step.
#'
#' `citation(package = "ringbp")` in [citation_server()] is left as it is:
#' `{ringbp}` *is* an installed package in every deployment.
#'
#' @return A [utils::bibentry()] object.
#' @keywords internal
propose_citation <- function() {
  meta <- as.list(read.dcf("DESCRIPTION")[1, ])
  # A source DESCRIPTION has none of the fields utils::citation() reads a year
  # from: `Date/Publication`, `Date` and `Packaged` are all added when a package
  # is built or published. Without one the citation year renders as "(????)".
  meta$Date <- format(Sys.Date())
  utils::citation(auto = meta)
}

#' Shiny server for ***Citation*** page
#'
#' @inheritParams shiny::moduleServer
#'
#' @return Output from [shiny::moduleServer()].
#' @keywords internal
citation_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    output$propose_citation <- renderPrint(propose_citation())
    output$ringbp_citation <- renderPrint(citation(package = "ringbp"))
    output$paper_citations <- renderPrint(bibtex::read.bib(file.path("www", "references.bib"))["Hellewell2020"])
  })
}
