#' Shiny UI for ***Docs*** page
#'
#' @inheritParams shiny::moduleServer
#' @param vignette_file_name A `character` string with the file name of the
#' \pkg{ringbp} vignette, including the `.html` file extension.
#'
#' @return Output from [shiny::tagList()].
#' @keywords internal
docs_ui <- function(id, vignette_file_name) {
  ns <- NS(id)

  ringbp_vig_src <- paste0("ring_docs/", vignette_file_name)

  tagList(
    page_title("Documentation"),

    tags$div(
      class = "alert alert-info d-flex align-items-center",
      role = "alert",
      bs_icon("info-circle", class = "me-2 fs-1"),
      tags$div(
        tags$b("Notice:"),
        "This documentation (vignette) is from the {ringbp} package.
        It is not a manual for {propose} but may help you better understand
        the epidemiological model used by {propose}."
      )
    ),

    tags$iframe(
      src = ringbp_vig_src,
      width = "100%",
      height = "800px",
      style = "border:none;"
    )
  )
}

# The Documentaiton page currently has no server logic
# `addResourcePath()` is stated in the app server
# docs_server <- function(id) {
#   moduleServer(id, function(input, output, session) {
#     # Insert docs server logic here
#   })
# }
