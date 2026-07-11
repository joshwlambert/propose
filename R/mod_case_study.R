#' Shiny UI for a ***Case study*** page
#'
#' Renders a case-study HTML fragment inline (via [shiny::includeHTML()]) so it
#' inherits the app's styling, rather than being sandboxed in an `<iframe>`. The
#' fragments live in `www/case_studies/` and must be body-only HTML (no
#' `<html>`, `<head>`, `<body>` or `<style>` wrappers).
#'
#' @inheritParams shiny::moduleServer
#' @param file_name A `character` string with the file name of the case-study
#' HTML fragment in `www/case_studies/`, including the `.html` file extension.
#'
#' @return Output from [shiny::tagList()].
#' @keywords internal
case_study_ui <- function(id, file_name) {
  ns <- NS(id)

  tagList(
    tags$div(
      class = "case-study",
      includeHTML(file.path("www", "case_studies", file_name))
    )
  )
}

# Case-study pages are static content and currently have no server logic.
# case_study_server <- function(id) {
#   moduleServer(id, function(input, output, session) {
#     # Insert case-study server logic here
#   })
# }
