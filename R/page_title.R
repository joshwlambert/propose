#' Create title for \pkg{propose} page (i.e. [bslib::nav_panel()])
#'
#' @param title A `character` string of length 1.
#' @param ... Extra arguments, not currently used (will warn if used).
#'
#' @return A [shiny::tags()] `div`.
#' @keywords internal
page_title <- function(title, ...) {

  stopifnot(
    "The page title needs to be a single character string" =
      is.character(title) && length(title) == 1
  )
  chkDots(...)

  tags$div(
    style = "
      display: flex;
      justify-content: space-between;
      align-items: center;
      margin-bottom: 20px;
    ",

    # Title (styled like titlePanel)
    tags$h2(
      class = "title",
      title,
      style = "margin: 0;"
    ),

    # Logo
    tags$img(
      src = "logo.svg",
      style = "width: 110px;"
    )
  )
}
