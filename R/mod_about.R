#' Shiny UI for ***About*** page
#'
#' @inheritParams shiny::moduleServer
#'
#' @return Output from [shiny::tagList()].
#' @keywords internal
about_ui <- function(id) {
  ns <- NS(id)

  tagList(
    page_title("About"),

    tags$div(
      markdown(
        "{propose} is a web application for running the {ringbp} R package.
        It provides an _Explore_ page to try and model under different epidemic
        scenarios, and visualise the outbreak dynamics and the probability of
        extinction."
      ),
      tags$h3(
        class = "title",
        "Development team"
      )
    ),

    # Adding some CSS to center everything and style the image
    tags$head(
      tags$style(HTML("
      .profile-card {
        text-align: center;
        margin-top: 50px;
      }
      .profile-pic {
        width: 150px;
        height: 150px;
        margin-bottom: 10px;
        border-radius: 50%; /* Makes the photo circular */
        object-fit: cover;
        border: 3px solid #f3f3f3;
      }
      .github-icon {
        width: 20px;
        margin-top: 10px;
        margin-bottom: 20px;
        transition: transform 0.2s;
      }
      .github-icon:hover {
        transform: scale(1.2); /* Subtle zoom on hover */
      }
    "))
    ),

    div(class = "profile-card",
        tags$img(src = "JWL.jpg", class = "profile-pic"),
        tags$h4("Joshua W. Lambert"),
        tags$a(href = "https://github.com/joshwlambert", target = "_blank",
               tags$img(src = "https://cdn-icons-png.flaticon.com/512/25/25231.png",
                        class = "github-icon")
        )
    ),

    tags$div(
      tags$h4("Disclaimer & Terms of Use"),
      tags$p(
        HTML("<b>Purpose of Tool</b>: This application is provided for informational
      and educational purposes only. It is intended to assist public health
      professionals in data visualization and preliminary analysis. It does
      not constitute formal medical advice, clinical diagnosis, or an
      official mandate for public health intervention.")
      ),
      tags$p(
        HTML("<b>Data Accuracy & Limitations</b>: While every effort is made to
      ensure data integrity, epidemiological data is subject to reporting
      lags, jurisdictional variances, and collection errors. Users should
      verify findings against official primary sources (e.g., CDC, WHO, or
      local MoH) before making policy or resource allocation decisions.")
      ),
      tags$p(
        HTML("<b>Professional Judgment</b>: The outputs of this model/dashboard are
      'Decision Support' only. The final interpretation of data and any
      subsequent public health actions remain the sole responsibility of
      the licensed professional using the tool.")
      ),
      tags$p(
        HTML("<b>No Warranty</b>: This software is provided 'as-is' without any
      express or implied warranties. The developers shall not be held liable
      for any inaccuracies, omissions, or outcomes resulting from the use of
      this dashboard.")
      )
    ),

    tags$div(
      tags$h4("{ringbp} Version")
    ),
    textOutput(ns("ringbp_version"))
  )
}

#' Shiny server for ***About*** page
#'
#' @inheritParams shiny::moduleServer
#'
#' @return Output from [shiny::moduleServer()].
#' @keywords internal
about_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    output$ringbp_version <- renderText(paste0("v", packageVersion("ringbp")))
  })
}
