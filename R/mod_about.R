#' Shiny UI for ***About*** page
#'
#' @inheritParams shiny::moduleServer
#'
#' @return Output from [shiny::tagList()].
#' @keywords internal
about_ui <- function(id) {
  ns <- NS(id)

  # `orcid` is a full ORCID URL (e.g. "https://orcid.org/0000-0002-1825-0097")
  profile_card <- function(img, name, role, affiliation, orcid) {
    div(
      class = "profile-card",
      tags$img(src = img, class = "profile-pic", alt = name),
      div(
        class = "d-flex align-items-center justify-content-center gap-2",
        tags$h4(name, class = "mb-0"),
        # orcid displayed according to guidelines:
        # https://info.orcid.org/documentation/integration-guide/orcid-id-display-guidelines/
        tags$a(
          href = orcid, target = "_blank",
          HTML(
            '<svg class="orcid-icon" xmlns="http://www.w3.org/2000/svg" viewBox="0 0 256 256" role="img" aria-label="ORCID iD"><path fill="#A6CE39" d="M256 128c0 70.7-57.3 128-128 128S0 198.7 0 128 57.3 0 128 0s128 57.3 128 128z"/><path fill="#FFF" d="M86.3 186.2H70.9V79.1h15.4v107.1z"/><path fill="#FFF" d="M108.9 79.1h41.6c39.6 0 57 28.3 57 53.6 0 27.5-21.5 53.6-56.8 53.6h-41.8V79.1zm15.4 93.3h24.5c34.9 0 42.9-26.5 42.9-39.7 0-21.5-13.7-39.7-43.7-39.7h-23.7v79.4z"/><path fill="#FFF" d="M88.7 56.8c0 5.5-4.5 10.1-10.1 10.1s-10.1-4.6-10.1-10.1c0-5.6 4.5-10.1 10.1-10.1 5.5 0 10.1 4.6 10.1 10.1z"/></svg>'
          )
        )
      ),
      tags$p(role, class = "text-muted"),
      tags$p(affiliation, class = "text-muted")
    )
  }

  tagList(
    page_title("About"),

    tags$div(
      markdown(
        "***propose*** is a web application for running the `{ringbp}` R package.
        It provides an _Explore_ page to try and model under different epidemic
        scenarios, and visualise the outbreak dynamics and the probability of
        extinction."
      ),
      tags$hr(),
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
      .orcid-icon {
        width: 20px;
        transition: transform 0.2s;
      }
      .orcid-icon:hover {
        transform: scale(1.2); /* Subtle zoom on hover */
      }
    "))
    ),

    div(
      class = "d-flex justify-content-center flex-wrap gap-5",
      profile_card(
        img = "JE.jpg",
        name = "John Edmunds",
        role = "Professor & Chief Scientific Adviser (Project lead)",
        affiliation = "LSHTM & FCDO",
        orcid = "https://orcid.org/0000-0002-9179-2917"
      ),
      profile_card(
        img = "JWL.jpg",
        name = "Joshua W. Lambert",
        role = "Research Fellow (Lead developer)",
        affiliation = "LSHTM",
        orcid = "https://orcid.org/0000-0001-5218-3046"
      ),
      profile_card(
        img = "JZ.jpg",
        name = "Jiaxin Zhou",
        role = "Research Fellow (Developer)",
        affiliation = "LSHTM",
        orcid = "https://orcid.org/0000-0002-6615-0752"
      ),
      profile_card(
        img = "JW.jpg",
        name = "Jack Ward",
        role = "PhD student (Developer)",
        affiliation = "University of Warwick",
        orcid = "https://orcid.org/0009-0006-3757-3306"
      )
    ),
    tags$hr(),
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
    tags$hr(),
    tags$div(
      tags$h4(propose_name(), " Version")
    ),
    textOutput(ns("propose_version")),
    tags$br(),
    tags$div(
      tags$h4(ringbp_name(), " Version")
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
    output$propose_version <- renderText(paste0("v", packageVersion("propose")))
    output$ringbp_version <- renderText(paste0("v", packageVersion("ringbp")))
  })
}
