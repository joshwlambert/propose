#' Shiny UI for ***FAQs*** page
#'
#' @inheritParams shiny::moduleServer
#'
#' @return Output from [shiny::tagList()].
#' @keywords internal
faq_ui <- function(id) {
  ns <- NS(id)

  tagList(
    page_title("FAQs"),

    # Custom CSS for the "Plus/Minus" interaction and styling
    tags$head(
      tags$style(HTML("
      .faq-container {
        max-width: 800px;
        margin: 40px auto;
      }

      .faq-item {
        border-bottom: 1px solid #eee;
        padding: 10px 0;
      }

      /* Style the summary (The Question) */
      details summary {
        list-style: none; /* Hide default arrow */
        display: flex;
        justify-content: space-between;
        align-items: center;
        padding: 15px;
        /* font params match standard Bootstrap lead */
        font-size: 1.25rem;
        font-weight: 300;
        line-height: 1.5;
        color: #212529;
        cursor: pointer;
        transition: background-color 0.3s ease;
      }

      details summary:hover {
        background-color: #f9f9f9;
      }

      /* Create the Plus icon using pseudo-elements */
      details summary::after {
        content: '+';
        font-size: 1.5em;
        font-weight: 300;
        color: #3498db;
        transition: transform 0.3s ease;
      }

      /* Rotate the Plus into a Cross (or Minus) when open */
      details[open] summary::after {
        transform: rotate(45deg);
        color: #e74c3c;
      }

      /* Style the answer text */
      .faq-content {
        padding: 15px 15px 15px 15px;
        line-height: 1.6;
        color: #555;
      }
    "))
    ),

    # FAQ UI Structure
    tags$div(
      class = "faq-container",
        tags$h2(
          "Frequently Asked Questions",
          style = "text-align: center; margin-bottom: 30px;"
        ),

        tags$div(
          class = "faq-item",
            tags$details(
              tags$summary(
                "What is the difference between Community and Isolated transmission?"
              ),
              tags$div(
                class = "faq-content",
                "Transmission is split into two states: 'Community' represents individuals moving freely,
           while 'Isolated' represents the reduced transmission after a case is detected.
           For a perfect intervention, set the Isolated R0 to 0."
              )
            )
        ),
        tags$div(
          class = "faq-item",
            tags$details(
              tags$summary(
                "Why does the outbreak grow despite high contact tracing?"
              ),
              tags$div(
                class = "faq-content",
                "This is often due to 'Onset-to-Isolation' delays. If transmission occurs before
           symptoms appear (presymptomatic transmission), cases may infect others before they are
           traced and isolated."
              )
            )
        )
    )
  )
}

# The FAQs page currently has no server logic
# faq_server <- function(id) {
#  moduleServer(id, function(input, output, session) {
#    # Insert FAQs server logic here
#  })
# }
