contact_ui <- function(id) {
  ns <- NS(id)

  tagList(
    page_title("Contact Us"),

    card(
      card_header(
        class = "bg-light",
        bs_icon("info-square"),
        "Directions"
      ),
      card_body(
        markdown(
          "* If you have general question about `{propose}` please email (<font color='orange'>left</font>).
          * If you have a problem to report about `{propose}` please raise an
          issue on GitHub (<font color='green'>centre</font>).
          * If you would like to contribute to the project see instructions
          (<font color='blue'>right</font>)"
        )
      )
    ),

    layout_column_wrap(
      width = "200px", height = 300,
      card(
        card_header(
          class = "bg-warning",
          bs_icon("envelope"),
          "Email us"
        ),
        card_body(
          markdown(
            "To contact the lead developer of the `{propose}` app please email:
        joshua.lambert@lshtm.ac.uk"
          )
        )
      ),
      card(
        card_header(
          class = "bg-success",
          bs_icon("github"),
          "Raise an issue on GitHub"
        ),
        card_body(
          markdown(
            "If you have an issue with {propose} please file an issue on the
        GitHub repository at the link below:

        [`{propose}` Issue tracker](https://github.com/joshwlambert/propose/issues/new).

        You will need a GitHub account to do this."
          )
        )
      ),
      card(
        card_header(
          class = "bg-primary",
          bs_icon("file-plus"),
          "Contribute to the project"
        ),
        card_body(
          markdown(
            "If you have any questions about how to contribute to the project
        please see our [Contributing guidelines](https://github.com/joshwlambert/propose/blob/main/.github/CONTRIBUTING.md).

        `{propose}` is an open-source project, and contributions to the project
        will be fairly recognised.

        `{propose}` has an MIT license, therefore, you may copy and modify the
        app for your own purposes, however, we encourage you to contribute your
        changes to this project as they may benefit other users."
          )
        )
      )
    )
  )
}

# The contact page currently has no server logic
# contact_server <- function(id) {
#   moduleServer(id, function(input, output, session) {
#     # Insert contact server logic here
#   })
# }

