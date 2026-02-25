#' Shiny UI for ***Funding*** page
#'
#' @inheritParams shiny::moduleServer
#'
#' @return Output from [shiny::tagList()].
#' @keywords internal
funding_ui <- function(id) {
  ns <- NS(id)

  tagList(
    page_title("Funding"),

    tags$div(
      style = "padding-top: 30px; padding-bottom: 30px;",
      markdown(
        "`{propose}` is developed at the [Centre for the Mathematical
        Modelling of Infectious Diseases](https://www.lshtm.ac.uk/research/centres/centre-mathematical-modelling-infectious-diseases)
        at the [London School of Hygiene and Tropical Medicine](https://www.lshtm.ac.uk/)
        and is funded by [ESCAPE](https://www.escapepandemics.com/) and
        [HPRU-HAM](https://www.lshtm.ac.uk/research/centres-projects-groups/hpru-ham)."
      )
    ),

    tags$div(
      style = "display: flex; justify-content: center; align-items: center; gap: 20px; padding-top: 30px; padding-bottom: 30px;",
      tags$img(
        src = "ESCAPE_logo.avif",
        style = "width: 250px;"
      ),
      tags$img(
        src = "EU_logo.avif",
        style = "width: 250px;"
      )
    ),

    tags$div(
      style = "padding-top: 30px; padding-bottom: 30px;",
      tags$p(
        "The ESCAPE project is an EU-funded research initiative
    focused on improving how governments and public health agencies prepare for
    and respond to future pandemics, including emerging “Pathogen X” threats.
    It is a consortium of 8 European institutions. Building on lessons from
    COVID-19, the project aims to develop a science-based preparedness
    blueprint that supports faster, evidence-informed decision-making during
    outbreaks.",
      ),
      tags$p(
        "Through improved data readiness, modelling tools, and decision-support
    frameworks, ESCAPE seeks to strengthen coordination between researchers,
    policymakers, and the public. By identifying what worked — and what did
    not — in past responses, the project aims to reduce the health, societal,
    and economic impacts of future pandemics across Europe and beyond."
      )
    ),

    tags$div(
      style = "display: flex; justify-content: center; align-items: center; gap: 20px; padding-top: 30px; padding-bottom: 30px;",
      tags$img(
        src = "NIHR_logo.png",
        style = "width: 250px;"
      ),
      tags$img(
        src = "NIHR_hpru_ham_logo.webp",
        style = "width: 250px;"
      )
    ),

    tags$div(
      tags$p(
        "The NIHR Health Protection Research Unit in Health & Analytics
        Modelling (HPRU-HAM) is a strategic research partnership between
        the London School of Hygiene & Tropical Medicine (LSHTM), the UK
        Health Security Agency (UKHSA) and Imperial College London, funded
        by the National Institute for Health and Care Research (NIHR) to
        strengthen UK health protection research and practice. The unit
        conducts cutting-edge analytical research to improve the way diverse
        health data are integrated, interpreted, and used to inform
        real-time decision-making during public health emergencies. Its
        work spans developing statistical and mathematical methods, building
        modelling and simulation tools, and translating research into
        practical software and training to boost UKHSA’s capacity to analyse
        complex surveillance data and forecast emerging threats."
      ),

      tags$p(
        "HPRU-HAM’s research is organised around connected themes including
      health equity and inclusion, health forecasting, and pandemic
      preparedness and preparedness evaluation. These themes aim to
      elucidate drivers of outbreak spread and healthcare demand, synthesise
      evidence from large and varied data sources, assess cost and impact
      of interventions, and enhance analytical methods that support outbreak
      control and health system resilience. The unit also emphasises
      engaging with stakeholders to ensure its work informs public health
      policies, improves forecasting accuracy, and supports equitable and
      effective responses to current and future health threats.",
      )
    )
  )
}

# The funding page currently has no server logic
# funding_server <- function(id) {
#  moduleServer(id, function(input, output, session)) {
#    # Insert funding server logic here
#  }
# }
