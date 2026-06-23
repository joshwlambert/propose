# propose v0.2.1

A patch release to fix a deployment error on shinyapps.io. The deployment of `{propose}` v0.2.0 errored because the `{brand.yml}` package was not installed in the deployment environment — `{rsconnect}` resolves only `Depends`/`Imports`/`LinkingTo` dependencies, and `{brand.yml}` is a suggested dependency of `{bslib}`. This has been resolved by adding `{brand.yml}` to `Imports` in the `DESCRIPTION`.

# propose v0.2.0

The second minor release of `{propose}`. This release is coupled with the release of [`{ringbp}` v1.0.0](https://github.com/epiforecasts/ringbp/releases/tag/v1.0.0).

This release contains several updates, many addressing feedback from the (pre-)alpha testing at the [ESCAPE](https://www.escapepandemics.com/) General Assembly 2026.

## Features

* Added selectable pathogen parameter presets (_Disease X_, _COVID-19_ and _Ebola_) to the Explore page, which populate the simulation parameters with values based on estimates from the literature (#33).

* Added a "Reset Defaults" button to the Explore page to restore all simulation parameters to their default values (#32).

* Improved the outbreak trajectory plot on the Explore page by only plotting times where the outbreak is active (i.e. not extinct or reached the maximum number of cases) (#43).

## User interface

* Reorganised the Explore sidebar into **Pathogen**, **Intervention** and **Simulation Control** parameter groups (#37).

* Added contextual tooltips to all parameter controls, including offspring distribution, incubation period, symptom event probability, onset-to-isolation, contact tracing, intervention and simulation control parameters (#42).

* Added a Docs menu providing the `{propose}` manual and the `{ringbp}` vignettes (#36, #38).

* Added a banner on the Home page linking to the `{propose}` manual (#36).

* Added an FAQ entry and manual content explaining the Negative binomial offspring distribution dispersion parameter (_k_) (#34).

* Added [theming](https://rstudio.github.io/bslib/articles/brand-yml/index.html) loaded from [`_brand.yml`](https://posit-dev.github.io/brand-yml/) (#19).

## Structure and internals

* Added the `PROPOSE_DEFAULTS` list of default simulation parameters, with per-pathogen presets, and the `reset_pathogen_params()` helper to reset the Explore page parameters (#32, #33).

* Added `*_input()` helper functions for shared parameter panels: `contact_tracing_input()`, `symptom_event_prob_input()`, `onset_to_isolation_input()` and `incubation_input()` (#37).

* Added the `shape_tip()` and `scale_tip()` helper functions for generating distribution parameter tooltips (#42).

* Added the Docs module (`docs_ui()`) and the manual module (`manual_ui()` and `manual_server()`) (#36, #38).

# propose v0.1.0

First minor release of `{propose}`, a Shiny application for exploring outbreak control with targeted, individual-level interventions using the branching process simulations from `{ringbp}`.

This release is the version for (pre-)alpha testing at the [ESCAPE](https://www.escapepandemics.com/) General Assembly 2026.

## Features

* Added an interactive Shiny interface for simulating outbreak scenarios using `ringbp::scenario_sim()`.

* Users can configure epidemiological parameters including reproduction number, offspring distributions, delays (e.g. incubation period and onset-to-isolation), intervention parameters, number of initial cases, and number of simulation replicates.

* Added visualisations of simulated outbreaks including epidemic trajectories and summaries of outbreak outcomes.

* Added a Compare page for running and comparing two scenarios side-by-side.

## User interface

* Pages:
  - Home
  - Explore
  - Compare
  - About
  - FAQ
  - Funding
  - Citations
  - Contact us

* Implemented the application using `{bslib}` components including `page_navbar()` and card-based layouts.

* Added parameter panels with contextual help and tooltips.

* Added loading indicators while simulations are running using `{waiter}`.

* Added modal dialog pop-up when simulations may take a long time to run.

* Added input checking of simulation parameters using `req()` and `{shinyFeedback}`.

* Added hex logo.

## Structure and internals

* The app is organised into [Shiny modules](https://mastering-shiny.org/scaling-modules.html).

* Shared UI components are organised into R functions (e.g. `page_title()`, `offspring_input()` and other `*_input()` functions).

* `www/` folder is used for static app assets (e.g. images).

* Modules and functions are documented using `{roxygen2}`, with `.Rd` files in `man/`.

## Project infrastructure

* Added package metadata (`DESCRIPTION`, `NAMESPACE`).

* Added project documentation including: `README`, `NEWS.md`, `CONTRIBUTING`, and `CODE_OF_CONDUCT`.

* Added repository configuration files (`.gitignore`, `.Rbuildignore`).
