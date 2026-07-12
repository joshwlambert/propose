# propose v0.3.0

The third minor release of `{propose}`. This release adds several new pre-packaged analysis pages, expands the range of configurable epidemiological and intervention parameters, and extends the `{propose}` manual.

This release is for beta testing at the [Proposing solutions: Shaping a new pandemic decision-support tool](https://www.lshtm.ac.uk/newsevents/events/proposing-solutions-shaping-new-pandemic-decision-support-tool) workshop. This workshop aims to demonstrate `{propose}` and is the first time stakeholders will use the app and provide feedback.

## Features

* Added a set of pre-packaged analysis pages, grouped under an **Analyses** dropdown menu in the navbar and linked from a grid on the Home page:

  - **Contact Tracing Effectiveness**, which sweeps over a sequence of contact tracing values to show how outbreak control varies with tracing effectiveness (new `tracing_effectiveness_ui()` and `tracing_effectiveness_server()` module; #45).
  - **Tracing Strategies**, for comparing contact tracing strategies with different onset-to-isolation delays (new `tracing_strategies_ui()` and `tracing_strategies_server()` module; #46).
  - **Outbreak Size & Length**, which sweeps over a sequence of reproduction number values and summarises outbreak size and length (new `outbreak_size_ui()` and `outbreak_size_server()` module; #48).

* Added plots of the offspring, incubation period, onset-to-isolation and presymptomatic transmission distributions, shown in a **Show simulation parameter distributions** panel on the Explore, Contact Tracing Effectiveness, Tracing Strategies and Outbreak Size & Length pages, which update as the corresponding parameters change (#44).

* Added a Basic/Advanced toggle to the pathogen transmissibility (offspring), incubation period and onset-to-isolation delay inputs, allowing users to specify either summary values or the full distribution parameters (#31).

* Added an optional "Different asymptomatic transmissibility" toggle to the _Advanced_ Pathogen Transmissibility input. When enabled, asymptomatic cases are given a separate offspring distribution, reproduction number and dispersion (_k_); when disabled (the default), asymptomatic cases transmit identically to symptomatic community cases. The asymptomatic offspring distribution is overlaid on the offspring distribution plot while the toggle is on (#52).

* Added a non-pharmaceutical intervention (NPI) activation delay parameter, controlling the delay before interventions take effect (#53).

* Added a test sensitivity parameter to the intervention controls (#50).

* Added a master switch to turn case isolation on or off in the intervention controls, revealing the onset-to-isolation and quarantine controls only when isolation is enabled (#49).

* Added Case studies that describe real-world examples of targeted interventions in response to infectious disease epidemics. Two case studies are included in this release — COVID-19 in Singapore (Case 01) and England (Case 02) — which explore contact tracing strategies. Case studies are accessible from the Docs menu and a Home page banner (new `case_study_ui()` module; #51).

* Added confidence intervals to the outbreak control value boxes on the Explore and Tracing Strategies pages.

## User interface

* Expanded the in-app manual, adding and updating the Overview, Quick start, Explore outbreak scenarios, Tracing Effectiveness, Tracing Strategies, Outbreak Size & Length, and Docs & Case studies content, with figures redesigned to match the app theme (#54).

* Added figure legends beneath the plots on the Explore, Contact Tracing Effectiveness and Outbreak Size & Length pages, and, on the Explore page, only plot the un-capped portion of the outbreak, with a warning on the weekly cases plot when the outbreak is capped (#45, #48, #55).

* Updated the Home page with a **Pre-packaged outbreak analyses** section, a grid of clickable cards linking to the Contact Tracing Effectiveness, Tracing Strategies and Outbreak Size & Length pages, and a Case studies banner linking to the Singapore and England case studies (#45, #46, #48, #51).

* Scroll back to the top of the page when navigating to a page from a Home page link or button.

* Replaced the text navbar title with a `{propose}` name logo image and added a window title.

* Added consistently styled `{propose}` and `{ringbp}` names across the app.

* Updated the [`_brand.yml`](https://posit-dev.github.io/brand-yml/) theming with the `{propose}` colour palette and hex and name logo definitions.

* Added a Development team section to the About page, with profile cards, and added the team to the `DESCRIPTION` authors.

* Added the `{propose}` version number to the About page.

* Expressed the contact tracing, asymptomatic and presymptomatic transmission parameters as percentages in the UI.

* Removed the Compare page from the app and manual until it is redesigned (#54).

## Structure and internals

* Merged `incubation_input()` and `onset_to_isolation_input()` into a single `delays_input()` function, with an `accordion` argument controlling whether it returns individual controls or an accordion (#31, #49).

* Merged `contact_tracing_input()` and `delays_input()` into `intervention_input()`, which now also handles the test sensitivity and NPI activation delay parameters (#49, #50, #53).

* Organised input validation with `{shinyFeedback}` into `*_feedback_server()` functions (e.g. `r0_seq_feedback_server()`, `test_sensitivity_feedback_server()`, `npi_activation_day_feedback_server()`) shared across pages (#45, #50, #53).

* Added the `distribution_plots.R` module of reusable distribution plotting functions (`offspring_dist_plot()`, `incubation_dist_plot()`, `onset_to_isolation_dist_plot()`, `presymptomatic_dist_plot()`), handling both the Basic and Advanced input UIs and reused across the Explore and analysis pages (#44, #31).

* Added the `r0_seq_input()` and `contact_tracing_seq_input()` functions for the sequence-based analyses (#45, #48).

* Added the `patho_param_input()` function for the pathogen presets dropdown, shared across pages.

* Added the `propose_name()` and `ringbp_name()` helpers for styled package names, and the `profile_card()` helper for the Development team section.

* Added the `NO_ISOLATION_DELAY` and `npi_activation()` constants and defaults (#49, #53).

* Extended `PROPOSE_DEFAULTS` with additional pathogen parameters, including asymptomatic transmissibility, test sensitivity and NPI activation defaults (#50, #52, #53).

* Added `{sn}` and `{epiparameter}` to `Imports` (#44).

* Redocumented the package with `{roxygen2}` v8.0.0.

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
