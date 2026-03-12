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
