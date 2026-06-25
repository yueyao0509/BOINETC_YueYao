# BOINETC Shiny App

This folder contains a standalone Shiny app for the `BOINETC` R package.

## What the app does

- Select one or more BOIN-ETC methods: BOIN-ETC1, BOIN-ETC2, BOIN-ETC3.
- Select one or more built-in scenarios.
- Preview toxicity and efficacy probability matrices and target dose locations.
- Set simulation parameters such as number of trials, cohort size, maximum cohorts, early-stop thresholds, target toxicity, efficacy target, decision thresholds, and utility scores.
- Run `BOINETC::run_boinetc_workflow()`.
- View and download summary tables and generated simulation files.

## Installation and launch

From this folder in R:

```r
install.packages(c("shiny", "DT", "openxlsx", "Iso"))
install.packages("BOINETC_0.1.0.tar.gz", repos = NULL, type = "source")
shiny::runApp(".")
```

Or source the launcher:

```r
source("install_and_run.R")
```

`Iso` or `UniIsoRegression` is needed because the package summary step uses `biviso()`. `openxlsx` enables Excel workbook output; the app can still return CSV summaries without Excel output.

## Recommended first run

Use the default settings with `ntrial = 100` for a quick smoke test. Increase `ntrial` to 1,000 or more for final operating-characteristic simulations.
