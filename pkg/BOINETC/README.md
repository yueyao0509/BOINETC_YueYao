# BOINETC

`BOINETC` is an R package assembled from the provided BOIN-ETC simulation scripts. It preserves the original function names and adds package-friendly helpers for scenario selection, simulation, and summary output.

## Included core functions

- `BOINETC.subtrial()` — select the next dose in a BOIN-ETC subtrial.
- `get.oc.BOINETC()` — calculate operating characteristics for BOIN-ETC1.
- `get.oc.BOINETC_m2()` — calculate operating characteristics for BOIN-ETC2.
- `get.oc.BOINETC_m3()` — calculate operating characteristics for BOIN-ETC3.
- `CALC.SUM()` — summarize simulation data and identify ODCs.

## Added convenience helpers

- `get_boinetc_scenario(SC)` — retrieve one of the 10 included scenarios.
- `run_boinetc_study()` — run one or more BOIN-ETC methods over selected scenarios.
- `run_boinetc_workflow()` — run simulations and summaries in one call.
- `write_boinetc_summary_workbook()` — write `Sum1` and `Sum2` summary tables to Excel.
- `run_boinetc_app()` — launch the included Shiny app.

The original uploaded scripts are preserved under `inst/extdata/original/`.

## Installation

From the source tarball:

```r
install.packages("BOINETC_0.1.0.tar.gz", repos = NULL, type = "source")
```

From an unpacked local folder:

```r
install.packages("path/to/BOINETC", repos = NULL, type = "source")
```

## Optional dependencies

`CALC.SUM()` and `run_boinetc_workflow()` require a package that provides `biviso()` such as `UniIsoRegression` or `Iso`. Excel output requires `openxlsx`. The Shiny app requires `shiny` and `DT`.

```r
install.packages(c("shiny", "DT", "UniIsoRegression", "Iso", "openxlsx"))
```

## Shiny app

After installing the package and app dependencies, launch the interactive app with:

```r
library(BOINETC)
run_boinetc_app()
```

The app lets you select BOIN-ETC1/2/3, choose scenarios, set simulation/design parameters, preview toxicity and efficacy matrices, run operating-characteristic simulations, and download summary CSV/ZIP files.

## Quick example

```r
library(BOINETC)

outdir <- tempfile("boinetc-")
dir.create(outdir)

sc <- get_boinetc_scenario(1)

set.seed(1234)
get.oc.BOINETC(
  ncohort = 2,
  cohortsize = 3,
  n.earlystop = 6,
  ntrial = 5,
  phi = 0.30,
  delta = 0.60,
  lambda1 = 0.14,
  lambda2 = 0.35,
  eta1 = 0.48,
  tdose = sc$tdose,
  pt.true.mat = sc$pt.true.mat,
  pe.true.mat = sc$pe.true.mat,
  filename = "demo_m1_sc1",
  outdir = outdir
)
```

A full workflow, after installing optional dependencies:

```r
library(BOINETC)

res <- run_boinetc_workflow(
  scenario_ids = 3:6,
  ncohort = c(16, 24),
  n.earlystop = c(6, 9),
  ntrial = 1000,
  outdir = "output",
  seed = 1234
)

res$out.sum1
res$out.sum2
```

## Notes

The package removes the need to call `source()` manually and replaces path globals such as `OUTFILE` and `OUT` with an `outdir` argument. For backward compatibility, if `outdir` is omitted and a caller has `OUTFILE` or `OUT` defined, those values will be used.
