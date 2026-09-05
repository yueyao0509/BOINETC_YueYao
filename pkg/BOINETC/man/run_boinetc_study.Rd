\name{run_boinetc_study}
\alias{run_boinetc_study}
\alias{run_boinetc_workflow}
\alias{write_boinetc_summary_workbook}
\title{Convenience workflows for BOIN-ETC simulations}
\usage{
run_boinetc_study(methods = c("BOINETC_m1", "BOINETC_m2", "BOINETC_m3"),
  scenario_ids = 3:6, ncohort = c(16, 24), cohortsize = 3,
  n.earlystop = c(6, 9), ntrial = 1000, phi = 0.3, delta = 0.6,
  lambda1 = 0.14, lambda2 = 0.35, eta1 = 0.48, u.min = NULL,
  outdir = getOption("BOINETC.outdir", "."), seed = NULL,
  filename_prefix = NULL)

run_boinetc_workflow(methods = c("BOINETC_m1", "BOINETC_m2", "BOINETC_m3"),
  scenario_ids = 3:6, ncohort = c(16, 24), cohortsize = 3,
  n.earlystop = c(6, 9), ntrial = 1000, phi = 0.3, delta = 0.6,
  lambda1 = 0.14, lambda2 = 0.35, eta1 = 0.48,
  u = c(100, 25, 75, 0), alpha = 1, beta = 1, u.min = NULL,
  outdir = getOption("BOINETC.outdir", "."), seed = 1234,
  write_workbook = TRUE, workbook_file = NULL)

write_boinetc_summary_workbook(out.sum1, out.sum2, file, overwrite = TRUE)
}
\arguments{
\item{methods}{BOIN-ETC method labels.}
\item{scenario_ids}{Scenario numbers to run.}
\item{ncohort, cohortsize, n.earlystop, ntrial}{Simulation size and stopping settings.}
\item{phi, delta, lambda1, lambda2, eta1}{Design parameters.}
\item{u, alpha, beta, u.min}{Utility settings.}
\item{outdir, seed, filename_prefix, write_workbook, workbook_file, out.sum1, out.sum2, file, overwrite}{Workflow and output settings.}
}
\value{A list of simulation files for \code{run_boinetc_study}; a list of simulation outputs and summary tables for \code{run_boinetc_workflow}; invisibly returns the workbook path for \code{write_boinetc_summary_workbook}.}
\description{Package-friendly replacements for the original source-based main workflow. Excel output requires the optional package \pkg{openxlsx}.}
