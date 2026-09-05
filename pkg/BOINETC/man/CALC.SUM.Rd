\name{CALC.SUM}
\alias{CALC.SUM}
\title{Summarize BOIN-ETC simulation output}
\usage{
CALC.SUM(FILENAME, SC, outdir = NULL, list.tdose = BOINETC_tdose,
  list.p.truetox = BOINETC_ptox, list.p.trueeff = BOINETC_peff,
  ntrial = NULL, phi = NULL, ncohort = NULL, cohortsize = NULL,
  delta = NULL, n.es = NULL, lambda1 = NULL, lambda2 = NULL,
  eta1 = NULL, u = NULL, alpha = NULL, beta = NULL, method = NULL,
  write_summary = TRUE)
}
\arguments{
\item{FILENAME}{Base filename of the simulation CSV, without \code{_data.csv}.}
\item{SC}{Scenario number.}
\item{outdir}{Directory containing simulation output and receiving summary files.}
\item{list.tdose, list.p.truetox, list.p.trueeff}{Scenario lists.}
\item{ntrial, phi, ncohort, cohortsize, delta, n.es, lambda1, lambda2, eta1}{Simulation and design parameters.}
\item{u, alpha, beta}{Utility scores and prior parameters.}
\item{method}{Method label for summary tables.}
\item{write_summary}{Whether to write a text summary file.}
}
\value{A list with \code{out.sum1}, \code{out.sum2}, ODC percentages, mean dose-level summaries, and file paths.}
\description{Reads a BOIN-ETC simulation CSV, estimates ODC selection percentages, and creates summary tables. Requires an optional package that provides \code{biviso()}, such as \pkg{UniIsoRegression} or \pkg{Iso}.}
