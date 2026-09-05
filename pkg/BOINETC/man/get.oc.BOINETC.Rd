\name{get.oc.BOINETC}
\alias{get.oc.BOINETC}
\alias{get.oc.BOINETC_m2}
\alias{get.oc.BOINETC_m3}
\title{Calculate BOIN-ETC operating characteristics}
\usage{
get.oc.BOINETC(ncohort, cohortsize, n.earlystop, ntrial, phi, delta,
  lambda1, lambda2, eta1, tdose = NULL, u.min = NULL, n.scr = NULL,
  pt.true.mat, pe.true.mat, filename, outdir = NULL)

get.oc.BOINETC_m2(ncohort, cohortsize, n.earlystop, ntrial, phi, delta,
  lambda1, lambda2, eta1, tdose = NULL, u.min = NULL, n.scr = NULL,
  pt.true.mat, pe.true.mat, filename, outdir = NULL)

get.oc.BOINETC_m3(ncohort, cohortsize, n.earlystop, ntrial, phi, delta,
  lambda1, lambda2, eta1, tdose = NULL, u.min = NULL, n.scr = NULL,
  pt.true.mat, pe.true.mat, filename, outdir = NULL)
}
\arguments{
\item{ncohort, cohortsize, n.earlystop, ntrial}{Simulation size and stopping settings.}
\item{phi, delta, lambda1, lambda2, eta1}{Design parameters.}
\item{tdose, u.min, n.scr}{Scenario metadata retained for compatibility with the original scripts.}
\item{pt.true.mat, pe.true.mat}{Matrices of true toxicity and efficacy probabilities.}
\item{filename}{Base filename for the generated data CSV.}
\item{outdir}{Output directory. If omitted, the function checks caller variables \code{OUTFILE} or \code{OUT}, then uses \code{getOption("BOINETC.outdir", ".")}.}
}
\value{Invisibly returns a list with the simulated data matrix, output file, and method name. A CSV file is written to \code{outdir}.}
\description{Simulates operating characteristics for BOIN-ETC1, BOIN-ETC2, or BOIN-ETC3.}
