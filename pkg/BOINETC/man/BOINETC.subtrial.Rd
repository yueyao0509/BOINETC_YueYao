\name{BOINETC.subtrial}
\alias{BOINETC.subtrial}
\title{Run one BOIN-ETC subtrial}
\usage{
BOINETC.subtrial(pt.true, pe.true, dosespace, npts, ntox, neff,
  nct1, nct2, nct3, nct4, elimi, ncohort, cohortsize, phi, delta,
  lambda1, lambda2, eta1, n.earlystop = 20, startdose = 1,
  cutoff.eli = 0.95, extrasafe = FALSE, offset = 0.05, sum.nn,
  titration.first.trial = FALSE)
}
\arguments{
\item{pt.true, pe.true}{True toxicity and efficacy probabilities for the current dose space.}
\item{dosespace}{Vector of dose-combination indices included in the subtrial.}
\item{npts, ntox, neff, nct1, nct2, nct3, nct4, elimi}{Current study matrices updated by the subtrial.}
\item{ncohort, cohortsize}{Maximum cohorts and cohort size.}
\item{phi, delta, lambda1, lambda2, eta1}{Design parameters.}
\item{n.earlystop, startdose, cutoff.eli, extrasafe, offset, sum.nn, titration.first.trial}{Stopping and dose-escalation settings.}
}
\value{A list containing updated dose matrices, selected dose, elimination status, and sample-size summaries.}
\description{Runs a BOIN-ETC subtrial and selects the next dose based on toxicity and efficacy outcomes.}
