\name{run_boinetc_app}
\alias{run_boinetc_app}
\title{Run the BOIN-ETC Shiny app}
\usage{
run_boinetc_app(...)
}
\arguments{
\item{...}{Arguments passed to \code{shiny::runApp()}, such as \code{launch.browser = TRUE}.}
}
\value{Runs the Shiny application.}
\description{Launches an interactive Shiny app for selecting BOIN-ETC methods, scenarios, simulation settings, and reviewing/downloading operating-characteristic summaries. Requires \pkg{shiny} and \pkg{DT}. Summary calculations require a package providing \code{biviso()}, such as \pkg{Iso} or \pkg{UniIsoRegression}.}
