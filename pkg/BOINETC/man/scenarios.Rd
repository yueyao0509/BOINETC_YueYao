\name{BOINETC_scenarios}
\alias{BOINETC_scenarios}
\alias{BOINETC_ptox}
\alias{BOINETC_peff}
\alias{BOINETC_tdose}
\alias{get_boinetc_scenario}
\alias{pt.true.mat1}
\alias{pe.true.mat1}
\alias{pt.true.mat2}
\alias{pe.true.mat2}
\alias{pt.true.mat3}
\alias{pe.true.mat3}
\alias{pt.true.mat4}
\alias{pe.true.mat4}
\alias{pt.true.mat5}
\alias{pe.true.mat5}
\alias{pt.true.mat6}
\alias{pe.true.mat6}
\alias{pt.true.mat7}
\alias{pe.true.mat7}
\alias{pt.true.mat8}
\alias{pe.true.mat8}
\alias{pt.true.mat9}
\alias{pe.true.mat9}
\alias{pt.true.mat10}
\alias{pe.true.mat10}
\alias{list.p.truetox}
\alias{list.p.trueeff}
\alias{list.tdose}
\title{BOIN-ETC scenario definitions}
\usage{
BOINETC_scenarios
BOINETC_ptox
BOINETC_peff
BOINETC_tdose
get_boinetc_scenario(SC)
}
\arguments{
\item{SC}{Scenario number from 1 to 10.}
}
\value{\code{get_boinetc_scenario()} returns a list containing scenario id, toxicity matrix, efficacy matrix, target doses, and dimensions.}
\description{Predefined toxicity and efficacy scenarios from the provided BOIN-ETC scripts.}
