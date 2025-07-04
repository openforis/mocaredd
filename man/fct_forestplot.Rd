% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/fct_forestplot.R
\name{fct_forestplot}
\alias{fct_forestplot}
\title{Make a forest plot to show Monte Carlo simulation results}
\usage{
fct_forestplot(
  .data,
  .id,
  .value_ari = NULL,
  .value,
  .uperc,
  .cilower,
  .ciupper,
  .id_colname,
  .conflevel,
  .filename = NA
)
}
\arguments{
\item{.data}{a data frame containing the simulation aggregated results}

\item{.id}{the name of the ID column of the data frame}

\item{.value_ari}{NULL if not relevant (most cases) or a column name with the arithmetic mean of Emission or emission reductions.}

\item{.value}{the name of the value column of the data frame (i.e median of the simulations)}

\item{.uperc}{the name of the percentage uncertainty of the simulations}

\item{.cilower}{the name of the lower end of confidence interval}

\item{.ciupper}{the name of the higher end of the confidence interval}

\item{.id_colname}{character text for the ID column names. goes to \code{\link[gt:md]{gt::md()}} in \code{\link[gt:cols_label]{gt::cols_label()}}}

\item{.conflevel}{character text to specify what confidence level was used, ex. "90\%".}

\item{.filename}{path to save the table or NA.}
}
\value{
A character value with the formula for calculating total carbon stock for a specific land use.
}
\description{
Produce a \code{\link[gt:gt]{gt::gt()}} table with the median and confidence interval of
Monte Carlo simulations for each category of a level of the analysis
(land use transition, REDD+ activity, time period) and a forest plot
showing the median and CI together.
}
