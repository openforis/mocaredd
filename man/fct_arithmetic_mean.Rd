% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/fct_arithmetic_mean.R
\name{fct_arithmetic_mean}
\alias{fct_arithmetic_mean}
\title{Calculate emissions based on arithmetic means}
\usage{
fct_arithmetic_mean(.ad, .cs, .usr, .time)
}
\arguments{
\item{.ad}{Activity Data input table for the shiny app (AD_lu_transitions)}

\item{.cs}{Carbon Stock input table for the shiny app (c_stocks)}

\item{.usr}{User inputs' table for the shiny app (user_inputs). Contains the number
of iterations of the MCS, carbon fraction if needed and if truncated PDFs
should be used when necessary.}

\item{.time}{the 'time' table from the tool input file (see template)}
}
\value{
A data frame with arithmetic mean of CO2 emissions for each land use
transition, REDD+ activity or emission reductions level.
}
\description{
TBD
}
\examples{
library(readxl)
library(dplyr)
library(mocaredd)

path <- system.file("extdata/example2-with-sims.xlsx", package = "mocaredd")
path <- system.file("extdata/example1-4pools.xlsx", package = "mocaredd")

cs <- read_xlsx(path = path, sheet = "c_stocks", na = "NA")
ad <- read_xlsx(path = path, sheet = "AD_lu_transitions", na = "NA")
usr <- read_xlsx(path = path, sheet = "user_inputs", na = "NA")
time <- read_xlsx(path = path, sheet = "time_periods", na = "NA")

time_clean <- time |> dplyr::mutate(nb_years = year_end - year_start + 1)

res <- fct_arithmetic_mean(.ad = ad, .cs = cs, .usr = usr, .time = time_clean)
head(res$emissions)
res$gg_emissions

}
