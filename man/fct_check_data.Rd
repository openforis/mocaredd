% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/fct_check_data.R
\name{fct_check_data}
\alias{fct_check_data}
\title{Check that the app's the input data has matching land uses and correct carbon pools and associated factors.}
\usage{
fct_check_data(.ad, .cs, .init)
}
\arguments{
\item{.ad}{Activity Data input table for the shiny app (AD_lu_transitions)}

\item{.cs}{Carbon Stock input table for the shiny app (c_stocks)}

\item{.init}{initial list of coded variables for carbon pools and REDD+ activity
to check the imported data against.}
}
\value{
A dataframe with TRUE or FALSE for each flag, (TRUE meaning the flag is raised and an issue was detected).
}
\description{
TBD
}
\examples{
library(mocaredd)
library(readxl)

path <- system.file("extdata/example1-4pools.xlsx", package = "mocaredd")

cs <- read_xlsx(path = path, sheet = "c_stocks", na = "NA")
ad <- read_xlsx(path = path, sheet = "AD_lu_transitions", na = "NA")

.init <- init <- list(
  c_pools = c("AGB", "BGB", "RS", "DW", "LI", "SOC", "ALL", "DG_ratio"),
  redd_acti = c("DF", "DG", "EN", "EN_AF", "EN_RE")
)

fct_check_data(.ad = ad, .cs = cs, .init = init)

}
