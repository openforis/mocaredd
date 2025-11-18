# Generate and propagate Monte Carlo Simulations based on a template input file.

TBD

## Usage

``` r
fct_combine_mcs_E(.ad, .cs, .usr)
```

## Arguments

- .ad:

  Activity Data input table for the shiny app (AD_lu_transitions)

- .cs:

  Carbon Stock input table for the shiny app (c_stocks)

- .usr:

  User inputs' table for the shiny app (user_inputs). Contains the
  number of iterations of the MCS, carbon fraction if needed and if
  truncated PDFs should be used when necessary.

## Value

A data frame with Monte Carlo simulations of CO2 emissions for each land
use transition, REDD+ activity or emission reductions level.

## Examples

``` r
library(mocaredd)
library(readxl)
library(dplyr)

path <- system.file("extdata/example2-with-sims.xlsx", package = "mocaredd")

cs <- read_xlsx(path = path, sheet = "c_stocks", na = "NA")
ad <- read_xlsx(path = path, sheet = "AD_lu_transitions", na = "NA")
usr <- read_xlsx(path = path, sheet = "user_inputs", na = "NA")

cs_clean <- cs |> filter(!is.na(c_value) | !is.na(c_pdf_a))

res <- fct_combine_mcs_E(.ad = ad, .cs = cs_clean, .usr = usr)

get_trans <- sample(res$trans_id, 1)
res_sub <- res |> filter(trans_id == get_trans)

hist(res_sub$E)

```
