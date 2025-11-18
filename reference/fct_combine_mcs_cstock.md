# Generate and propagate Monte Carlo Simulations based on a template input file.

TBD

## Usage

``` r
fct_combine_mcs_cstock(.ad, .cs, .usr)
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

res <- fct_combine_mcs_cstock(.ad = ad, .cs = cs, .usr = usr)
res |> filter(sim_no == 1)
#> # A tibble: 5 × 11
#>   sim_no period lu_id    AGB     RS   ALL     CF c_form         c_stock DG_ratio
#>    <int> <chr>  <chr>  <dbl>  <dbl> <dbl>  <dbl> <chr>            <dbl>    <dbl>
#> 1      1 ALL    EV      120.  0.299    NA  0.472 (AGB + AGB * …    73.7   NA    
#> 2      1 ALL    M       206.  0.374    NA  0.472 (AGB + AGB * …   133.    NA    
#> 3      1 ALL    Crop     NA  NA         0  0.472 ALL                0     NA    
#> 4      1 ALL    EV_deg   NA  NA        NA NA     DG_ratio * (c…    64.8    0.879
#> 5      1 ALL    M_deg    NA  NA        NA NA     DG_ratio * (c…    80.7    0.605
#> # ℹ 1 more variable: c_stock_before <dbl>
```
