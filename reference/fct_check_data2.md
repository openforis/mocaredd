# Check that the app's input data meet the template criteria.

Tests 6 types of conformity with the app template to avoid code break
during analysis: (1) column names, (2) tables' size, (3) data types, (4)
categories names, (5) unique IDs and (6) matching key variables between
tables.

## Usage

``` r
fct_check_data2(.usr, .time, .ad, .cs)
```

## Arguments

- .usr:

  User inputs' table for the shiny app (user_inputs)

- .time:

  the 'time' table from the tool input file (see template)

- .ad:

  Activity Data input table for the shiny app (AD_lu_transitions)

- .cs:

  Carbon Stock input table for the shiny app (c_stocks)

## Value

A dataframe with TRUE or FALSE (TRUE if each check passes), and broad
error locations if FALSE.

## Examples

``` r
library(mocaredd)
library(readxl)

path <- system.file("extdata/example1-4pools.xlsx", package = "mocaredd")

cs <- read_xlsx(path = path, sheet = "c_stocks", na = "NA")
ad <- read_xlsx(path = path, sheet = "AD_lu_transitions", na = "NA")
usr <- read_xlsx(path = path, sheet = "user_inputs", na = "NA")
time <- read_xlsx(path = path, sheet = "time_periods", na = "NA")

fct_check_data2(.ad = ad, .cs = cs, .usr = usr, .time = time)
#> $cols_ok
#> [1] TRUE
#> 
#> $size_ok
#> [1] TRUE
#> 
#> $datatypes_ok
#> [1] TRUE
#> 
#> $cats_ok
#> [1] TRUE
#> 
#> $ids_ok
#> [1] TRUE
#> 
#> $matches_ok
#> [1] TRUE
#> 
#> $all_ok
#> [1] TRUE
#> 
#> $pbs
#> named list()
#> 
```
