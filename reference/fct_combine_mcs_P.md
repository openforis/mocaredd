# Combine MCS of emissions to a defined time period

Calculate the Emission Level for a reference or monitoring period for
each simulation, as the length-weighted average of the annual emissions
(E_year) of the periods it contains.

## Usage

``` r
fct_combine_mcs_P(.data, .period_type)
```

## Arguments

- .data:

  a data frame containing the simulations, output of
  fct_combine_mcs_E(). Must contain time_period, period_length,
  period_type, sim_no and E_year.

- .period_type:

  "REF" or "MON", matched against the period_type column.

## Value

A tibble with simulations at the final estimate per type of period.

## Examples

``` r
path <- system.file("extdata/mocaredd-templatev2-simple.xlsx", package = "mocaredd")

checked <- fct_checkinput(.path = path)
#> Loading data... - progress: 0%.
#> ✓ Tables loaded successfully from template v2
#> Checking column names... - progress: 14%.
#> ✓ Column names: all required columns present
#> Checking table dimensions... - progress: 29%.
#> ✓ Table sizes: all tables have sufficient rows
#> Checking column data types... - progress: 43%.
#> ✓ Data types: all columns have correct data types
#> Checking category values... - progress: 57%.
#> ✓ Category values: all categories are valid
#> Checking unique IDs... - progress: 71%.
#> ✓ Unique IDs: no duplicates or missing IDs found
#> Checking cross-table and intra_table consistency... - progress: 86%.
#> ✓ Cross-table consistency: all references match
#> -- All checks passed.

sim_trans <- fct_combine_mcs_E(.checked_data = checked)

sim_FREL <- fct_combine_mcs_P(.data = sim_trans, .period_type = "REF")

hist(sim_FREL$E)

round(median(sim_FREL$E))
#> [1] 20714297
```
