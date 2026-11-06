# Generate and propagate Monte Carlo Simulations based on a template input file.

TBD

## Usage

``` r
fct_combine_mcs_E(.checked_data)
```

## Arguments

- .checked_data:

  The list returned by
  [`fct_checkinput()`](https://openforis.github.io/mocaredd/reference/fct_checkinput.md).
  Its `template_version` element drives version-specific behaviour and
  its `data` element supplies the `setup`, `time`, `area` and `carbon`
  tables.

## Value

A data frame with Monte Carlo simulations of CO2 emissions for each land
use transition, REDD+ activity or emission reductions level.

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

res <- fct_combine_mcs_E(.checked_data = checked)

get_trans <- sample(res$trans_id, 1)
res_sub <- res |> dplyr::filter(trans_id == get_trans)

hist(res_sub$E)

```
