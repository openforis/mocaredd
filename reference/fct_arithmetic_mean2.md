# Calculate emissions and IPCC Tier 1 uncertainty from arithmetic means

Analytical counterpart of `fct_arithmetic_mean()`. Instead of Monte
Carlo simulations, activity data and emission factors are aggregated
deterministically and their uncertainty is propagated with the IPCC Tier
1 (first-order error propagation) rules: relative uncertainties combine
in quadrature for products and absolute standard errors combine in
quadrature for sums / differences. The propagation is done numerically
over the independent input variables (carbon pool values, carbon
fraction, degradation ratios and activity data), so correlations
introduced by shared inputs (e.g. a degraded land use that reuses the
intact carbon stock, or a global carbon fraction) are handled
consistently.

The result reports, for each reference / monitoring emission level and
for each emission reduction, the mean, its standard error, and the
half-width uncertainty \\U\\ = z \cdot se / \|mean\| \cdot 100\\, where
\\z\\ is the two-sided normal quantile for `setup$conf_level`.

## Usage

``` r
fct_arithmetic_mean2(.checked_data)
```

## Arguments

- .checked_data:

  The list returned by
  [`fct_checkinput()`](https://openforis.github.io/mocaredd/reference/fct_checkinput.md).
  Its `template_version` element drives version-specific behaviour and
  its `data` element supplies the `setup`, `time`, `area` and `carbon`
  tables.

## Value

A list with elements `ER` (reference / monitoring levels and emission
reductions with mean, se, U\\ (per time period, with uncertainty),
`gg_emissions` (figure with emissions as lines with confidence interval
areas, and emission reductions as points with error bars) and
`emissions_table` (a tidy tibble ready to be turned into a gt table by
the app).

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

res <- fct_arithmetic_mean2(.checked_data = checked)
res$ER
#> # A tibble: 3 × 6
#>   period_type         E     E_se   E_U  E_lower   E_upper
#>   <chr>           <dbl>    <dbl> <dbl>    <dbl>     <dbl>
#> 1 REF         21874156. 7658975.  57.6 9276262. 34472049.
#> 2 E-MON1       7925065. 2654361.  55.1 3559029. 12291100.
#> 3 ER-MON1     13949091. 5271751.  62.2 5277832. 22620350.
```
