# Carbon stock formulas and IPCC Approach 1 uncertainty per land use

Builds, for each period and land use of the carbon table, the carbon
stock formula and its variance and relative uncertainty formulas (IPCC
2006, Vol. 1, Ch. 3, Eq. 3.1 and 3.2).

             Each stock is written as \code{C = CF * S_DM + S_C}, with
             \code{S_DM} the sum of pools in dry matter and \code{S_C} the sum
             of pools in carbon. CF is factored once, so its uncertainty is
             not counted as independent across pools:

             \code{Var(C) = CF^2 * V_DM + (CF * CF_U * S_DM)^2 + V_C}

             with \code{V_S = sum((X * X_U)^2)} over the pools of \code{S}.
             With RS, AGB becomes \code{AGB * (1 + RS)} with variance
             \code{(AGB * (1 + RS) * AGB_U)^2 + (AGB * RS * RS_U)^2}, and BGB is
             ignored. If \code{ALL} is given, it replaces the other pools.

             \code{c_tot_U = sqrt(Var(C)) / C}, simplified with the product
             rule when the stock is a product (one pool, or DM pools only),
             e.g. \code{sqrt(AGB_U^2 + (RS / (1 + RS) * RS_U)^2 + CF_U^2)}.

             Evaluate with each element and its relative uncertainty as
             \code{<element>} and \code{<element>_U} (se / mean). Land uses
             without pools (e.g. DG_ratio only) return NA.

## Usage

``` r
fct_cstock_formula2(.carbon)
```

## Arguments

- .carbon:

  Carbon table, from `fct_checkinput()$data$carbon`.

## Value

A tibble with `c_period`, `c_lu`, `c_tot` (stock), `c_tot_V` (variance)
and `c_tot_U` (relative uncertainty).

## Examples

``` r
path    <- system.file("extdata/mocaredd-templatev2-simple.xlsx", package = "mocaredd")
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
fct_cstock_formula2(checked$data$carbon)
```
