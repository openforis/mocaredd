# Calculate emissions and IPCC Approach 1 uncertainty from arithmetic means

Deterministic counterpart of the Monte Carlo chain
([`fct_combine_mcs_E()`](https://openforis.github.io/mocaredd/reference/fct_combine_mcs_E.md),
[`fct_combine_mcs_P()`](https://openforis.github.io/mocaredd/reference/fct_combine_mcs_P.md),
[`fct_combine_mcs_ER()`](https://openforis.github.io/mocaredd/reference/fct_combine_mcs_ER.md)),
following the same steps with means instead of simulations, and
propagating uncertainty with the IPCC Approach 1 rules (IPCC 2006, Vol.
1, Ch. 3, Eq. 3.1 and 3.2):

- product: relative standard errors combine in quadrature, \\U\_{a b} =
  \sqrt{U_a^2 + U_b^2}\\;

- sum / difference: absolute standard errors combine in quadrature,
  \\se\_{a + b} = \sqrt{se_a^2 + se_b^2}\\.

Calculation chain:

1.  Carbon stock of each land use from
    [`fct_make_formula2()`](https://openforis.github.io/mocaredd/reference/fct_make_formula2.md).
    Composite factors are resolved first: `(1 + RS)` and `(AGB + BGB)`.
    Then product rule within each term, sum rule across terms.

2.  Degraded land uses: `DG_ratio * C_intact` (product rule), plus pools
    excluded from degradation when `dg_pool` is not "ALL".

3.  Emission factors: `EF = (C_i - C_f) * 44/12` (sum rule). For intact
    to degraded transitions `EF = (1 - DG_ratio) * C_intact * 44/12`
    (product rule), so the intact stock is not counted twice.

4.  Emissions: `E = AD * EF` (product rule), annualised as in
    [`fct_combine_mcs_E()`](https://openforis.github.io/mocaredd/reference/fct_combine_mcs_E.md).

5.  Emissions per time period and REDD+ activity (sum rule).

6.  Reference and monitoring levels: length-weighted mean of the annual
    emissions of their time periods (sum rule).

7.  Emission reductions: `ER = FREL - E_MON` (sum rule).

Approach 1 assumes all combined terms are independent. Correlations from
shared inputs (the carbon fraction, a carbon stock used by several
transitions, the same emission factors in the reference and monitoring
periods) are ignored; the Monte Carlo simulations (Approach 2) account
for them.

## Usage

``` r
fct_arithmetic_mean3(.checked_data)
```

## Arguments

- .checked_data:

  The list returned by
  [`fct_checkinput()`](https://openforis.github.io/mocaredd/reference/fct_checkinput.md).

## Value

A list with: `ER` (reference / monitoring levels and emission reductions
with mean, se, U\\ `gg_emissions` (figure), `emissions_table` (tidy
table for the app), `c_stock` (carbon stock per land use) and `trans`
(EF and E per transition). U\\ `setup$conf_level`.

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

res <- fct_arithmetic_mean3(.checked_data = checked)
res$c_stock
#> # A tibble: 5 × 6
#>   c_lu_id            c_form              c_form_U                  C  C_se   C_U
#>   <chr>              <chr>               <chr>                 <dbl> <dbl> <dbl>
#> 1 evergreen          AGB * (1 + RS) * CF AGB * (1 + RS) * CF … 172.   45.4  43.3
#> 2 mountain           AGB * (1 + RS) * CF AGB * (1 + RS) * CF … 129.   32.1  40.8
#> 3 cropland           ALL                 ALL_se                  0     0    NA  
#> 4 evergreen_degraded DG_ratio * C_intact DG_ratio * C_intact … 124.   43.0  57.0
#> 5 mountain_degraded  DG_ratio * C_intact DG_ratio * C_intact …  93.1  35.4  62.4
res$ER
#> # A tibble: 3 × 6
#>   period_type         E     E_se   E_U  E_lower   E_upper
#>   <chr>           <dbl>    <dbl> <dbl>    <dbl>     <dbl>
#> 1 REF         21874156. 7680506.  57.8 9240847. 34507464.
#> 2 E-MON1       7925065. 2690695.  55.8 3499266. 12350864.
#> 3 ER-MON1     13949091. 8138183.  96.0  562972. 27335210.
```
