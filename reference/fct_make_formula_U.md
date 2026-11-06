# Make the carbon stock uncertainty formula of a land use (IPCC Approach 1)

Companion of
[`fct_make_formula2()`](https://openforis.github.io/mocaredd/reference/fct_make_formula2.md):
same inputs, returns the formula of the standard error of the carbon
stock, built from the terms of
[`fct_make_formula2()`](https://openforis.github.io/mocaredd/reference/fct_make_formula2.md)
with the IPCC Approach 1 rules (IPCC 2006, Vol. 1, Ch. 3, Eq. 3.1 and
3.2): - product: `x * y * sqrt((x_se / x)^2 + (y_se / y)^2)`, - sum:
`sqrt(x_se^2 + y_se^2)`.

             Composite factors are resolved first:
             - \code{(1 + RS)}: standard error \code{RS_se},
             - \code{(AGB + BGB)}: standard error \code{sqrt(AGB_se^2 + BGB_se^2)}.

             Evaluate it with each element and its standard error as
             \code{<element>} and \code{<element>_se} (e.g. \code{AGB}, \code{AGB_se},
             \code{CF}, \code{CF_se}). Terms are assumed independent. A factor
             equal to 0 in a product returns NaN.

## Usage

``` r
fct_make_formula_U(.c_el, .c_unit, .version = 1)
```

## Arguments

- .c_el:

  Vector of carbon elements, among "AGB", "BGB", "RS", "DW", "LI",
  "SOC", "ALL". Other elements (e.g. "DG_ratio") are ignored.

- .c_unit:

  Carbon unit. `.version = 1`: a single value ("DM" or "C") for the land
  use; CF applies to biomass (AGB, BGB) only. `.version = 2`: a vector
  aligned with `.c_el`, one unit per element; CF applies to each element
  in DM. RS takes the unit of AGB.

- .version:

  Template version, 1 (default) or 2.

## Value

A character value with the standard error formula of the carbon stock.

## Examples

``` r
fct_make_formula_U(.c_el = c("AGB", "RS"), .c_unit = "DM")
#> [1] "AGB * (1 + RS) * CF * sqrt((AGB_se / AGB)^2 + (RS_se / (1 + RS))^2 + (CF_se / CF)^2)"
#> "AGB * (1 + RS) * CF * sqrt((AGB_se / AGB)^2 + (RS_se / (1 + RS))^2 + (CF_se / CF)^2)"

fct_make_formula_U(.c_el = c("AGB", "RS", "DW"), .c_unit = c("DM", NA, "C"), .version = 2)
#> [1] "sqrt((AGB * (1 + RS) * CF * sqrt((AGB_se / AGB)^2 + (RS_se / (1 + RS))^2 + (CF_se / CF)^2))^2 + (DW_se)^2)"

## Evaluate
env <- list(AGB = 250, AGB_se = 25, RS = 0.24, RS_se = 0.05, CF = 0.47, CF_se = 0.02)
eval(parse(text = fct_make_formula_U(c("AGB", "RS"), "DM")), env)
#> [1] 16.88907
```
