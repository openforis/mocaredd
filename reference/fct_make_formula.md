# Check that the app's the input data has matching land uses and correct carbon pools and associated factors.

The expected input carbon stock table for the tool contains carbon
values of different carbon pools coded: - AGB for aboveground biomass, -
BGB or RS (Root-to-Shoot ratio) for the belowground biomass, - DW for
deadwood, - LI for litter and - SOC for soil organic carbon. All pools
can be expressed in tons of carbon (C), except AGB and BGB which can
also be expressed in ton of dry matter (DM) if a carbon fraction 'CF' is
provided.

## Usage

``` r
fct_make_formula(.c_el, .c_unit)
```

## Arguments

- .c_el:

  Vector of carbon elements, inc. "A::

- .c_unit:

  "DM" or "C",

## Value

A character value with the formula for calculating total carbon stock.

## Examples

``` r
library(mocaredd)

c_el <- c("AGB", "RS", "DW")

fct_make_formula(.c_el = c_el, .c_unit = "DM")
#> [1] "(AGB + AGB * RS) * CF + DW"
```
