# Make the carbon stock formula of a land use from its carbon elements

The biomass part is written so each input appears once: - AGB + RS:
`AGB * (1 + RS)` (BGB derived from AGB), - AGB + BGB: `(AGB + BGB)`,
multiplied by `CF` when biomass is in dry matter (DM). DW, LI and SOC
are added as separate terms.

             The factored form makes the AGB/BGB dependence explicit, which
             matters when uncertainty is propagated term by term with IPCC
             Approach 1 rules.

## Usage

``` r
fct_make_formula2(.c_el, .c_unit, .version = 1)
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

A character value with the carbon stock formula.

## Examples

``` r
fct_make_formula2(.c_el = c("AGB", "RS", "DW"), .c_unit = "DM")
#> [1] "AGB * (1 + RS) * CF + DW"
#> "AGB * (1 + RS) * CF + DW"

fct_make_formula2(.c_el = c("AGB", "RS", "DW"), .c_unit = c("DM", NA, "C"), .version = 2)
#> [1] "AGB * (1 + RS) * CF + DW"
#> "AGB * (1 + RS) * CF + DW"
```
