# Generate histogram to show the simulations at any stage.

Takes simulation data from fct_combine_mcs_E() or fct_combine_mcs_P()
and aggregated results from fct_calc_res() and generate an histogram
showing the PDF density, modeled normal distribution, simulations'
median and confidence interval.

## Usage

``` r
fct_histogram(.data, .res, .id, .value, .value_type, ...)
```

## Arguments

- .data:

  data frame of simulated data. Output of fct_combine_mcs_E() or
  fct_combine_mcs_P().

- .res:

  aggregated results of a simulation. Output of fct_calc_res()

- .id:

  ID column of .data and .res inputs.

- .value:

  simulated values column in .data.

- .value_type:

  "E" or "ER" if .data represents emissions or emission reductions
  respectively.

- ...:

  filter query, passed to
  [`dplyr::filter()`](https://dplyr.tidyverse.org/reference/filter.html).

## Value

a ggplot or a named list of ggplots
