# Make a forest plot to show Monte Carlo simulation results

Produce a [`gt::gt()`](https://gt.rstudio.com/reference/gt.html) table
with the median and confidence interval of Monte Carlo simulations for
each category of a level of the analysis (land use transition, REDD+
activity, time period) and a forest plot showing the median and CI
together.

## Usage

``` r
fct_forestplot(
  .data,
  .id,
  .value_ari = NULL,
  .value,
  .uperc,
  .cilower,
  .ciupper,
  .id_colname,
  .conflevel,
  .filename = NA
)
```

## Arguments

- .data:

  a data frame containing the simulation aggregated results

- .id:

  the name of the ID column of the data frame

- .value_ari:

  NULL if not relevant (most cases) or a column name with the arithmetic
  mean of Emission or emission reductions.

- .value:

  the name of the value column of the data frame (i.e median of the
  simulations)

- .uperc:

  the name of the percentage uncertainty of the simulations

- .cilower:

  the name of the lower end of confidence interval

- .ciupper:

  the name of the higher end of the confidence interval

- .id_colname:

  character text for the ID column names. goes to
  [`gt::md()`](https://gt.rstudio.com/reference/md.html) in
  [`gt::cols_label()`](https://gt.rstudio.com/reference/cols_label.html)

- .conflevel:

  character text to specify what confidence level was used, ex. "90%".

- .filename:

  path to save the table or NA.

## Value

A character value with the formula for calculating total carbon stock
for a specific land use.
