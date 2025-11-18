# Calculate median and confidence interval based on Monte Carlo Simulations

Monte Carlo simulations from other package functions generate a table
with one row = one simulation. fct_calc_res() takes the simulation table
and output the simulated variable's median and its confidence interval
for each of the selected category (land use transition, REDD+ activity
or time period).

## Usage

``` r
fct_calc_res(.data, .id, .sim, .ci_alpha)
```

## Arguments

- .data:

  a data frame containing the simulation aggregated results

- .id:

  the name of the ID column of the data frame

- .sim:

  the name of the column containing the simulated values

- .ci_alpha:

  alpha value for the confidence interval with confidence level = 1 -
  alpha.

## Value

A tibble with simulation results per category if ID column: estimated
mean, percentage uncertainty, margin of error, lower and upper bound of
confidence interval.

## Examples

``` r
## TBD
```
