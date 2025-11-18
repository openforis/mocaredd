# Generate Monte Carlo Simulations for a given PDF

TBD

## Usage

``` r
fct_make_mcs(
  .n_iter = 10000,
  .pdf,
  .mean = NA,
  .se = NA,
  .params = NULL,
  .trunc = FALSE
)
```

## Arguments

- .n_iter:

  Number of iterations of the Monte Carlo simulations

- .pdf:

  Probability Distribution Function name, supported distributions so
  far: "normal", "beta".

- .mean:

  Mean value if PDF parameters are mean and sd

- .se:

  Standard deviation of the mean value if PDF parameters are mean and sd

- .params:

  Vector pf parameters for other PDFs (NOT IMPLEMENTED YET)

- .trunc:

  TRUE or FALSE, should simulations be truncated to allow only values
  above 0 where negative values are impossible

## Value

a vector of N simulation with N, the number of iterations (.n_iter)

## Examples

``` r
library(mocaredd)

tt <- fct_make_mcs(.pdf = "normal", .mean = 0, .se = 1)
hist(tt)

```
