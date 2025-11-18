# Run Monte Carlo for REDD+ uncertainty's shiny app

Starts a R Shiny application that guide users to fill in context and
land use changes for forest related greenhouse gas emissions and
removals. Inputs include distribution parameters for various
distribution then the app returns error propagation and sensitivity
analysis of the overall uncertainty for GHG estimates.

## Usage

``` r
shiny_run_mocaredd(...)
```

## Arguments

- ...:

  arguments to pass to shinyApp

## Examples

``` r
if (interactive()) {

shiny_run_mocaredd()

}
```
