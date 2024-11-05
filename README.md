<!-- badges: start -->
[![R-CMD-check](https://github.com/openforis/mocaredd/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/openforis/mocaredd/actions/workflows/R-CMD-check.yaml)
[![R-CMD-check](https://github.com/gaelso/mocaredd/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/gaelso/mocaredd/actions/workflows/R-CMD-check.yaml)
<!-- badges: end -->


# {mocaredd}:  Monte Carlo Simulations for REDD+ uncertainty analysis

Contains functions and a shiny app to run Monte Carlo Simulations for producing improved estimates of and confidence intervals around greenhouse gas emissions and emission reductions from the forestry sector.

The apps takes an input XLSX file following a specific template meant to harmonize reporting of areas of land use transitions and associated carbon stocks. It then provides simulations, medians, percentage uncertainties and confidence intervals at various stages of aggregation from emissions related land use transitions to REDD+ activities and emission reductions between reference and monitoring periods.

\  

### Workflow

1. Input XSLX filled following provided template. (Note for v2.0, accept separated CSV files, other format tbd).
1. Checks are run to ensure the entity codes are unique and matching between tables. 
1. Simulations are produced and results can be seen as forest plots, simulations densities and comparison between simulations and arithmetic means for the final emission reductions.
1. Sensitivity analysis shows the impact of AE vs EF, REDD+ activities, time periods.


### Calculation steps

TBD

### Template

(ADD Link to template from github here)

The template includes 4 tabs:

#### User inputs (user_inputs)
They help offering great flexibility with a minimum number of parameters.

- **trunc_pdf**: use truncated PDF to avoid negative simulated values were illogical (for ex. areas.). (Note for v2.0, currently only works for normal distributions).
- **n_iter**: number of iterations. Most standards require 10,000 simulations.
- **ran_seed**: rand seed for the simulation. If note specified, the first run sets a random seed and reports it for reproducing the same results. 
- Carbon stocks are reported following many different methods and pools, to allow flexibility the next user inputs provide ways to customize what is reported and how. 
    - **c_unit**: for AGB abd BGB, is the unit dry matter (DM) or carbon (C)?
    - **dg_pool**: If degradation is a ratio of intact forest Cstock, list of pools separated by coma to which the ratio applies.
    - **dg_expool**: (experimental) Pools that are excluded from degradation process (may be redundant).
- **ad_annual**: Is Activity Data reported annual or as a sum over the time period considered? (TRUE or FALSE).
- **conf_level**: Confidence level (1 - alpha) for the uncertainty calculations.

#### Time periods (time_periods)

TBD

#### land use transitions (AD_lu_transitions)

TBD

#### Carbon stocks (c_stocks)

TBD


### Road map of future developements

1. load input as csv 
1. Include various soil organic carbon method
1. Include removals

