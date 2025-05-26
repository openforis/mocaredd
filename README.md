<!-- badges: start -->
[![R-CMD-check](https://github.com/openforis/mocaredd/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/openforis/mocaredd/actions/workflows/R-CMD-check.yaml)
<!-- badges: end -->


# {mocaredd}:  Monte Carlo Simulations for REDD+ uncertainty analysis

Contains functions and a shiny app for (1) setting up, (2) running Monte Carlo Simulations and (3) producing improved estimates of and confidence intervals around greenhouse gas emissions and emission reductions from the forestry sector.

The apps takes an input XLSX file following a specific template meant to harmonize reporting of areas of land use transitions and associated carbon stocks. It then provides simulations, medians, percentage uncertainties and confidence intervals at various stages of aggregation from emissions related land use transitions to REDD+ activities and emission reductions between reference and monitoring periods.

\  

### Run the app

The app require R version 3.4 minimum. RStudio is recommended as Integrated Development Environment.
With Windows machines, Rtools should also be installed:

- [https://cloud.r-project.org/]()
- [https://posit.co/download/rstudio-desktop/]()
- [https://cran.r-project.org/bin/windows/Rtools/]()

Install the package and run the app:

    if (!require(remotes)) install.packages("remotes")
    if (!require(mocaredd)) remotes::install_github("openforis/mocaredd")
    
    mocaredd::shiny_run_mocaredd()



### Workflow

1. Input XSLX file following provided template.
1. Checks are run to ensure the entity codes are unique and matching between tables. 
1. Simulations are produced and results can be seen as forest plots, simulations densities and comparison between simulations and arithmetic means for the final emission reductions.
1. Sensitivity analysis shows the impact of AE vs EF, REDD+ activities, time periods.


### Calculation steps

1. Check that the template is followed correctly and that the tables are correctly linked (keys link time to land use changes and land use changes to carbon stock elements)
2. Generate simulations for all probability distribution functions
3. Calculate carbon stocks based on carbon stock elements reported.
4. Calculate Emission factors for each land use change and time period
5. Calculate Emissions from IPCC formula Eij = EFij x ADij for each land use change i and time period j.
6. Aggregate Emissions per time period
7. Aggregate Emissions for the reference and monitoring periods
8. Calculate Emission Reductions as the difference between annualized emissions during the reference period and annualized emissions during each of the monitoring periods.


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
    - **c_fraction**: Carbon fraction to convert dry matter to carbon, if c_unit is "DM".
    - **c_fraction_se**: standard error of the carbon fraction
    - **c_fraction_pdf**: probability distribution function of the carbon fraction.
    - **dg_pool**: If degradation is a ratio of intact forest Cstock, list of pools separated by coma to which the ratio applies.
    - **dg_ext**: Extension used in the land use IDs for degraded land uses, for example if Evergreen forest is coded "EV" and degraded evergreen forest "EV_deg", dg_ext is "_deg". Necessary to automatically link intact forest carbon stock with its degradation ratio.
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

