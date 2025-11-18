<!-- badges: start -->

[![R-CMD-check](https://github.com/openforis/mocaredd/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/openforis/mocaredd/actions/workflows/R-CMD-check.yaml)

<!-- badges: end -->

# Welcome to {mocaredd} v1.0!

### Monte Carlo Simulations for REDD+ uncertainty analysis (mocaredd) is a free and opensource application that aims at making simulation-based uncertainty calculations for REDD+ easier to process, report and verify.

It works by combining the strengths of spreadsheet tools (XLSX) for templating and a statistical language (R) for running the simulations, aggregating results and producing helpful visuals.

<!-- new section -->

## What for?

In the context of the REDD+ mechanism (Reducing emission from Deforestation and forest Degradation "plus"), countries can receive result-based payments for emission reductions and removal increases of greenhouse gas (GHG) from the forestry sector. Emission reductions are estimated by quantifying the amount of greenhouse gas emissions between a reference level (i.e baseline) and a monitorig period. These estimates come with a level of uncertainty due to the sampling methods and models used to generate the estimations.

High integrity REDD+ standards, that set rules for the quantification of emissions and removals, and their uncertainties, increasingly require the use of Monte Carlo simulations for REDD+ uncertainties. These simulations are often run in spreadsheet tools, but the lack of structure and inherent disadvantage of spreadsheets for this task (error prone, difficulty to handle large amount of data) is becoming a barrier for the quality, reproducibility and verification of uncertainties based on simulations.

{mocaredd} provides a template for organizing data, and a tool that takes the data, runs Monte Carlo Simulations and produces improved estimates of and confidence intervals around greenhouse gas emissions and emission reductions from the forestry sector (REDD+).

<!-- new section -->

## How to run the application ?

The application is available online (read-to-use) at: <https://openforis-shiny.shinyapps.io/mocaredd/>

It can also be installed for offline use (require installing R and Rstudio, may require administrator permission). The app require R version 3.4 minimum. RStudio is recommended as Integrated Development Environment. With Windows machines, Rtools should also be installed:

-   [https://cloud.r-project.org/]()
-   [https://posit.co/download/rstudio-desktop/]()
-   [https://cran.r-project.org/bin/windows/Rtools/]()

R and Rstudio are installed, open R studio and run the folloing code in the terminal:

```         
if (!require(remotes)) install.packages("remotes")
if (!require(mocaredd)) remotes::install_github("openforis/mocaredd@v1.0")

mocaredd::shiny_run_mocaredd()
```

<!-- new section -->

## Step-by-Step app workflow

1.  The app provides a template spreadsheet to download, either directly in the app or here (see Resources below):

2.  The user fills in the spreadsheet 4 tabs:

    1.  **User inputs** (user_inputs). Helps offering great flexibility with a minimum number of parameters.

    2.  **Time periods** (time_periods). Describes the times periods covered by the data and how they are combined into reference or reporting.

    3.  **Land use transitions** (AD_lu_transitions). Describes the probability density functions (PDFs) for the land use change areas for each time period

    4.  **Carbon stocks** (c_stocks). Contains the probability density functions for all the carbon elements necessary to associate emission or removal factors to all land use changes and time periods.

3.  The user upload the filled template into the tool.

4.  The app run checks:

    1.  verifies that all connections between time periods, land use changes and carbon elements are correctly entered,

    2.  provides an overview of the data (number of time periods, carbon pools, method for forest degradation, etc.),

    3.  calculates the arithmetic mean of aggregated emission reductions (no simulation yet).

    4.  makes land use change matrices in ha.

5.  If all checks are passed, the app runs the simulations:

    1.  for each PDF, the determined number of simulations is randomly generated, then

    2.  the carbon accounting chain of calculations is created based on the data uploaded,

    3.  the simulations are aggregated to emissions per land use change, emissions and removals per REDD+ activity and time period, emissions reduction and increased removals for each monitoring period in the data.

    4.  The improved estimates and their confidence intervals are reported as the median and quantiles of the simulations at each stage of the aggregation.

6.  A sensitivity analysis is run to provide the respective contributions of REDD+ activities, activity data and emission factors, and time periods to the overall uncertainty.

<!-- new section -->

## Resources

-   Online app: <https://openforis-shiny.shinyapps.io/mocaredd/>

-   Online documentation: <https://openforis.github.io/mocaredd/>

-   Template guidelines: <https://openforis.github.io/mocaredd/articles/tuto-template.html>

    -   Template simple (2 time periods, 12 land use changes classes, 2 carbon pools, inc. simulations in Excel for comparison): [Example2-with-sims.xlsx](https://docs.google.com/spreadsheets/d/19ySu_L4aI3tepq9FbYksbBa0yNi4s49c/edit?usp=sharing&ouid=104053208207168605668&rtpof=true&sd=true)

    -   Template intermediate (4 time periods, 48 land use change classes, 4 carbon pools): [Example1-4pools.xlsx](https://docs.google.com/spreadsheets/d/1H9yfPCSfSELgW506OIHqS-15hJtTECHB/edit?usp=sharing&ouid=104053208207168605668&rtpof=true&sd=true)

-   Github repository: <https://github.com/openforis/mocaredd>

-   Contact form: [Contact us](https://forms.gle/YZy4xmviSMvUT8DJ9)

<!-- new section -->

## Planned features

-   Improve the template guidelines with examples from real cases.
-   Improve the sensitivity analysis.
-   Develop the template tab and calculations for annual processes (removals, soil organic carbon).
-   Develop the template and calculations for bootstrap from raw data.
-   Add truncated PDFs support.
-   Add IPCC tier 1 uncertainties propagation around the arithmetic means.

```{=html}
<!--

Current features:

-   Download a demo template with user inputs, time periods, land use change areas, carbon elements.
-   User filled template submission to the tool,
-   Verification of template integrity,
-   Build calculation chain from input data to greenhouse gas emission reductions (ER)
-   Calculate arithmetic mean ERs from input data
-   Run simulations for all variables and reproduce calculation chain for each simulation
-   Calculate the median of simulated ERs as a better estimate than the arithmetic mean
-   Calculate uncertainty as quantiles of simulations at the chosen level
-   Show medians and confidence intervals at intermediate level (REDD+ activities, activity data and emission factors)
-   Show sansitivity analysis

-->
```
