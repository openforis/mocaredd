# Welcome to {mocaredd} v1.0!

### Monte Carlo Simulations for REDD+ uncertainty analysis (mocaredd) is a free and opensource application that aims at making simulation-based uncertainty calculations for REDD+ easier to process, report and verify.

It works by combining the strengths of spreadsheet tools (XLSX) for
templating and a statistical language (R) for running the simulations,
aggregating results and producing helpful visuals.

## What for?

In the context of the REDD+ mechanism (Reducing emission from
Deforestation and forest Degradation “plus”), countries can receive
result-based payments for emission reductions and removal increases of
greenhouse gas (GHG) from the forestry sector. Emission reductions are
estimated by quantifying the amount of greenhouse gas emissions between
a reference level (i.e baseline) and a monitorig period. These estimates
come with a level of uncertainty due to the sampling methods and models
used to generate the estimations.

High integrity REDD+ standards, that set rules for the quantification of
emissions and removals, and their uncertainties, increasingly require
the use of Monte Carlo simulations for REDD+ uncertainties. These
simulations are often run in spreadsheet tools, but the lack of
structure and inherent disadvantage of spreadsheets for this task (error
prone, difficulty to handle large amount of data) is becoming a barrier
for the quality, reproducibility and verification of uncertainties based
on simulations.

{mocaredd} provides a template for organizing data, and a tool that
takes the data, runs Monte Carlo Simulations and produces improved
estimates of and confidence intervals around greenhouse gas emissions
and emission reductions from the forestry sector (REDD+).

## How to run the application ?

### Online app

The application is available online (read-to-use) at:

- <https://openforis-shiny.shinyapps.io/mocaredd/>

### offline / local installation

It can also be installed for offline use (require installing R and
Rstudio, may require administrator permission). The app require R
version 3.4 minimum. RStudio is recommended as Integrated Development
Environment. With Windows machines, Rtools should also be installed:

- [https://cloud.r-project.org/](https://openforis.github.io/mocaredd/)
- [https://posit.co/download/rstudio-desktop/](https://openforis.github.io/mocaredd/)
- [https://cran.r-project.org/bin/windows/Rtools/](https://openforis.github.io/mocaredd/)

R and Rstudio are installed, open R studio and run the folloing code in
the terminal:

    if (!require(remotes)) install.packages("remotes")
    if (!require(mocaredd)) remotes::install_github("openforis/mocaredd@v1.0")

    mocaredd::shiny_run_mocaredd()

## Resources

- Online app: <https://openforis-shiny.shinyapps.io/mocaredd/>

- Online documentation: <https://openforis.github.io/mocaredd/>

- Template guidelines:
  <https://openforis.github.io/mocaredd/articles/tuto-template.html>

  - Simple template (2 time periods, 12 land use changes classes, 2
    carbon pools, inc. simulations in Excel for comparison):
    [Example2-with-sims.xlsx](https://docs.google.com/spreadsheets/d/19ySu_L4aI3tepq9FbYksbBa0yNi4s49c/edit?usp=sharing&ouid=104053208207168605668&rtpof=true&sd=true)

  - Intermediate template (4 time periods, 48 land use change classes, 4
    carbon pools):
    [Example1-4pools.xlsx](https://docs.google.com/spreadsheets/d/1H9yfPCSfSELgW506OIHqS-15hJtTECHB/edit?usp=sharing&ouid=104053208207168605668&rtpof=true&sd=true)

- Github repository: <https://github.com/openforis/mocaredd>

- Contact form: [Contact us](https://forms.gle/YZy4xmviSMvUT8DJ9)
