<!-- badges: start -->
[![R-CMD-check](https://github.com/openforis/mocaredd/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/openforis/mocaredd/actions/workflows/R-CMD-check.yaml)
<!-- badges: end -->


# {mocaredd}:  Monte Carlo Simulations for REDD+ uncertainty analysis


Monte Carlo simulations for estimating the uncertainty of REDD+ greenhouse gas emissions, removals and emission reduction from forest changes.

\  

## Workflow:

1. Input Monte Carlo model input variables and their characteristics (or raw data and aggregation factors, see v2.0 roadmap)
1. Chose number of repetitions
1. Choose or add custom formulas between input variables.
1. Run Simulations

The app simulate the desired number of repetitions and calculate uncertainty of calculated variables. 


## Template

input_category / input_name / input_error_type / input_distribution / fixed_value / normal_mu / normal_sigma / others 



## Input options

### Option 1: Input a list of variables and their distribution characteristics:

Ex. 

var 1 / normal / mu / sigma / 
var 2 / normal / mu / sigma /
var 3 / fixed  / mu /     0 /

### Option 2: Input raw data and distribution automatically assigned (for v2.0, distrubution might be better using biological characteristics than stats)

### Option 3: Raw data, include tree AGB allometric equations uncertainty


## Road map 

TBD
