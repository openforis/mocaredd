
library(tidyverse)
library(readxl)

## IN SHINY FORCE XLSX FILE
.path <- "inst/extdata/example1.xlsx"

## LOAD DATA
## IN SHINY: inputFile, Force XSLX or offer CSV (to be discussed)
## V1.0 file contains C Stock, AD, time period and user input tables
ad   <- readxl::read_xlsx(.path, sheet = "AD_lu_transitions", na = "NA")
cs   <- readxl::read_xlsx(.path, sheet = "c_stock", na = "NA")
usr  <- readxl::read_xlsx(.path, sheet = "user_inputs", na = "NA")
time <- readxl::read_xlsx(.path, sheet = "time_periods", na = "NA")

# ## Get user inputs from Shiny
# .usr <- usr <- list(
#   trunc_pdf = TRUE,
#   n_iter    = 10000,
#   ran_seed  = 93,
#   c_unit    = "C"
# )

##
## INITAL CALCULATIONS ######
##

## Remove NAs (to be added to actions on XLSX upload)
cs <- cs |> filter(!(is.na(c_value) & is.na(c_pdf_a)))

## Calculate how many years for each time period
time <- time |> mutate(nb_years = year_end - year_start + 1)

## alpha
ci_alpha <- 1 - usr$conf_level

##
## IN SHINY: Initiation lists ######
##

init <- list(
  c_pools = c("AGB", "BGB", "RS", "DW", "LI", "SOC", "ALL", "DG_ratio"),
  redd_acti = c("DF", "DG", "EN", "EN_AF", "EN_RE")
)

##
## Load functions ######
##

ls_f <- list.files("R", pattern = "fct_", full.names = T)
walk(ls_f, function(x){ source(x, local = F) })

##
## TEST FUNCTIONS ######
##

## !! FOR TESTING INSIDE FUNCTIONS ONLY
.ad <- ad
.cs <- cs
.usr <- usr
.time <- time
.c_lu <- cs |> filter(lu_id == "dg_ev_wet_closed")
## !!

## test fct_check_data()
flag_all <- fct_check_data(.ad = ad, .cs = cs, .init = init)
message("All checks passed: ",all(flag_all))

## test fct_check_pool() and fct_make_formula()
c_lu <- cs |> filter(lu_id == "ev_wet_closed")

c_check <- fct_check_pool(.c_lu = c_lu, .c_unit = usr$c_unit, .c_fraction = usr$c_fraction)
c_form  <- fct_make_formula(.c_check = c_check, .c_unit = usr$c_unit)


## test fct_make_mcs()
c_sims <- c_lu |> filter(c_pool == "AGB")

sims <- fct_make_mcs(.n_iter = 10000, .pdf = c_sims$c_pdf, .mean = c_sims$c_value, .se = c_sims$c_se, .trunc = F)
hist(sims)
median(sims)
c_sims$c_value

## test fct_combine_mcs_cstock()
res <- fct_combine_mcs_cstock(.n_iter = 10000, .c_sub = c_lu, .c_unit = "C", .c_fraction = NA)

check_cstock <- res |>
  mutate(
    C_all_check = eval(parse(text = c_form), res),
    flag_cstock = C_all_check == C_all
    )

message("CSTOCK sims working: ", all(check_cstock$flag_cstock))


## Test fct_combine_mcs_all()
sim_trans <- fct_combine_mcs_all(.ad = ad, .cs = cs, .init = init, .usr = usr)

res_trans <- sim_trans |>
  group_by(time_period, redd_activity, trans_id) |>
  summarise(
    E = round(median(E_trans)),
    E_ciupper = quantile(E_trans, 1 - ci_alpha/2),
    E_cilower = quantile(E_trans, ci_alpha/2),
    .groups = "drop"
  ) |>
  mutate(
    E_ME  = (E_ciupper - E_cilower) / 2,
    E_U   = round(E_ME / E * 100),
  ) |>
  select(time_period, redd_activity, trans_id, E, E_U, E_ciupper, E_cilower, E_ME)

res_trans

write_csv(res_trans, "tests/res_trans.csv")


##
## TEST NO FUNCTION
##

## trans to redd+ acti
sim_redd <- sim_trans |>
  group_by(sim_no, redd_activity, time_period) |>
  summarize(E_redd = sum(E), .groups = "drop")

sim_redd |> filter(sim_no == 1)

res_redd <-

## redd+ acti to time period
sim_period <- sim_redd |>
  group_by(sim_no, time_period) |>
  summarise(E_period = sum(E_redd), .groups = "drop")

res_period |> filter(sim_no == 1)

## aggregate redd+ periods for the reference level
time_ref   <- time |> filter(period_type == "reference")
nb_ref     <- length(unique(time_ref$period_combinations))
length_ref <- sum(time_ref$nb_years)

if (nrow(time_ref) == 1) {

  ## Extract the sims of the period if only one period
  FREL_SIMS <- res_period |>
    filter(time_period == time_ref$period_no) |>
    select(sim_no, E = E_period)

} else if (nrow(time_ref) > 1 & usr$ad_annual) {

  ## Weighted average of the sims from the reference sub-periods
  ## Get the volume per period then divide by total length of reference period
  FREL_SIMS <- res_period |>
    filter(time_period %in% time_ref$period_no) |>
    left_join(time_ref, by = join_by(time_period == period_no)) |>
    group_by(sim_no) |>
    summarise(E = sum(E_period * nb_years) / length_ref, .groups = "drop")


} else if (nrow(time_ref) > 1 & !usr$ad_annual) {
  ## Divide the volume of E over the reference period by the total length of the reference period
  FREL_SIMS <- res_period |>
    filter(time_period %in% time_ref$period_no) |>
    group_by(sim_no) |>
    summarise(E = sum(E_period) / length_ref, .groups = "drop")

} else {

  ## Something wrong
  return("Issues while aggregating Emissions over sub-periods.")

}


SIMS <- FREL_SIMS$E

FREL <- round(median(SIMS))
FREL

FREL_ME <- as.numeric((quantile(SIMS, 1 - ci_alpha/2) - quantile(SIMS, ci_alpha/2)) / 2)
FREL_U  <- round(FREL_ME / FREL * 100, 0)

message("FREL is: ", FREL, " ± ", FREL_U, "%")

## Arithmetic mean
ad2 <- ad |> mutate(trans_se = 0)
cs2 <- cs |> mutate(c_se = 0)
res_trans <- fct_combine_mcs_all(.ad = ad2, .cs = cs2, .init = init, .usr = usr)



##
## Monitoring ######
##

time_mon <- time |> filter(period_type == "monitoring")
nb_ref <- length(unique(time_mon$period_combinations))

res_res <- map(time_mon$period_no, function(x){

  ## !! FOR TESTING ONLY
  # x = "T2"
  ## !!

  if (usr$ad_annual) {
    nb_years <- time |> filter(period_no == x) |> pull(nb_years)
  } else {
    nb_years <- 1
  }

  res_p <- res_redd |>
    filter(time_period == x) |>
    group_by(sim_no) |>
    summarise(E_ref = sum(E_redd) / nb_years) |>
    mutate(period_no = x) |>
    left_join(time, by = join_by(period_no))

  res_p

}) |> list_rbind()

res_res

## Combine multiple periods used for one reference period
if (nrow(time_mon) > 1) {

  ref_length <- sum(time_ref$nb_years)

  res_res2 <- res_res |>
    group_by(sim_no) |>
    summarise(E_ref_total = sum(E_ref * nb_years) / ref_length)

} else {

  res_ref2 <- res_ref

}

E <- res_ref2$E_ref_total

FREL <- round(median(E))
FREL

FREL_ME <- as.numeric((quantile(E, 1 - ci_alpha/2) - quantile(E, ci_alpha/2)) / 2)
FREL_U  <- round(FREL_ME / FREL * 100, 0)

message("FREL is: ", FREL, " ± ", FREL_U, "%")
