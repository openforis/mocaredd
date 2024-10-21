
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

## Remove NAs (to be added to actions on XLSX upload)
cs <- cs |> filter(!(is.na(c_value) & is.na(c_pdf_a)))

## IN SHINY: Initiation lists
init <- list(
  c_pools = c("AGB", "BGB", "RS", "DW", "LI", "SOC", "ALL", "DG_ratio"),
  redd_acti = c("DF", "DG", "EN", "EN_AF", "EN_RE")
)

## Load functions
ls_f <- list.files("R", pattern = "fct_", full.names = T)
walk(ls_f, function(x){ source(x, local = F) })

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

## Test fct_combine_mcs_all()

res <- fct_combine_mcs_all(.ad = ad, .cs = cs, .init = init, .usr = usr)

## What next
## Trans to redd acti x time period

res_redd <- res |>
  group_by(sim_no, redd_activity, time_period) |>
  summarize(E_redd = sum(E), .groups = "drop")




