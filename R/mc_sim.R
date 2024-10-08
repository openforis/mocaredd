
## Process
## 1. decision tree of approach to carbon accounting
##    1.1. base of sims is land use transitions and associated REDD+ activity
##    1.2 AD per land use transition is key
##    1.3 EF is based on different formulas:
##          EF = (AGB_i - AGB_f) * (1 + RS) * CF * 44/12 (ex. Nepal)
##          EF = (C_i - C_f) * 44/12 with C = C_ag + C_bg + C_dw + C_l + SOC (ex. Ghana)
##          General formula would be
##          EF = (C_i - C_f) * 44/12 with
##          C = AGB * CF_ag + BGB * CF_bg + DW * CF_dw + L * CF_l + SOC



## USER INPUTS
use_truncated_pdf <- TRUE
n_iter <- 10000
ran_seed <- 93
usr_c_unit <- "C"


list.files("inst/extdata")

.path <- "inst/extdata/MCREDD_template_Ghana_simplified.xlsx"

## Check extension to be able to read
## IN SHINY Force XLSX FILE


## V1.0 file contains c-stock and AD tables
.ad <- ad <- readxl::read_xlsx(.path, sheet = "lu_transitions", na = "NA")
.cs <- cs <- readxl::read_xlsx(.path, sheet = "c_stock", na = "NA")


## Remove NAs
.cs <- cs <- cs |> filter(!is.na(c_value))



## Check the data
check_data <- function(.ad, .cs){

  ## Unique IDs
  flag_trans_id <- length(unique(.ad$trans_id)) == nrow(.ad)
  flag_c_id     <- length(unique(.cs$c_id)) == nrow(.cs)

  ## .cs conformity
  flag_pool <- unique(.cs$c_pool) %in% c("AGB", "BGB", "DW", "LI", "SOC", "ALL") |> all()

  ## V1.0, can only handle one type of unit
  # flag_unit <- unique(.cs$c_unit) %in% c("DM", "C", "CO2") |> all()

  ## AD conformity
  flag_acti <- unique(.ad$redd_activity) %in% c("DF", "DG", "E", "E_AF", "E_RE") |> all()

  ## Matching landuse
  flag_lu_no <- length(unique(c(.ad$lu_initial_id, .ad$lu_final_id))) == length(unique(.cs$lu_id))

flag_all <- c(flag_trans_id, flag_c_id, flag_pool, flag_unit, flag_acti, flag_lu_no)
names(flag_all) <- c( "flag_trans_id", "flag_c_id", "flag_pool", "flag_unit", "flag_acti", "flag_lu_no")

flag_all

}

## RUN
flag_all <- check_data(.ad = ad, .cs = cs)
flag_all
all(flag_all)


## New function for MCS
mc_sim <- function(.ad, .cs){

  n_trans <- nrow(.ad)
  c_pool  <- sort(unique(.cs$c_pool))

  mcs_trans <- map(vec_trans_id, function(x){

    redd_activity <- .ad$redd_activity[x]
    trans_id      <- .ad$trans_id[x]

    LU_init  <- .ad$lu_initial_id[x]
    LU_final <- .ad$lu_final_id[x]

    AD_mean <- .ad$trans_area[x]
    AD_se   <- .ad$trans_se[x]




    c_init <- .cs |> filter(lu_id == LU_init)
    c_pool_init <- c_init |> pull(c_pool) |> sort()

    c_init_has_AG <- if ("AGB" %in% c_pool_init) T else F
    c_init_has_BG <- if ("BGB" %in% c_pool_init) T else F
    c_init_has_RS <- if ("RS"  %in% c_pool_init) T else F
    c_init_has_DW <- if ("DW" %in% c_pool_init)  T else F
    c_init_has_LI <- if ("LI" %in% c_pool_init)  T else F
    c_init_has_SO <- if ("SOC" %in% c_pool_init) T else F
    c_init_has_AL <- if ("ALL" %in% c_pool_init) T else F
    c_init_has_CF <- if ("CF" %in% c_pool_init)  T else F

    ## Checks
    if(c_init_has_BGB & c_init_has_RS) {
      c_init_has_RS <- F
      message("trans_id: ", trans_id, ", has both BGB and RS in the data, using BGB only.")
    }

    if(usr_c_unit == "DM" & !c_init_has_CF) {
      flag_miss_CF <- F
      message("Cstock unit is dry matter (DM) but no carbon fraction provided")
    } else {
      flag_miss_CF <- T
    }

    ## Get PDF parameters
    if (c_init_has_AG) {
      AGB_i_mean <- c_init |> filter(c_pool == "AGB") |> pull(c_value)
      AGB_i_se   <- c_init |> filter(c_pool == "AGB") |> pull(c_se)
    } else {
      AGB_i_mean <- 0
      AGB_i_se   <- 0
    }

    if (c_init_has_BG){
      BGB_i_mean <- c_init |> filter(c_pool == "BGB") |> pull(c_value)
      BGB_i_se   <- c_init |> filter(c_pool == "BGB") |> pull(c_se)
    } else {
      BGB_i_mean <- 0
      BGB_i_se   <- 0
    }

    if (c_init_has_RS){
      RS_i_mean <- c_init |> filter(c_pool == "RS") |> pull(c_value)
      RS_i_se   <- c_init |> filter(c_pool == "RS") |> pull(c_se)
    } else {
      RS_i_mean <- 0
      RS_i_se   <- 0
    }

    if (c_init_has_DW){
      DW_i_mean <- c_init |> filter(c_pool == "DW") |> pull(c_value)
      DW_i_se   <- c_init |> filter(c_pool == "DW") |> pull(c_se)
    } else {
      DW_i_mean <- 0
      DW_i_se   <- 0
    }

    if (c_init_has_LI){
      LI_i_mean <- c_init |> filter(c_pool == "LI") |> pull(c_value)
      LI_i_se   <- c_init |> filter(c_pool == "LI") |> pull(c_se)
    } else {
      LI_i_mean <- 0
      LI_i_se   <- 0
    }

    if (c_init_has_SO){
      SOC_i_mean <- c_init |> filter(c_pool == "SOC") |> pull(c_value)
      SOC_i_se   <- c_init |> filter(c_pool == "SOC") |> pull(c_se)
    } else {
      SOC_i_mean <- 0
      SOC_i_se   <- 0
    }

    if (c_init_has_AL){
      ALL_i_mean <- c_init |> filter(c_pool == "ALL") |> pull(c_value)
      ALL_i_se   <- c_init |> filter(c_pool == "ALL") |> pull(c_se)
    } else {
      ALL_i_mean <- 0
      ALL_i_se   <- 0
    }

    if (c_init_has_CF){
      CF_i_mean <- c_init |> filter(c_pool == "CF") |> pull(c_value)
      CF_i_se   <- c_init |> filter(c_pool == "CF") |> pull(c_se)
    } else {
      CF_i_mean <- 0
      CF_i_se   <- 0
    }


    ## Simulations
    set.seed(93)
    AD    <- rnorm(n = n_iter, mean = AD_mean   , sd = AD_se)
    AGB_i <- rnorm(n = n_iter, mean = AGB_i_mean, sd = AGB_i_se)
    AGB_f <- rnorm(n = n_iter, mean = AGB_f_mean, sd = AGB_f_se)
    RS    <- rnorm(n = n_iter, mean = RS_mean   , sd = RS_se)
    CF    <- rnorm(n = n_iter, mean = RS_mean   , sd = CF_se)

    sim <- cbind(AD, AGB_i, AGB_f, RS, CF) |>
      as_tibble() |>
      mutate(
        E = AD * (AGB_i - AGB_f) * (1 + RS) * CF * 44/12,
        trans_id = trans_id,
        redd_activity = redd_activity,
        sim_no = 1:n_iter
      ) |>
      select(sim_no, redd_activity, trans_id, everything())

    # hist(sim$E)
    E_median <- median(sim$E)
    E_ci <- (quantile(sim$E, 0.95) - quantile(sim$E, 0.05)) / 2
    E_ciperc <- round(E_ci / E_median * 100, 0)
    res <- paste0(round(E_median, 0), " +/- ", E_ciperc, "%")
    print(paste0(trans_id, ": ", res))

    sim

  }) |> list_rbind()
  #

  U_redd <- U_all |>
    group_by(redd_activity, sim_no) |>
    summarise(E_redd = sum(E), .groups = "drop") |>
    mutate(E_redd_mean = E_redd / RP)



}



