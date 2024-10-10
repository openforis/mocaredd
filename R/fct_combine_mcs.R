#' Generate and propagate Monte Carlo Simulations based on a template input file.
#'
#' @description TBD
#'
#'
#' @param .ad Activity Data input table for the shiny app (AD_lu_transitions)
#' @param .cs Carbon Stock input table for the shiny app (c_stock)
#'
#' @return A list of dataframes with Monte Carlo simulations for input variables, REDD+ activities CO2 emissions
#'         and removals and emission reductions levels.
#'
#' @examples
#' library(mocaredd)
#' library(readxl)
#' library(dplyr)
#' library(ggplot2)
#'
#' cs <- read_xlsx(system.file("extdata/example1.xlsx", package = "mocaredd"), sheet = "c_stock", na = "NA")
#' ad <- read_xlsx(system.file("extdata/example1.xlsx", package = "mocaredd"), sheet = "AD_lu_transitions", na = "NA")
#'
#' res <- fct_combine_mcs(.ad = ad, .cs = cs)
#'
#' ## ADD HISTOGRAM FOR ONE SET OF SIMULATIONS
#'
#' @export
fct_combine_mcs <- function(.ad, .cs){

  ##
  ## !! FOR TESTING ONLY
  ##
  ## IN SHINY FORCE XLSX FILE
  .path <- "inst/extdata/example1.xlsx"

  ## LOAD DATA
  ## V1.0 file contains C Stock, AD, time period and user input tables
  .ad   <- ad   <- readxl::read_xlsx(.path, sheet = "AD_lu_transitions", na = "NA")
  .cs   <- cs   <- readxl::read_xlsx(.path, sheet = "c_stock", na = "NA")
  .usr  <- usr  <- readxl::read_xlsx(.path, sheet = "user_inputs", na = "NA")
  .time <- time <- readxl::read_xlsx(.path, sheet = "time_periods", na = "NA")

  # ## Get user inputs from Shiny
  # .usr <- usr <- list(
  #   trunc_pdf = TRUE,
  #   n_iter    = 10000,
  #   ran_seed  = 93,
  #   c_unit    = "C"
  # )

  ## Remove NAs
  .cs <- cs <- cs |> filter(!is.na(c_value))
  ##
  ## !!
  ##

  ## CHECK THE INPUT DATA CONFORMITY
  flag_all <- fct_check_data(.ad = ad, .cs = cs)

  n_trans <- nrow(.ad)
  # c_pool  <- sort(unique(.cs$c_pool))

  ## For each transition, calculate simulations for each element of the calculation chain
  mcs_trans <- map(1:n_trans, function(x){

    ## !! FOR TESTING ONLY
    x = 2

    ad_x <- .ad[x,]

    # redd_activity <- .ad$redd_activity[x]
    # trans_id      <- .ad$trans_id[x]
    #
    # LU_init  <- .ad$lu_initial_id[x]
    # LU_final <- .ad$lu_final_id[x]

    ## Seed for random simulation
    if (!is.na(usr$ran_seed)){
      set.seed(usr$ran_seed)
    } else {
      app_ran_seed <- sample(1:100, 1)
      set.seed(app_ran_seed)
      message("Seed for random simulations: ", app_ran_seed)
    }

    ## AD - Activity Data
    AD <- fct_make_mcs(.n_iter = usr$n_iter, .pdf = ad_x$trans_pdf, .mean = ad_x$trans_area, .se = ad_x$trans_se, .trunc = usr$trunc_pdf)

    ## EF - Emissions Factors decomposed for each carbon pool

    ## Carbon stock of initial land use
    c_init    <- .cs |> filter(lu_id == ad_x$lu_initial_id)
    c_i_check <- fct_check_pool(.c_lu = c_lu_init, .c_unit = usr_c_unit)
    c_i_form  <- fct_make_formula(.c_check = c_i_check, .c_unit = usr$c_unit)

    message("For land use: ", ad_x$lu_initial, ", the carbon stock formula is: ", c_i_form)

    ## AGB initial
    if (c_i_check$has_AG) {
      c_pool <- c_init |> filter(c_pool == "AGB")
      AGB_i <- fct_make_mcs(.n_iter = usr$n_iter, .pdf = c_pool$c_pdf, .mean = c_pool$c_value, .se = c_pool$c_se, .trunc = usr$trunc_pdf)
    }
    # median(AGB_i)
    # c_pool$c_value

    ## BGB initial
    if (c_i_check$has_BG) {
      c_pool <- c_init |> filter(c_pool == "BGB")
      BGB_i <- fct_make_mcs(.n_iter = usr$n_iter, .pdf = c_pool$c_pdf, .mean = c_pool$c_value, .se = c_pool$c_se, .trunc = usr$trunc_pdf)
    }

    ## RS initial
    if (c_i_check$has_RS) {

      c_pool <- c_init |> filter(c_pool == "RS")

      if (c_pool$c_pdf == "normal") {
        sim_mean <- c_pool$c_value
        sim_se   <- c_pool$c_se
        SIMS <- rnorm(n = usr$n_iter, mean = sim_mean, sd = sim_se)
        if (usr$trunc_pdf) SIMS[SIMS < 0] <- 0
        RS_i <- SIMS
      }

    }






  }) ## END MAP()



  ## Get PDF parameters
  if (c_init_has_AG) {
    c_init_ag <- c_init |> filter(c_pool == "AGB")

    if (c_init_ag$c_pdf == "normal") {
      AGB_i_mean <- c_init |> filter(c_pool == "AGB") |> pull(c_value)
      AGB_i_se   <- c_init |> filter(c_pool == "AGB") |> pull(c_se)

      if (!is.na(usr_ran_seed)) set.seed(usr_ran_seed)
      AGB_i <- rnorm(n = usr_n_iter, mean = AGB_i_mean, sd = AGB_i_se)
      # min(AGB_i)
      # median(AGB_i)
      if (usr_trunc_pdf) AGB_i[AGB_i < 0] <- 0
      # median(AGB_i)
    }




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

  ## Formula


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



