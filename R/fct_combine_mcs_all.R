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
#' res <- fct_combine_mcs_all(.ad = ad, .cs = cs)
#'
#' ## ADD HISTOGRAM FOR ONE SET OF SIMULATIONS
#'
#' @export
fct_combine_mcs_all <- function(.ad, .cs){

  ## CHECK THE INPUT DATA CONFORMITY
  flag_all <- fct_check_data(.ad = ad, .cs = cs)

  n_trans <- nrow(.ad)

  ## START LOOP
  ## For each transition, calculate simulations for each element of the calculation chain
  mcs_trans <- map(1:n_trans, function(x){

    ## !! FOR TESTING ONLY
    # x = 1
    ## !!

    ad_x <- .ad[x,]

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
    AD <- tibble(
      sim_no = 1:usr$n_iter,
      redd_activity = ad_x$redd_activity,
      trans_id = ad_x$trans_id,
      AD = AD
      )

    ## EF - Emissions Factors decomposed for each carbon pool
    ## Carbon stock of initial land use
    c_i     <- .cs |> filter(lu_id == ad_x$lu_initial_id)
    c_i_mcs <- fct_combine_mcs_cstock(.c_sub = c_i, .c_unit = usr$c_unit, .n_iter = usr$n_iter)
    names(c_i_mcs) <- c(
      "sim_no", "redd_activity", "trans_id",
      paste0(setdiff(names(c_i_mcs), c("sim_no", "redd_activity", "trans_id")), "_i")
      )

    c_f     <- .cs |> filter(lu_id == ad_x$lu_final_id)
    c_f_mcs <- fct_combine_mcs_cstock(.c_sub = c_f, .c_unit = usr$c_unit, .n_iter = usr$n_iter)
    names(c_f_mcs) <- c(
      "sim_no", "redd_activity", "trans_id",
      paste0(setdiff(names(c_f_mcs), c("sim_no", "redd_activity", "trans_id")), "_f")
    )

    combi <- AD |>
      left_join(c_i_mcs, by = join_by(sim_no, redd_activity, trans_id)) |>
      left_join(c_f_mcs, by = join_by(sim_no, redd_activity, trans_id))

  }) |> list_rbind()
  ## END LOOP

  mcs_trans |> filter(redd_activity == "DG")

  mcs_E <- mcs_trans |>
    mutate(
      EF = C_all_i - C_all_f,
      E  = AD * EF
    )

#
#   # hist(sim$E)
#   E_median <- median(sim$E)
#   E_ci <- (quantile(sim$E, 0.95) - quantile(sim$E, 0.05)) / 2
#   E_ciperc <- round(E_ci / E_median * 100, 0)
#   res <- paste0(round(E_median, 0), " +/- ", E_ciperc, "%")
#   print(paste0(trans_id, ": ", res))
#
#   sim
#
# }) |> list_rbind()
#   #
#
#   U_redd <- U_all |>
#     group_by(redd_activity, sim_no) |>
#     summarise(E_redd = sum(E), .groups = "drop") |>
#     mutate(E_redd_mean = E_redd / RP)


}



