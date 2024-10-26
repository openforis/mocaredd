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
fct_combine_mcs_all <- function(.ad, .cs, .init, .usr){

  ## CHECK THE INPUT DATA CONFORMITY
  flag_all <- fct_check_data(.ad = ad, .cs = cs, .init = init)

  #n_trans <- nrow(.ad)

  ## Seed for random simulation
  if (!is.na(.usr$ran_seed)){
    set.seed(.usr$ran_seed)
    message("Random simulations with seed: ", .usr$ran_seed)
  } else {
    app_ran_seed <- sample(1:100, 1)
    set.seed(app_ran_seed)
    message("Seed for random simulations: ", app_ran_seed)
  }

  ## START LOOP
  ## For each transition, calculate simulations for each element of the calculation chain
  mcs_trans <- map(ad$trans_id, function(x){

    ## !! FOR TESTING ONLY
    # x = "T1_DF_ev_wet_closed"
    ## !!

    ad_x <- .ad |> filter(trans_id == x)

    redd_x <- ad_x$redd_activity

    ## AD - Activity Data
    SIMS <- fct_make_mcs(
      .n_iter = .usr$n_iter,
      .pdf    = ad_x$trans_pdf,
      .mean   = ad_x$trans_area,
      .se     = ad_x$trans_se,
      .params = c(ad_x$c_pdf_a, ad_x$c_pdf_b, ad_x$c_pdf_c),
      .trunc  = .usr$trunc_pdf
      )

    SIMS_AD <- tibble(
      sim_no = 1:.usr$n_iter,
      redd_activity = ad_x$redd_activity,
      trans_id = ad_x$trans_id,
      trans_period = ad_x$trans_period,
      AD = SIMS
      )

    ## EF - Emissions Factors decomposed for each carbon pool
    ## Carbon stock of initial land use
    c_i     <- .cs |> filter(lu_id == ad_x$lu_initial_id)
    SIMS_CI <- fct_combine_mcs_cstock(.c_sub = c_i, .c_unit = .usr$c_unit, .n_iter = .usr$n_iter)

    names(SIMS_CI) <- c("sim_no", paste0(setdiff(names(SIMS_CI), "sim_no"), "_i"))

    c_f     <- .cs |> filter(lu_id == ad_x$lu_final_id)
    SIMS_CF <- fct_combine_mcs_cstock(.c_sub = c_f, .c_unit = .usr$c_unit, .n_iter = .usr$n_iter)

    names(SIMS_CF) <- c("sim_no", paste0(setdiff(names(SIMS_CF), "sim_no"), "_f"))

    combi <- SIMS_AD |>
      left_join(SIMS_CI, by = join_by(sim_no)) |>
      left_join(SIMS_CF, by = join_by(sim_no))

    ## If degradation is ratio, using .usr$dg_pool to calculate C_all_f
    if (redd_x == "DG" & length(.usr$dg_pool) > 0) {

      dg_pool <- str_split(.usr$dg_pool, pattern = ",") |> map(str_trim) |> unlist() |> paste0("_i")

      combi <- combi |>
        rowwise() |>
        mutate(C_all_f = C_all_f * sum(!!!syms(dg_pool)) * 44/12) |>
        ungroup()

    }

    ## If degradation has unaffected pools, we identify them by difference and
    ## exclude them from EF formula
    if (redd_x == "DG" & .usr$dg_expool) {

      dg_expool <- paste0(setdiff(c_i$c_pool, c_f$c_pool), "_i")

      combi <- combi |>
        rowwise() |>
        mutate(C_all_f = C_all_f + sum(!!!syms(dg_expool)) )|>
        ungroup()
    }

    combi

  }) |> list_rbind()
  ## END LOOP

  ## Re-arrange columns and add EF and E (emissions at transition level)
  mcs_trans2 <- mcs_trans |>
    mutate(
      EF = C_all_i - C_all_f,
      E_trans  = AD * EF
    ) |>
    select(
      sim_no, redd_activity, time_period = trans_period, trans_id, AD, EF, E_trans, C_form_i, C_all_i, C_form_f, C_all_f, everything()
    )

}



