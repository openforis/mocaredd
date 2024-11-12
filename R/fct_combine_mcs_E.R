#' Generate and propagate Monte Carlo Simulations based on a template input file.
#'
#' @description TBD
#'
#'
#' @param .ad Activity Data input table for the shiny app (AD_lu_transitions)
#' @param .cs Carbon Stock input table for the shiny app (c_stocks)
#' @param .usr User inputs' table for the shiny app (user_inputs). Contains the number
#'             of iterations of the MCS, carbon fraction if needed and if truncated PDFs
#'             should be used when necessary.
#'
#' @return A list of dataframes with Monte Carlo simulations for input variables, REDD+ activities CO2 emissions
#'         and removals and emission reductions levels.
#'
#' @importFrom rlang .data
#' @importFrom magrittr %>%
#'
#' @examples
#' library(readxl)
#' library(dplyr)
#' library(mocaredd)
#'
#' cs <- read_xlsx(
#'   system.file("extdata/example1.xlsx", package = "mocaredd"),
#'   sheet = "c_stocks",
#'   na = "NA"
#'   )
#' ad <- read_xlsx(
#'   system.file("extdata/example1.xlsx", package = "mocaredd"),
#'   sheet = "AD_lu_transitions",
#'   na = "NA"
#'   )
#' usr <- read_xlsx(
#'   system.file("extdata/example1.xlsx", package = "mocaredd"),
#'   sheet = "user_inputs",
#'   na = "NA"
#'   )
#'
#' cs_clean <- cs |> filter(!is.na(c_value) | !is.na(c_pdf_a))
#'
#' res <- fct_combine_mcs_E(.ad = ad, .cs = cs_clean, .usr = usr)
#' hist(res$E_sim)
#' round(median(res$E_sim))
#'
#' @export
fct_combine_mcs_E <- function(.ad, .cs, .usr){

  ## CHECK THE INPUT DATA CONFORMITY - done outside function
  # flag_all <- fct_check_data(.ad = ad, .cs = cs, .init = init)

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
  vec_trans <- unique(.ad$trans_id)
  ## For each transition, calculate simulations for each element of the calculation chain
  mcs_trans <- purrr::map(vec_trans, function(x){

    ## !! FOR TESTING ONLY
    # x = "T1_DG_ev_wet_closed"
    ## !!

    ad_x <- .ad %>% dplyr::filter(.data$trans_id == x)

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

    SIMS_AD <- dplyr::tibble(
      sim_no = 1:.usr$n_iter,
      redd_activity = ad_x$redd_activity,
      trans_id = ad_x$trans_id,
      trans_period = ad_x$trans_period,
      AD = SIMS
      )

    ## EF - Emissions Factors decomposed for each carbon pool
    ## Carbon stock of initial land use
    c_i     <- .cs %>% dplyr::filter(.data$lu_id == ad_x$lu_initial_id)
    SIMS_CI <- fct_combine_mcs_C(.c_sub = c_i, .usr = .usr)

    names(SIMS_CI) <- c("sim_no", paste0(setdiff(names(SIMS_CI), "sim_no"), "_i"))

    c_f     <- .cs %>% dplyr::filter(.data$lu_id == ad_x$lu_final_id)
    SIMS_CF <- fct_combine_mcs_C(.c_sub = c_f, .usr = .usr)

    names(SIMS_CF) <- c("sim_no", paste0(setdiff(names(SIMS_CF), "sim_no"), "_f"))

    combi <- SIMS_AD |>
      dplyr::left_join(SIMS_CI, by = "sim_no") |>
      dplyr::left_join(SIMS_CF, by = "sim_no")

    ## If degradation is ratio, using .usr$dg_pool to calculate C_all_f
    if (redd_x == "DG" & length(.usr$dg_pool) > 0) {

      dg_pool <- stringr::str_split(.usr$dg_pool, pattern = ",") |> purrr::map(stringr::str_trim) |> unlist()
      dg_pool_i <- paste0(dg_pool, "_i")

      combi <- combi %>%
        dplyr::rowwise() %>%
        dplyr::mutate(C_all_f = .data$DG_ratio_f * sum(!!!rlang::syms(dg_pool_i)) * 44/12) %>%
        dplyr::ungroup()

      ## If degradation has unaffected pools, we identify them by difference and
      ## exclude them from EF formula
      if (.usr$dg_expool) {
        dg_expool <- paste0(setdiff(c_i$c_pool, dg_pool), "_i")
        combi <- combi %>%
          dplyr::rowwise() %>%
          dplyr::mutate(C_all_f = .data$C_all_f + sum(!!!rlang::syms(dg_expool))) %>%
          dplyr::ungroup()
      }

    }

    combi

  }) |> purrr::list_rbind()
  ## END LOOP

  ## Re-arrange columns and add EF and E (emissions at transition level)
  mcs_trans2 <- mcs_trans %>%
    dplyr::mutate(
      EF = .data$C_all_i - .data$C_all_f,
      E_sim  = .data$AD * .data$EF
    ) %>%
    dplyr::select(
      .data$sim_no, .data$redd_activity, time_period = .data$trans_period, .data$trans_id,
      .data$AD, .data$EF, .data$E_sim, .data$C_form_i, .data$C_all_i, .data$C_form_f,
      .data$C_all_f, dplyr::everything()
    )

}



