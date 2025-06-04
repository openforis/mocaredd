#' #' Generate and propagate Monte Carlo Simulations based on a template input file.
#' #'
#' #' @description TBD
#' #'
#' #'
#' #' @param .ad Activity Data input table for the shiny app (AD_lu_transitions)
#' #' @param .cs Carbon Stock input table for the shiny app (c_stocks)
#' #' @param .usr User inputs' table for the shiny app (user_inputs). Contains the number
#' #'             of iterations of the MCS, carbon fraction if needed and if truncated PDFs
#' #'             should be used when necessary.
#' #'
#' #' @return A data frame with Monte Carlo simulations of CO2 emissions for each land use
#' #'         transition, REDD+ activity or emission reductions level.
#' #'
#' #' @importFrom rlang .data
#' #'
#' #' @examples
#' #' library(mocaredd)
#' #' library(readxl)
#' #' library(dplyr)
#' #'
#' #' path <- system.file("extdata/example1-4pools.xlsx", package = "mocaredd")
#' #'
#' #' cs <- read_xlsx(path = path, sheet = "c_stocks", na = "NA")
#' #' ad <- read_xlsx(path = path, sheet = "AD_lu_transitions", na = "NA")
#' #' usr <- read_xlsx(path = path, sheet = "user_inputs", na = "NA")
#' #'
#' #' cs_clean <- cs |> filter(!is.na(c_value) | !is.na(c_pdf_a))
#' #'
#' #' res <- fct_combine_mcs_E(.ad = ad, .cs = cs_clean, .usr = usr)
#' #'
#' #' get_trans <- sample(res$trans_id, 1)
#' #' res_sub <- res |> filter(trans_id == get_trans)
#' #'
#' #' hist(res_sub$E_sim)
#' #' round(median(res_sub$E_sim))
#' #'
#' #' @export
#' fct_combine_mcs_E <- function(.ad, .cs, .usr){
#'
#'   ## !!! FOR TESTING ONLY - run example then assign ad, cs and usr to the input vars.
#'   # .ad <- ad
#'   # .cs <- cs
#'   # .usr <- usr
#'   ## !!!
#'
#'   ## Seed for random simulation
#'   ## Implemented outside function now
#'   # if (!is.na(.usr$ran_seed)){
#'   #   set.seed(.usr$ran_seed)
#'   #   message("Random simulations with seed: ", .usr$ran_seed)
#'   # } else {
#'   #   app_ran_seed <- sample(1:100, 1)
#'   #   set.seed(app_ran_seed)
#'   #   message("Seed for random simulations: ", app_ran_seed)
#'   # }
#'
#'   ## Get all Cstock simulations
#'   mcs_c <- fct_combine_mcs_cstock(.ad = .ad, .cs = .cs, .usr = .usr)
#'
#'   ## Get all land use transition
#'   vec_trans <- unique(.ad$trans_id)
#'
#'   ## For each transition, calculate simulations for each element of the calculation chain
#'   mcs_trans <- purrr::map(vec_trans, function(x){
#'
#'     ## !! FOR TESTING ONLY
#'     # x = "T1_H_H_deg" #"T1_P_Crop" #"T1_ev_wet_closed_dg_ev_wet_closed"
#'     # x = "T1_EV_Crop"
#'     ## !!
#'
#'     ad_x   <- .ad |> dplyr::filter(.data$trans_id == x)
#'     redd_x <- ad_x$redd_activity
#'
#'     ## AD - Activity Data
#'     SIMS_AD <- dplyr::tibble(
#'       sim_no = 1:.usr$n_iter,
#'       redd_activity = ad_x$redd_activity,
#'       trans_id = ad_x$trans_id,
#'       trans_period = ad_x$trans_period,
#'       AD = round(fct_make_mcs(
#'         .n_iter = .usr$n_iter,
#'         .pdf    = ad_x$trans_pdf,
#'         .mean   = round(ad_x$trans_area, 0),
#'         .se     = round(ad_x$trans_se, 0),
#'         .params = c(ad_x$c_pdf_a, ad_x$c_pdf_b, ad_x$c_pdf_c),
#'         .trunc  = .usr$trunc_pdf
#'       ), 0)
#'     )
#'
#'     ## EF - Emissions Factors decomposed for each carbon pool
#'     ## Carbon stock of initial land use
#'     SIMS_CI <- mcs_c |>
#'       dplyr::filter(.data$lu_id == ad_x$lu_initial_id) |>
#'       dplyr::select("sim_no", "period", lu_id_i = "lu_id", c_form_i = "c_form", c_stock_i = "c_stock")
#'
#'     ## Carbon stock of final land use
#'     SIMS_CF <- mcs_c |>
#'       dplyr::filter(.data$lu_id == ad_x$lu_final_id) |>
#'       dplyr::select("sim_no", "period", lu_id_f = "lu_id", c_form_f = "c_form", c_stock_f = "c_stock")
#'
#'     ## Combine AD and EF by land use and if needed time period
#'     if (unique(SIMS_CI$period) == "ALL" &  unique(SIMS_CF$period) == "ALL") {
#'
#'       SIMS_CI <- SIMS_CI |> dplyr::select(-"period")
#'       SIMS_CF <- SIMS_CF |> dplyr::select(-"period")
#'
#'       combi <- SIMS_AD |>
#'         dplyr::left_join(SIMS_CI, by = "sim_no") |>
#'         dplyr::left_join(SIMS_CF, by = "sim_no")
#'
#'     } else {
#'
#'       combi <- SIMS_AD |>
#'         dplyr::left_join(SIMS_CI, by = c("sim_no", "trans_period" = "period")) |>
#'         dplyr::left_join(SIMS_CF, by = c("sim_no", "trans_period" = "period"))
#'
#'     }
#'
#'     combi
#'
#'   }) |> purrr::list_rbind()
#'   ## END LOOP
#'
#'   ## Re-arrange columns and add EF and E (emissions at transition level)
#'   mcs_trans |>
#'     dplyr::mutate(
#'       EF = round((.data$c_stock_i - .data$c_stock_f) * 44/12, 3),
#'       E_sim  = round(.data$AD * .data$EF, 0)
#'     ) |>
#'     dplyr::select(
#'       "sim_no", "redd_activity", time_period = "trans_period", "trans_id",
#'       "AD", "EF", "E_sim", "c_form_i", "c_stock_i", "c_form_f",
#'       "c_stock_f", dplyr::everything()
#'     )
#'
#' }
#'
#'
#'
