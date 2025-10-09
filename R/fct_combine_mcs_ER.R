#' Combine MCS of emissions from Reference and monitoring period into emission reductions
#'
#' @description Depending on how the period is defined and if the data are annualized
#'              or not, calculate the Emission Level for a reference or monitoring period
#'              for each simulation.
#'
#' @param .sim_ref simulation aggregated to reference period, output from fct_combine_mcs_P()
#' @param .sim_mon simulation aggregated to monitoring period(s), output from fct_combine_mcs_P()
#' @param .ad_annual TRUE or FALSE, is the activity data annualized or not.
#'
#' @return A tibble with simulations at the final estimate per type of period.
#'
#' @importFrom rlang .data
#'
#' @examples
#' library(mocaredd)
#' library(readxl)
#' library(dplyr)
#'
#' path <- system.file("extdata/example1-4pools.xlsx", package = "mocaredd")
#'
#' cs <- read_xlsx(path = path, sheet = "c_stocks", na = "NA")
#' ad <- read_xlsx(path = path, sheet = "AD_lu_transitions", na = "NA")
#' usr <- read_xlsx(path = path, sheet = "user_inputs", na = "NA")
#' time <- read_xlsx(path = path, sheet = "time_periods", na = "NA")
#'
#' time_clean <- time |> dplyr::mutate(nb_years = year_end - year_start + 1)
#'
#' sim_trans <- fct_combine_mcs_E(.ad = ad, .cs = cs, .usr = usr)
#'
#' sim_REF <- fct_combine_mcs_P(
#'   .data = sim_trans,
#'   .time = time_clean,
#'   .period_type = "REF",
#'   .ad_annual = usr$ad_annual
#' )
#'
#'sim_MON <- fct_combine_mcs_P(
#'   .data = sim_trans,
#'   .time = time_clean,
#'   .period_type = "MON",
#'   .ad_annual = usr$ad_annual
#' )
#'
#' ## !!! SIM MON and ER to be done
#'
#' @export
fct_combine_mcs_ER <- function(
    .sim_ref,
    .sim_mon,
    .ad_annual
){

  ## !!! FOR TESTING ONLY - run example then assign to function inputs
  # .sim_ref = sim_REF
  # .sim_mon = sim_MON
  # .ad_annual = usr$ad_annual
  ## !!!

  moni_combi <- unique(.sim_mon$period_type)

  sim_ER <- purrr::map(moni_combi, function(x){

    out <- .sim_mon |>
      dplyr::filter(.data$period_type == x) |>
      dplyr::inner_join(.sim_ref, by = "sim_no", suffix = c("", "_R")) |>
      dplyr::mutate(ER_sim = .data$E_R - .data$E)

  }) |> purrr::list_rbind()

  sim_ER

  # res_ER <- sim_ER |>
  #   fct_calc_res(.id = period_type, .sim = ER_sim, .ci_alpha = ci_alpha)
  #
  # tmp_ER <- time_clean |>
  #   group_by(period_type) |>
  #   summarise(
  #     year_start = min(year_start),
  #     year_end = max(year_end),
  #     nb_years = sum(nb_years)
  #   )
  #
  # res_ER2 <- tmp_ER |> inner_join(res_ER, by = join_by(period_type))
  #
  # gt_ER <- res_ER |> fct_forestplot(
  #   .id = period_type,
  #   .value = E,
  #   .uperc = E_U,
  #   .cilower = E_cilower,
  #   .ciupper = E_ciupper,
  #   .id_colname = "Monitoring period",
  #   .conflevel = "90%",
  #   .filename = NA
  # )
  #
  # gg_ER <- fct_histogram(
  #   .dat = sim_ER,
  #   .res = res_ER,
  #   .id = period_type,
  #   .value = ER_sim,
  #   .value_type = "ER"
  # )


}
