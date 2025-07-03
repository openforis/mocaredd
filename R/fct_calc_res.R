#' Calculate median and confidence interval based on Monte Carlo Simulations
#'
#' @description Monte Carlo simulations from other package functions generate
#'              a table with one row = one simulation. fct_calc_res() takes the
#'              simulation table and output the simulated variable's median and
#'              its confidence interval for each of the selected category (land
#'              use transition, REDD+ activity or time period).
#'
#' @param .data a data frame containing the simulation aggregated results
#' @param .id the name of the ID column of the data frame
#' @param .sim the name of the column containing the simulated  values
#' @param .ci_alpha alpha value for the confidence interval with confidence level = 1 - alpha.
#'
#' @return A tibble with simulation results per category if ID column: estimated mean,
#'         percentage uncertainty, margin of error, lower and upper bound of confidence interval.
#'
#' @importFrom rlang .data
#'
#' @examples
#' ## TBD
#'
#' @export
fct_calc_res <- function(
    .data,
    .id,
    .sim,
    .ci_alpha
){

  col_id <- rlang::enquo(.id)
  col_sim <- rlang::enquo(.sim)

  tmp_name <- "E_sim" ## avoid E on both side of = in summarise()

  .data |>
    dplyr::rename("E_sim" := !!col_sim) |>
    dplyr::group_by(!!col_id) |>
    dplyr::summarise(
      E = round(stats::median(.data$E_sim)),
      E_cilower = round(stats::quantile(.data$E_sim, .ci_alpha/2)),
      E_ciupper = round(stats::quantile(.data$E_sim, 1 - .ci_alpha/2)),
      .groups = "drop"
    ) |>
    dplyr::mutate(
      E_ME  = round((.data$E_ciupper - .data$E_cilower) / 2),
      E_U   = round(.data$E_ME / .data$E * 100),
    ) |>
    dplyr::select(rlang::as_label(col_id), "E", "E_U", "E_ME", "E_cilower", "E_ciupper")

}
