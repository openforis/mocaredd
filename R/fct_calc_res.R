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
#' @importFrom dplyr group_by summarise mutate select
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

  .data |>
    group_by(!!col_id) |>
    summarise(
      E = round(median(!!col_sim)),
      E_cilower = round(quantile(!!col_sim, .ci_alpha/2)),
      E_ciupper = round(quantile(!!col_sim, 1 - .ci_alpha/2)),
      .groups = "drop"
    ) |>
    mutate(
      E_ME  = round((E_ciupper - E_cilower) / 2),
      E_U   = round(E_ME / E * 100),
    ) |>
    select(!!col_id, E, E_U, E_ME, E_cilower, E_ciupper)
}
