#' Combine MCS of emissions to a defined time period
#'
#' @description Depending on how the period is defined and if the data are annualized
#'              or not, calculate the Emission Level for a reference or monitoring period
#'              for each simulation.
#'
#' @param .data a data frame containing the simulations
#' @param .time the 'time' table from the tool input file (see template)
#' @param .period_type "reference" or "monitoring"
#' @param .ad_annual TRUE or FALSE, is the activity data annualized or not.
#'
#' @return A tibble with simulations at the final estimate per type of period.
#'
#' @importFrom dplyr filter left_join join_by group_by summarise mutate select
#'
#' @examples
#' ## TBD
#'
#' @export
fct_combine_mcs_P <- function(
    .data,
    .time,
    .period_type,
    .ad_annual
){

  ## aggregate redd+ periods for the reference level
  time_ref   <- .time |> filter(period_type == .period_type)
  nb_ref     <- length(unique(time_ref$period_no))
  length_ref <- sum(time_ref$nb_years)


if (.ad_annual) {

    ## Weighted average of the sims from the reference sub-periods
    ## Get the volume per period then divide by total length of reference period
    out <- map(unique(time_ref$period_type), function(x){
      .data |>
        left_join(time_ref, by = join_by(time_period == period_no)) |>
        filter(period_type == x) |>
        group_by(sim_no, period_type) |>
        summarise(E_sim = sum(E_sim * nb_years) / length_ref, .groups = "drop")
    }) |> list_rbind()

  } else {
    ## Divide the volume of E over the reference period by the total length of the reference period
    out <- map(unique(time_ref$period_type), function(x){
      .data |>
        left_join(time_ref, by = join_by(time_period == period_no)) |>
        filter(period_type == x) |>
        group_by(sim_no, period_type) |>
        summarise(E_sim = sum(E_sim) / length_ref, .groups = "drop")
    }) |> list_rbind()

  }

  out

}
