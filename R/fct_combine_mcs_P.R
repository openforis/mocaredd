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
#' @importFrom dplyr filter, left_join, join_by, group_by, summarise, mutate, select
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
  nb_ref     <- length(unique(time_ref$period_combinations))
  length_ref <- sum(time_ref$nb_years)

  if (nrow(time_ref) == 1) {

    ## Extract the sims of the period if only one period
    sim_FREL <- .data |>
      filter(time_period == time_ref$period_no) |>
      group_by(sim_no) |>
      summarise(E_sim = sum(E_sim))

  } else if (nrow(time_ref) > 1 & .ad_annual) {

    ## Weighted average of the sims from the reference sub-periods
    ## Get the volume per period then divide by total length of reference period
    sim_FREL <- .data |>
      filter(time_period %in% time_ref$period_no) |>
      left_join(time_ref, by = join_by(time_period == period_no)) |>
      group_by(sim_no) |>
      summarise(E_sim = sum(E_sim * nb_years) / length_ref, .groups = "drop")


  } else if (nrow(time_ref) > 1 & !.ad_annual) {
    ## Divide the volume of E over the reference period by the total length of the reference period
    sim_FREL <- .data |>
      filter(time_period %in% time_ref$period_no) |>
      group_by(sim_no) |>
      summarise(E_sim = sum(E_sim) / length_ref, .groups = "drop")

  } else {

    ## Something wrong
    return("Issues while aggregating Emissions over sub-periods.")

  }

}
