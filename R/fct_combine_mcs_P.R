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
#' @importFrom rlang .data
#'
#' @examples
#' library(readxl)
#' library(dplyr)
#' library(mocaredd)
#'
#' path <- system.file("extdata/example1-4pools.xlsx", package = "mocaredd")
#'
#' cs <- read_xlsx(path = path, sheet = "c_stocks", na = "NA")
#' ad <- read_xlsx(path = path, sheet = "AD_lu_transitions", na = "NA")
#' usr <- read_xlsx(path = path, sheet = "user_inputs", na = "NA")
#' time <- read_xlsx(path = path, sheet = "time_periods", na = "NA")
#'
#' ad_clean <- ad |> dplyr::filter(!is.na(trans_area) | !is.na(trans_pdf_a))
#' cs_clean <- cs |> dplyr::filter(!is.na(c_value) | !is.na(c_pdf_a))
#' time_clean <- time |> dplyr::mutate(nb_years = year_end - year_start + 1)
#'
#' sim_trans <- fct_combine_mcs_E(.ad = ad_clean, .cs = cs_clean, .usr = usr)
#'
#' sim_FREL <- fct_combine_mcs_P(
#'   .data = sim_trans,
#'   .time = time_clean,
#'   .period_type = "REF",
#'   .ad_annual = usr$ad_annual
#' )
#'
#' hist(sim_FREL$E_sim)
#' round(median(sim_FREL$E_sim))
#'
#' @export
fct_combine_mcs_P <- function(
    .data,
    .time,
    .period_type,
    .ad_annual
){

  ## !!! FOR TESTING ONLY
  # .data = sim_trans
  # .time = time_clean
  # .period_type = "REF"
  # .ad$annual = usr$ad_annual
  ## !!!


  ## aggregate redd+ periods for REF or MON
  time_ref   <- .time |> dplyr::filter(stringr::str_detect(.data$period_type, pattern = .period_type))
  nb_ref     <- length(unique(time_ref$period_no))
  length_ref <- sum(time_ref$nb_years)


  if (.ad_annual) {

    ## Weighted average of the sims from the reference sub-periods
    ## Get the volume per period then divide by total length of reference period
    out <- purrr::map(unique(time_ref$period_type), function(x){
      .data |>
        dplyr::left_join(time_ref, by = c("time_period" = "period_no")) |>
        dplyr::filter(.data$period_type == x) |>
        dplyr::group_by(.data$sim_no, .data$period_type) |>
        dplyr::summarise(E_sim = round(sum(.data$E_sim * .data$nb_years) / length_ref, 0), .groups = "drop")
    }) |> purrr::list_rbind()

  } else {

    ## Divide the volume of E over the reference period by the total length of the reference period
    out <- purrr::map(unique(time_ref$period_type), function(x){
      .data |>
        dplyr::left_join(time_ref, by = c("time_period" = "period_no")) |>
        dplyr::filter(.data$period_type == x) |>
        dplyr::group_by(.data$sim_no, .data$period_type) |>
        dplyr::summarise(E_sim = round(sum(.data$E_sim) / length_ref), .groups = "drop")
    }) |> purrr::list_rbind()

  }

  out

}
