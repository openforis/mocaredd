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
#' @return A data frame with Monte Carlo simulations of CO2 emissions for each land use
#'         transition, REDD+ activity or emission reductions level.
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
#' res <- fct_combine_mcs_cstock(.ad = ad, .cs = cs_clean, .usr = usr)
#' res |> filter(sim_no == 1)
#'
#' @export
fct_combine_mcs_cstock <- function(.ad, .cs, .usr){

  ## !!! FOR TESTING ONLY - run example then assign ad, cs and usr to the input vars.
  # .ad <- ad
  # .cs <- cs
  # .usr <- usr
  ## !!!

  ## Run sims for all carbon stocks and time periods
  all_lu <- unique(c(.ad$lu_initial_id, .ad$lu_final_id))
  c_lu   <- unique(.cs$lu_id[.cs$lu_id %in% all_lu])
  c_period <- sort(unique(.cs$c_period))

  combi <- tidyr::expand_grid(lu = c_lu, period = c_period)

  mcs_c <- purrr::pmap(combi, function(lu, period){

    c_sub <- .cs %>%
      dplyr::filter(.data$lu_id == lu, .data$c_period == period) %>%
      dplyr::filter(!(is.na(.data$c_value) & is.na(.data$c_pdf_a)))

    fct_combine_mcs_cpools(.c_sub = c_sub, .usr = .usr) |>
      dplyr::mutate(lu_id = lu, c_period = period)

  }) |>
    purrr::list_rbind() |>
    dplyr::select("sim_no", "c_period", "lu_id", "C_all", "C_form", dplyr::everything())

  ## CHECK
  # mcs_c |> dplyr::filter(.data$sim_no == 1)

}



