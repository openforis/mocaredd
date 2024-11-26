#' Calculate emissions based on arithmetic means
#'
#' @description TBD
#'
#'
#' @param .ad Activity Data input table for the shiny app (AD_lu_transitions)
#' @param .cs Carbon Stock input table for the shiny app (c_stocks)
#' @param .usr User inputs' table for the shiny app (user_inputs). Contains the number
#'             of iterations of the MCS, carbon fraction if needed and if truncated PDFs
#'             should be used when necessary.
#' @param .time the 'time' table from the tool input file (see template)
#'
#' @return A data frame with arithmetic mean of CO2 emissions for each land use
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
#' time <- read_xlsx(
#'   path = system.file("extdata/example1.xlsx", package = "mocaredd"),
#'   sheet = "time_periods",
#'   na = "NA"
#'   )
#'
#' #ad_clean <- ad |> dplyr::filter(!is.na(trans_area) | !is.na(trans_pdf_a))
#' #cs_clean <- cs |> dplyr::filter(!is.na(c_value) | !is.na(c_pdf_a))
#' time_clean <- time |> dplyr::mutate(nb_years = year_end - year_start + 1)
#'
#' res <- fct_arithmetic_mean(.ad = ad, .cs = cs, .usr = usr, .time = time_clean)
#' head(res$emissions)
#' res$gg_emissions
#'
#' @export
fct_arithmetic_mean <- function(.ad, .cs, .usr, .time){

  ## !!! FOR TESTING ONLY
  # .ad = ad
  # .cs = cs
  # .usr = usr
  # .time = time_clean
  ## !!!

  usr_ari <- .usr |> dplyr::mutate(n_iter = 1)
  ad_ari  <- .ad  |> dplyr::mutate(trans_se = 0, trans_pdf = "normal")
  cs_ari  <- .cs  |> dplyr::mutate(c_se = 0, c_pdf = "normal")

  .time <- .time |> dplyr::mutate(nb_years = .data$year_end - .data$year_start + 1)

  ari_trans <- fct_combine_mcs_E(.ad = ad_ari, .cs = cs_ari, .usr = usr_ari)

  ari_REF  <- fct_combine_mcs_P(
    .data = ari_trans,
    .time = .time,
    .period_type = "REF",
    .ad_annual = usr_ari$ad_annual
  )

  ari_MON  <- fct_combine_mcs_P(
    .data = ari_trans,
    .time = .time,
    .period_type = "MON",
    .ad_annual = usr_ari$ad_annual
  )

  ari_ER <- fct_combine_mcs_ER(.sim_ref = ari_REF, .sim_mon = ari_MON, .ad_annual = .usr$ad_annual) |>
    dplyr::mutate(period_type = paste0("ER-", .data$period_type)) |>
    dplyr::select("period_type", E = "ER_sim")

  ari_REF2 <- ari_REF |> dplyr::select("period_type", E = "E_sim")
  ari_MON2 <- ari_MON |>
    dplyr::mutate(period_type = paste0("E-", .data$period_type)) |>
    dplyr::select("period_type", E = "E_sim")

  ari_combi <- ari_REF2 |>
    dplyr::bind_rows(ari_MON2) |>
    dplyr::bind_rows(ari_ER)

  out_combi <- .time |>
    dplyr::group_by(.data$period_type) |>
    dplyr::summarize(
      year_start = min(.data$year_start),
      year_end   = max(.data$year_end)
    ) |>
    dplyr::mutate(nb_years = .data$year_end - .data$year_start + 1) |>
    dplyr::arrange(.data$year_start) |>
    dplyr::left_join(dplyr::bind_rows(ari_REF, ari_MON), by = "period_type")

  mon <- out_combi |> dplyr::filter(stringr::str_detect(.data$period_type, pattern = "MON"))

  years_mon <- min(mon$year_start):max(mon$year_end)

  ggdat <- purrr::map(years_mon, function(x){

    REF <- out_combi |>
      dplyr::filter(.data$period_type == "REF") |>
      dplyr::pull("E_sim")

    REF  <- round(REF / 10^6, 2)

    E <- mon |>
      dplyr::filter(.data$year_start <= x, .data$year_end >= x) |>
      dplyr::pull("E_sim")

    E <- round(E / 10^6, 2)

    data.frame(year = x, E = E, REF = REF)

  }) |> purrr::list_rbind()

  out_gg <- ggdat |>
    # mutate(year = lubridate::year(.data$year)) |>
    ggplot2::ggplot(ggplot2::aes(x = .data$year)) +
    ggplot2::geom_col(ggplot2::aes(y = .data$REF), col = "darkgreen", fill = "lightgreen") +
    ggplot2::geom_col(ggplot2::aes(y = .data$E), col = "darkred", fill = "pink") +
    #ggplot2::scale_x_continuous(expand=c(0, .9)) +
    ggplot2::theme_bw(base_size = 20) +
    ggplot2::labs(
      x = "Years",
      y = "Emissions (MtCO2e/y)"
    )

  list(ER = ari_combi, emissions = out_combi, gg_emissions = out_gg)

}




