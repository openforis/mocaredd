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
#'
#' @examples
#' library(readxl)
#' library(dplyr)
#' library(mocaredd)
#'
#' path <- system.file("extdata/example2-with-sims.xlsx", package = "mocaredd")
#' path <- system.file("extdata/example1-4pools.xlsx", package = "mocaredd")
#'
#' cs <- read_xlsx(path = path, sheet = "c_stocks", na = "NA")
#' ad <- read_xlsx(path = path, sheet = "AD_lu_transitions", na = "NA")
#' usr <- read_xlsx(path = path, sheet = "user_inputs", na = "NA")
#' time <- read_xlsx(path = path, sheet = "time_periods", na = "NA")
#'
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

  ari_REF <- fct_combine_mcs_P(
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

  ari_REF2 <- ari_REF |> dplyr::select("period_type", "E")
  ari_MON2 <- ari_MON |>
    dplyr::mutate(period_type = paste0("E-", .data$period_type)) |>
    dplyr::select("period_type", "E")

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


  # mon <- out_combi |> dplyr::filter(stringr::str_detect(.data$period_type, pattern = "MON"))
  #
  # years_mon <- min(mon$year_start):max(mon$year_end)
  #
  # ggdat <- purrr::map(years_mon, function(x){
  #
  #   REF <- out_combi |>
  #     dplyr::filter(.data$period_type == "REF") |>
  #     dplyr::pull("E")
  #
  #   REF  <- round(REF / 10^6, 2)
  #
  #   E <- mon |>
  #     dplyr::filter(.data$year_start <= x, .data$year_end >= x) |>
  #     dplyr::pull("E")
  #
  #   E <- round(E / 10^6, 2)
  #
  #   data.frame(year = x, E = E, REF = REF)
  #
  # }) |> purrr::list_rbind()
  #
  # out_gg <- ggdat |>
  #   ggplot2::ggplot(ggplot2::aes(x = .data$year)) +
  #   #ggplot2::geom_col(ggplot2::aes(y = .data$REF), col = "darkgreen", fill = "lightgreen") +
  #   ggplot2::geom_line(ggplot2::aes(y = .data$REF), col = "darkgreen") +
  #   ggplot2::geom_col(ggplot2::aes(y = .data$E), col = "darkred", fill = "pink", width = 0.1) +
  #   ggplot2::scale_x_continuous(breaks = min(ggdat$year):max(ggdat$year), minor_breaks = NULL) +
  #   ggplot2::theme_bw(base_size = 20) +
  #   ggplot2::labs(
  #     x = "Years",
  #     y = "Emissions (MtCO2e/y)"
  #   )

  ## Add yearly table
  out_yearly <- purrr::map(out_combi$period_type, function(x){

    tt <- out_combi |> dplyr::filter(.data$period_type == x)
    year_start <- tt$year_start
    year_end   <- tt$year_end
    E          <- round(tt$E / 10^6, 2)

    dplyr::tibble(
      year = year_start:year_end,
      E = rep(E, length(year_start:year_end)),
      period_type = x,
      FREL = round(ari_REF$E / 10^6, 2)
    )

  }) |> purrr::list_rbind()

  out_gg2 <- out_yearly |>
    ggplot2::ggplot(ggplot2::aes(x = .data$year)) +
    ggplot2::geom_col(ggplot2::aes(y = .data$FREL), col = "darkgreen", fill = "lightgreen", width = 0.3) +
    ggplot2::geom_col(ggplot2::aes(y = .data$E), col = "darkred", fill = "pink", width = 0.3) +
    ggplot2::scale_x_continuous(breaks = min(out_yearly$year):max(out_yearly$year), minor_breaks = NULL) +
    ggplot2::theme_bw(base_size = 20) +
    ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 45, hjust = 1)) +
    ggplot2::labs(
      x = "Years",
      y = "Emissions (MtCO2e/y)"
    )

  out_yearly_mon <- out_yearly |> dplyr::filter(stringr::str_detect(.data$period_type, "MON"))
  out_gg3 <- out_yearly |>
    ggplot2::ggplot(ggplot2::aes(x = .data$year)) +
    ggplot2::geom_line(
      ggplot2::aes(y = .data$FREL),
      col = "pink", linewidth = 1
      ) +
    ggplot2::geom_point(ggplot2::aes(y = .data$E, colour = .data$period_type), size = 4) +
    ggplot2::geom_segment(
      data = out_yearly_mon,
      ggplot2::aes(xend = .data$year, y = .data$FREL, yend = .data$E),
      col = "limegreen", linewidth = 1,
      arrow = grid::arrow(length = grid::unit(0.2, "cm"), ends = "both")
      ) +
    ggplot2::scale_x_continuous(breaks = min(out_yearly$year):max(out_yearly$year), minor_breaks = NULL) +
    ggplot2::scale_y_continuous(limits = c(0, NA)) +
    ggplot2::theme_bw(base_size = 20) +
    ggplot2::theme(
      axis.text.x = ggplot2::element_text(angle = 45, hjust = 1),
      legend.position = "bottom") +
    ggplot2::labs(
      x = "Years",
      y = "Emissions (MtCO2e/y)",
      color = ""
    )

  list(ER = ari_combi, emissions = out_combi, gg_emissions = out_gg3)

}




