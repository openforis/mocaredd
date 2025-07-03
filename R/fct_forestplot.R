#' Make a forest plot to show Monte Carlo simulation results
#'
#' @description Produce a [gt::gt()] table with the median and confidence interval of
#'              Monte Carlo simulations for each category of a level of the analysis
#'              (land use transition, REDD+ activity, time period) and a forest plot
#'              showing the median and CI together.
#'
#' @param .data a data frame containing the simulation aggregated results
#' @param .id the name of the ID column of the data frame
#' @param .value_ari NULL if not relevant (most cases) or a column name with the arithmetic mean of Emission or emission reductions.
#' @param .value the name of the value column of the data frame (i.e median of the simulations)
#' @param .uperc the name of the percentage uncertainty of the simulations
#' @param .cilower the name of the lower end of confidence interval
#' @param .ciupper the name of the higher end of the confidence interval
#' @param .id_colname character text for the ID column names. goes to [gt::md()] in [gt::cols_label()]
#' @param .conflevel character text to specify what confidence level was used, ex. "90%".
#' @param .filename path to save the table or NA.
#'
#' @return A character value with the formula for calculating total carbon stock for a specific land use.
#'
#' @importFrom rlang .data :=
#' @importFrom ggplot2 ggplot aes geom_point geom_segment geom_vline theme_minimal
#'             scale_y_discrete scale_x_continuous theme element_text labs
#'             element_blank coord_cartesian
#' @importFrom gt gt
#'
#' @export
fct_forestplot <- function(
    .data,
    .id,
    .value_ari = NULL,
    .value,
    .uperc,
    .cilower,
    .ciupper,
    .id_colname,
    .conflevel,
    .filename = NA
    ){

  ## Defuse col names to avoid input characters
  col_id    <- rlang::enquo(.id)
  col_value <- rlang::enquo(.value)
  col_uperc <- rlang::enquo(.uperc)
  col_cilo  <- rlang::enquo(.cilower)
  col_ciup  <- rlang::enquo(.ciupper)

  ## data range for the plot
  E_min <- .data |> dplyr::pull(!!col_cilo) |> min()
  E_min <- min(0, E_min)

  E_max <- .data |> dplyr::pull(!!col_ciup) |> max()
  E_max <- max(0, E_max)

  #E_min <- eval(substitute(min(.data$.cilower, 0)))
  #E_max <- eval(substitute(max(.data$.ciupper, 0)))
  E_range <- c(E_min, E_max)

  ## Extend range to 5% of the data range to make it look nicer
  plot_min <- E_min - ((E_max - E_min) * 0.05)
  plot_max <- E_max + ((E_max - E_min) * 0.05)
  plot_range <- c(plot_min, plot_max)

  ## Output
  if (missing(.value_ari)){

    gt_out <- .data |>
      dplyr::select(!!col_id, !!col_value, !!col_uperc, !!col_cilo, !!col_ciup) |>
      dplyr::mutate(
        plot = !!col_id,
        !!col_cilo := dplyr::if_else(!!col_cilo == 0, NA_integer_, !!col_cilo),
        !!col_ciup := dplyr::if_else(!!col_ciup == 0, NA_integer_, !!col_ciup)
      ) |>
      gt::gt() |>
      gt::cols_label(
        !!col_id := gt::md(.id_colname),
        !!col_value := "E (tCO2/y)",
        !!col_uperc := "U (%)",
        !!col_cilo  := paste0("CI (", .conflevel, ")"),
        plot = ""
      ) |>
      gt::cols_merge(
        columns = c(!!col_cilo, !!col_ciup),
        pattern = "<<({1}>> - <<{2})>>",
      ) |>
      gt::tab_spanner(
        label = "MCS results",
        columns = c(!!col_value, !!col_uperc, !!col_cilo, !!col_ciup, plot)
        #columns = starts_with("E")
      ) |>
      gt::fmt_number(decimals = 0) |>
      gt::fmt_percent(columns = rlang::as_name(col_uperc), scale_values = F, decimals = 0) |>
      gt::sub_missing(
        columns = "E_U",
        missing_text = "-"
      ) |>
      gt::text_transform(
        locations = gt::cells_body(columns = 'plot'),
        fn = function(column) {
          purrr::map(column, function(x){

            ## !! FOR TESTING ONLY
            # x = "T1_DF_ev_moist_closed"
            # column = res_trans$trans_id
            ## !!

            .data |>
              dplyr::filter(!!col_id == x) |>
              ggplot() +
              geom_point(aes(x = !!col_value, y = !!col_id), size = 40) +
              geom_segment(
                aes(x = !!col_cilo, xend = !!col_ciup, y = !!col_id, yend = !!col_id),
                linewidth = 12
              ) +
              geom_vline(xintercept = 0, linetype = "dotted", linewidth = 8) +
              geom_vline(xintercept = plot_min, linewidth = 4) +
              geom_vline(xintercept = plot_max, linewidth = 4) +
              theme_minimal() +
              scale_y_discrete(breaks = NULL) +
              scale_x_continuous(breaks = NULL) +
              theme(axis.text = element_text(size = 120)) +
              labs(x = element_blank(), y = element_blank()) +
              coord_cartesian(xlim = plot_range)

          }) |>
            gt::ggplot_image(height = gt::px(30), aspect_ratio = 5)
        }
      )

  } else {

    col_ari <- rlang::enquo(.value_ari)

    gt_out <- .data |>
      dplyr::select(!!col_id, !!col_ari, !!col_value, !!col_uperc, !!col_cilo, !!col_ciup) |>
      dplyr::mutate(
        plot = !!col_id,
        !!col_cilo := dplyr::if_else(!!col_cilo == 0, NA_integer_, !!col_cilo),
        !!col_ciup := dplyr::if_else(!!col_ciup == 0, NA_integer_, !!col_ciup)
      ) |>
      gt::gt() |>
      gt::cols_label(
        !!col_id := gt::md(.id_colname),
        !!col_ari := gt::md("Arithmetic<br>mean<br>(tCO2/y)"),
        !!col_value := "E (tCO2/y)",
        !!col_uperc := "U (%)",
        !!col_cilo  := paste0("CI (", .conflevel, ")"),
        plot = ""
      ) |>
      gt::cols_merge(
        columns = c(!!col_cilo, !!col_ciup),
        pattern = "<<({1}>> - <<{2})>>",
      ) |>
      gt::tab_spanner(
        label = "MCS results",
        columns = c(!!col_value, !!col_uperc, !!col_cilo, !!col_ciup, plot)
        #columns = starts_with("E")
      ) |>
      gt::fmt_number(decimals = 0) |>
      gt::fmt_percent(columns = rlang::as_name(col_uperc), scale_values = F, decimals = 0) |>
      gt::sub_missing(
        columns = "E_U",
        missing_text = "-"
      ) |>
      gt::text_transform(
        locations = gt::cells_body(columns = 'plot'),
        fn = function(column) {
          purrr::map(column, function(x){

            ## !! FOR TESTING ONLY
            # x = "T1_DF_ev_moist_closed"
            # column = res_trans$trans_id
            ## !!

            .data |>
              dplyr::filter(!!col_id == x) |>
              ggplot() +
              geom_point(aes(x = !!col_value, y = !!col_id), size = 40) +
              geom_segment(
                aes(x = !!col_cilo, xend = !!col_ciup, y = !!col_id, yend = !!col_id),
                linewidth = 12
              ) +
              geom_vline(xintercept = 0, linetype = "dotted", linewidth = 8) +
              geom_vline(xintercept = plot_min, linewidth = 4) +
              geom_vline(xintercept = plot_max, linewidth = 4) +
              theme_minimal() +
              scale_y_discrete(breaks = NULL) +
              scale_x_continuous(breaks = NULL) +
              theme(axis.text = element_text(size = 120)) +
              labs(x = element_blank(), y = element_blank()) +
              coord_cartesian(xlim = plot_range)

          }) |>
            gt::ggplot_image(height = gt::px(30), aspect_ratio = 5)
        }
      )


  }


  ## Save the table as img is path specified
  if (is.character(.filename)) {
    gt::gtsave(gt_out, filename = .filename)

    ## Workaround gtsave not working on linux/macos
    f <- chromote::default_chromote_object(); f$close()
  }

  gt_out

}
