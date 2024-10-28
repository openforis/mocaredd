#' MAke a forest plot to show Monte Carlo simulation results
#'
#' @description Produce a [gt] table with the median and confidence interval of
#'              Monte Carlo simulations for each category of a level of the analysis
#'              (land use transition, REDD+ activity, time period) and a forest plot
#'              showing the median and CI together.
#'
#' @param .data a data frame containing the simulation aggregated results
#' @param .id the ID column of the data frame
#' @param .value the value column of the data frame (i.e median of the simulations)
#' @param .uperc the percentage uncertainty of the simulations
#' @param .cilower the lower end of confidence interval
#' @param .ciupper the higher end of the confidence interval
#' @param .id_colname character text for the ID column names. goes to [gt::md()] in [gt::cols_label()]
#' @param .conflevel character text to specify what confidence level was used
#' @param .filename path to save the table as png or NA.
#'
#' @return A character value with the formula for calculating total carbon stock for a specific land use.
#'
#' @import ggplot
#' @import gt
#' @importFrom dplyr mutate, select, if_else
#'
#' @examples
#' library(mocaredd)
#' library(readxl)
#' library(dplyr)
#'
#' cs    <- read_xlsx(system.file("extdata/example1.xlsx", package = "mocaredd"), sheet = "c_stock", na = "NA")
#' c_lu  <- cs |> filter(lu_id == "dg_ev_wet_closed")
#'
#' c_check <- fct_check_pool(.c_lu = c_lu, .c_unit = "C", .c_fraction = NA)
#'
#' fct_make_formula(.c_check = c_check, .c_unit = "C")
#'
#' @export
fct_forestplot <- function(
    .data,
    .id,
    .value,
    .uperc,
    .cilower,
    .ciupper,
    .id_colname,
    .conflevel,
    .filename
    ){

  ## Wrap around col names to avoid typing characters
  col_id    <- rlang::enquo(.id)
  col_value <- rlang::enquo(.value)
  col_uperc <- rlang::enquo(.uperc)
  col_cilo  <- rlang::enquo(.cilower)
  col_ciup  <- rlang::enquo(.ciupper)


  E_min <- eval(substitute(min(.data$.cilower)))
  E_max <- eval(substitute(max(.data$.ciupper)))
  E_range <- c(E_min, E_max)

  gt_out <- .data |>
    dplyr::select(!!col_id, !!col_value, !!col_uperc, !!col_cilo, !!col_ciup) |>
    dplyr::mutate(
      plot = !!col_id,
      !!col_cilo := dplyr::if_else(!!col_cilo == 0, NA_integer_, !!col_cilo),
      !!col_ciup := dplyr::if_else(!!col_ciup == 0, NA_integer_, !!col_ciup)
    ) |>
    gt() |>
    cols_label(
      !!col_id := md(.id_colname),
      !!col_value := "E (tCO2/y)",
      !!col_uperc := "U (%)",
      !!col_cilo  := paste0("CI (", .conflevel, ")"),
      plot = ""
    ) |>
    cols_merge(
      columns = c(!!col_cilo, !!col_ciup),
      pattern = "<<({1}>> - <<{2})>>",
    ) |>
    tab_spanner(
      label = "MCS results",
      columns = c(!!col_value, !!col_uperc, !!col_cilo, !!col_ciup, plot)
      #columns = starts_with("E")
    ) |>
    fmt_number(decimals = 0) |>
    fmt_percent(columns = rlang::as_name(col_uperc), scale_values = F, decimals = 0) |>
    sub_missing(
      columns = "E_U",
      missing_text = "-"
    ) |>
    text_transform(
      locations = cells_body(columns = 'plot'),
      fn = function(column) {
        map(column, function(x){

          ## !! FOR TESTING ONLY
          # x = "T1_DF_ev_moist_closed"
          # column = res_trans$trans_id
          ## !!

          .data |>
            filter(!!col_id == x) |>
            ggplot() +
            geom_point(aes(x = !!col_value, y = !!col_id), size = 40) +
            geom_segment(
              aes(x = !!col_cilo, xend = !!col_ciup, y = !!col_id, yend = !!col_id),
              linewidth = 12
              ) +
            geom_vline(xintercept = 0, linetype = "dotted", linewidth = 8) +
            geom_vline(xintercept = E_min, linewidth = 4) +
            geom_vline(xintercept = E_max, linewidth = 4) +
            theme_minimal() +
            scale_y_discrete(breaks = NULL) +
            scale_x_continuous(breaks = NULL) +
            theme(axis.text = element_text(size = 120)) +
            labs(x = element_blank(), y = element_blank()) +
            coord_cartesian(xlim = E_range)

        }) |>
          ggplot_image(height = px(30), aspect_ratio = 5)
      }
    )

  if (is.character(.filename)) gtsave(gt_out, filename = .filename)

  gt_out

}
