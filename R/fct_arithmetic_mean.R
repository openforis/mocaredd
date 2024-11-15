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
#' cs_clean <- cs |> filter(!is.na(c_value) | !is.na(c_pdf_a))
#'
#' res <- fct_arithmetic_mean(.ad = ad, .cs = cs_clean, .usr = usr)
#' head(res)
#'
#' @export
fct_combine_mcs_E <- function(.ad, .cs, .usr){

  ## !!! FOR TESTING ONLY
  # .ad = ad
  # .cs = cs
  # .usr = usr
  ## !!!

  ## START LOOP
  vec_trans <- unique(.ad$trans_id)
  ## For each transition, calculate simulations for each element of the calculation chain
  arithm_trans <- purrr::map(vec_trans, function(x){

    ## !! FOR TESTING ONLY
    # x = "T1_ev_wet_closed_dg_ev_wet_closed"
    ## !!

    ad_x <- .ad %>%
      dplyr::filter(.data$trans_id == x) %>%
      dplyr::select("redd_activity", "trans_id", "trans_period", "lu_initial_id", "lu_final_id", AD = "trans_area")

    redd_x <- ad_x$redd_activity

    ## EF - Emissions Factors decomposed for each carbon pool
    ## Carbon stock of initial land use
    c_i <- .cs %>%
      dplyr::filter(.data$lu_id == ad_x$lu_initial_id) %>%
      dplyr::filter(!(is.na(.data$c_value) & is.na(.data$c_pdf_a)))

    c_pools <- unique(c_i$c_pool)
    c_check <- fct_check_pool(.c_lu = c_i, .c_unit = .usr$c_unit, .c_fraction = .usr$c_fraction)
    c_form  <- fct_make_formula(.c_check = c_check, .c_unit = .usr$c_unit)

    c_i_wide <- c_i  %>%
      tidyr::pivot_wider(id_cols = "lu_id", names_from = "c_pool", values_from = "c_value")

    c_i_calc <- c_i_wide %>%
      dplyr::mutate(
        C_form = c_form,
        C_all = eval(parse(text=c_form), c_i_wide),
      )

    names(c_i_calc) <- paste0(names(c_i_calc), "_i")

    c_f     <- .cs %>%
      dplyr::filter(.data$lu_id == ad_x$lu_final_id) %>%
      dplyr::filter(!(is.na(.data$c_value) & is.na(.data$c_pdf_a)))

    c_pools <- unique(c_f$c_pool)
    c_check <- fct_check_pool(.c_lu = c_f, .c_unit = .usr$c_unit, .c_fraction = .usr$c_fraction)
    c_form  <- fct_make_formula(.c_check = c_check, .c_unit = .usr$c_unit)

    c_f_wide <- c_f  %>%
      tidyr::pivot_wider(id_cols = "lu_id", names_from = "c_pool", values_from = "c_value")

    c_f_calc <- c_f_wide %>%
      dplyr::mutate(
        C_form = c_form,
        C_all = eval(parse(text=c_form), c_f_wide),
      )

    names(c_f_calc) <- paste0(names(c_f_calc), "_f")

    combi <- ad_x |>
      dplyr::left_join(c_i_calc, by = c("lu_initial_id" = "lu_id_i")) |>
      dplyr::left_join(c_f_calc, by = c("lu_final_id" = "lu_id_f"))

    ## If degradation is ratio, using .usr$dg_pool to re-calculate C_all_f
    if (redd_x == "DG" & length(.usr$dg_pool) > 0) {

      dg_pool <- stringr::str_split(.usr$dg_pool, pattern = ",") |> purrr::map(stringr::str_trim) |> unlist()
      dg_pool_i <- paste0(dg_pool, "_i")

      combi <- combi %>%
        dplyr::rowwise() %>%
        dplyr::mutate(C_all_f = .data$DG_ratio_f * sum(!!!rlang::syms(dg_pool_i)) * 44/12) %>%
        dplyr::ungroup()

      ## If degradation has unaffected pools, we identify them by difference and
      ## exclude them from EF formula
      if (.usr$dg_expool) {
        dg_expool <- paste0(setdiff(c_i$c_pool, dg_pool), "_i")
        combi <- combi %>%
          dplyr::rowwise() %>%
          dplyr::mutate(C_all_f = .data$C_all_f + sum(!!!rlang::syms(dg_expool))) %>%
          dplyr::ungroup()
      }

    }

    combi

  }) |> purrr::list_rbind()
  ## END LOOP

  ## Re-arrange columns and add EF and E (emissions at transition level)
  arithm_trans2 <- arithm_trans %>%
    dplyr::mutate(
      EF = .data$C_all_i - .data$C_all_f,
      E_sim  = .data$AD * .data$EF
    ) %>%
    dplyr::select(
      "redd_activity", time_period = "trans_period", "trans_id",
      "AD", "EF", "E_sim", "C_form_i", "C_all_i", "C_form_f",
      "C_all_f", dplyr::everything()
    )

}



