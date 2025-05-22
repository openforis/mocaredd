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
#'
#' @examples
#' library(mocaredd)
#' library(readxl)
#' library(dplyr)
#'
#' path <- system.file("extdata/example2-with-sims.xlsx", package = "mocaredd")
#'
#' cs <- read_xlsx(path = path, sheet = "c_stocks", na = "NA")
#' ad <- read_xlsx(path = path, sheet = "AD_lu_transitions", na = "NA")
#' usr <- read_xlsx(path = path, sheet = "user_inputs", na = "NA")
#'
#' res <- fct_combine_mcs_cstock(.ad = ad, .cs = cs, .usr = usr)
#' res |> filter(sim_no == 1)
#'
#' @export
fct_combine_mcs_cstock <- function(.ad, .cs, .usr){

  ## !!! FOR TESTING ONLY - run example then assign ad, cs and usr to the input vars.
  # .ad <- ad
  # .cs <- cs
  # .usr <- usr
  ## !!!

  ## STEPS:
  ## 1. If AGB and/or BGB expressed as dry matter (DM), simulate carbon fraction (CF)
  ## 2. Simulate C elements
  ## 3. Get C stock formula and calculate C stock
  ## 4. Calculate degraded C stocks if based on degradation ratios DG_ratio

  ## 1. simulate CF ####

  if (is.numeric(.usr$c_fraction) & .usr$c_unit == "DM") {
    sims_CF <- dplyr::tibble(
      sim_no = 1:.usr$n_iter,
      CF = round(fct_make_mcs(
        .n_iter = .usr$n_iter,
        .pdf    = .usr$c_fraction_pdf,
        .mean   = round(.usr$c_fraction, 3),
        .se     = round(.usr$c_fraction_se, 3),
        #.params = c(params$c_pdf_a, params$c_pdf_b, params$c_pdf_c),
        .trunc  = .usr$trunc_pdf
      ), 3)
    )
  } else {
    sims_CF <- dplyr::tibble(
      sim_no = 1:.usr$n_iter,
      CF     = rep("NA", .usr$n_iter)
    )
  }

  ## 2. simulate C elements except DG_ratio ####

  ## + Make simulations as list columns
  sims_cols <- .cs |>
    dplyr::filter(!(is.na(.data$c_value) & is.na(.data$c_pdf_a))) |>
    dplyr::rowwise() |>
    dplyr::mutate(
      params_not_norm = list(round(c(.data$c_pdf_a, .data$c_pdf_b, .data$c_pdf_c), 3)),
      SIMS = list(fct_make_mcs(
        .n_iter = .usr$n_iter,
        .pdf    = .data$c_pdf,
        .mean   = round(.data$c_value, 3),
        .se     = round(.data$c_se, 3),
        .params = .data$params_not_norm,
        .trunc  = .usr$trunc_pdf
      ))
    ) |>
    dplyr::ungroup()

  ## + Make long table with C elements as columns
  sims_cols_noDG <- sims_cols |> dplyr::filter(.data$c_element != "DG_ratio")

  sims_C_noDG <- sims_cols_noDG |>
    dplyr::select(period = "c_period", lu_id = "c_lu_id", "c_element", "SIMS") |>
    tidyr::unnest("SIMS") |>
    dplyr::mutate(sim_no = rep(1:.usr$n_iter, nrow(sims_cols_noDG))) |>
    tidyr::pivot_wider(names_from = "c_element", values_from = "SIMS") |>
    dplyr::left_join(sims_CF, by = "sim_no")

  ## CHECK
  # sims_cols[1,]
  # hist(sims_cols$SIMS[[1]])
  # hist(sims_C_noDG$AGB[1:.usr$n_iter])

  ## 3. Calculate C stock - no degradation from ratios ####

  ## + Get C elements for each period x lu
  c_elements <- .cs |>
    dplyr::filter(.data$c_element != "DG_ratio") |>
    dplyr::summarise(c_el = list(c(.data$c_element)), .by = c("c_period", "c_lu_id")) |>
    dplyr::rowwise() |>
    dplyr::mutate(
      c_form = fct_make_formula(.c_el = .data$c_el, .c_unit = .usr$c_unit)
    ) |>
    dplyr::ungroup() |>
    dplyr::select(period = "c_period", lu_id = "c_lu_id", "c_form")

  sims_C_noDG_calc <- sims_C_noDG |>
    dplyr::left_join(c_elements, by = c("period", "lu_id")) |>
    dplyr::rowwise() |>
    dplyr::mutate(c_stock = round(eval(parse(text = .data$c_form)), 3)) |>
    dplyr::ungroup() |>
    dplyr::select("sim_no", tidyr::everything())


  ## 4. Calculate C for degraded land uses using ratio ####

  if (!"DG_ratio" %in% unique(sims_cols$c_element)) {

    return(sims_C_noDG_calc)

  } else {

    ## + Get C elements used for DG
    if (.usr$dg_pool == "ALL") {
      dg_pool <- "c_stock"
    } else {
      dg_pool <- stringr::str_split(.usr$dg_pool, pattern = ",") |>
        purrr::map(stringr::str_trim) |>
        unlist()
    }

    dg_pool_before <- paste0(dg_pool, "_before")

    ## + Get intact land use that are degraded
    dg_lu        <- stringr::str_subset(.cs$c_lu_id, pattern = .usr$dg_ext)
    dg_lu_before <- stringr::str_remove(dg_lu, pattern = .usr$dg_ext)

    ## + Get simulated Cstocks before degradation
    sims_C_before <- sims_C_noDG_calc |>
      dplyr::filter(.data$lu_id %in% dg_lu_before) |>
      purrr::discard(~all(is.na(.))) |>
      dplyr::rename_with(.cols = tidyr::all_of(dg_pool), .fn = paste0, "_before") |>
      dplyr::select("sim_no", "period", lu_before = "lu_id", tidyr::ends_with("_before"))

    ## GS: NOT NEEDED / TO BE CHECKED USING DIRECTLY 'dg_pool'
    ## + Check if some pools are not affected by degradation
    # if (.usr$dg_pool == "ALL") {
    #   dg_excluded_pool <- NA
    # } else {
    #   dg_potential_pool <- .cs |>
    #     filter(c_lu_id %in% lu_intact) |>
    #     pull(c_element) |>
    #     unique()
    #   dg_excluded_pool <- dg_potential_pool[!dg_potential_pool %in% dg_pool]
    #   ## GS: May need to revise when processing Ghana template
    # }

    ## + Get DG_ratio simulations as long table
    sims_cols_DG <- sims_cols |> dplyr::filter(.data$c_element == "DG_ratio")

    sims_DG <- sims_cols_DG |>
      dplyr::select(period = "c_period", lu_id = "c_lu_id", "c_element", "SIMS") |>
      tidyr::unnest("SIMS") |>
      dplyr::mutate(sim_no = rep(1:.usr$n_iter, nrow(sims_cols_DG))) |>
      tidyr::pivot_wider(names_from = "c_element", values_from = "SIMS") |>
      dplyr::mutate(lu_before = stringr::str_remove(.data$lu_id, .usr$dg_ext))

    ## + Join intact pools C to degraded land uses, remake formula and calculate new Cstock
    sims_DG_calc <- sims_DG |>
      dplyr::left_join(sims_C_before, by = c("sim_no", "period", "lu_before")) |>
      dplyr::rowwise() |>
      dplyr::mutate(
        c_form = paste0("DG_ratio * (", paste0(dg_pool_before, collapse = " + "), ")"),
        c_stock  = round(.data$DG_ratio * sum(!!!rlang::syms(dg_pool_before)), 3)
      ) |>
      dplyr::ungroup() |>
      dplyr::select(-"lu_before") |>
      dplyr::select("sim_no", tidyr::everything())


    ## Output
    dplyr::bind_rows(sims_C_noDG_calc, sims_DG_calc)

  }

}



