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
#' res <- fct_combine_sims_C_allstock(.ad = ad, .cs = cs, .usr = usr)
#' res |> filter(sim_no == 1)
#'
#' @export
fct_combine_sims_C_allstock <- function(.ad, .cs, .usr){

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

  if (is.numeric(.usr$c_fraction)) {
    sims_CF <- tibble(
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
  }

  ## 2. simulate C elements ####

  ## + Prepare loop over time periods, land uses and C elements
  combi <- .cs |>
    dplyr::filter(!(is.na(.data$c_value) & is.na(.data$c_pdf_a))) |>
    dplyr::select(period = "c_period", lu = "c_lu_id", c_el = "c_element") |>
    dplyr::distinct()

  ## + Run loop
  sims_C <- purrr::pmap(combi, function(period, lu, c_el){

    ## !!! FOR TESTING ONLY
    # period = "ALL"
    # lu = "EV_deg"
    # c_el = "DG_ratio"
    ## !!!

    params <- .cs |>
      dplyr::filter(.data$c_lu_id == lu, .data$c_period == period, .data$c_element == c_el)

    params_not_norm <- round(c(params$c_pdf_a, params$c_pdf_b, params$c_pdf_c), 3)

    sims_el <- fct_make_mcs(
      .n_iter = .usr$n_iter,
      .pdf    = params$c_pdf,
      .mean   = round(params$c_value, 3),
      .se     = round(params$c_se, 3),
      .params = params_not_norm,
      .trunc  = .usr$trunc_pdf
    )

    dplyr::tibble(
      sim_no = 1:.usr$n_iter,
      period = period,
      lu_id = lu,
      c_element = c_el,
      sims = sims_el
    )

    }) |>
    purrr::list_rbind() |>
    tidyr::pivot_wider(names_from = c_element, values_from = sims)

  ## CHECK
  # tt <- sims_C |> dplyr::filter(.data$sim_no == 1)
  # n_rows_expected <- cs |> dplyr::distinct(.data$c_period, .data$c_lu_id) |> nrow()
  # nrow(sims_C) == .usr$n_iter * n_rows_expected

  ## 3. Get C stock formula and calculate C stock ####

  ## + Prepare loop
  combi <- .cs |>
    dplyr::filter(!(is.na(.data$c_value) & is.na(.data$c_pdf_a))) |>
    dplyr::select(period = "c_period", lu = "c_lu_id") |>
    dplyr::distinct()

  ## + Run loop
  combi_form <- purrr::pmap(combi, function(period, lu){

    c_sub <- .cs |> filter(.data$c_period == period, .data$c_lu_id == lu)

    c_check <- fct_check_pool(.c_sub = c_sub, .c_unit = .usr$c_unit, .c_fraction = .usr$c_fraction)
    c_form  <- fct_make_formula(.c_check = c_check, .c_unit = .usr$c_unit)

    tibble(
      period = period,
      lu_id = lu,
      c_form = c_form
    )

  }) |> purrr::list_rbind()

  ## CHECK
  # combi_form

  ## + Add to sims
  if (is.numeric(.usr$c_fraction)) {
    sims_C_form <- sims_C |>
      left_join(sims_CF, by = "sim_no") |>
      left_join(combi_form, by = c("period", "lu_id"))
  } else {
    sims_C_form <- sims_C |>
      left_join(combi_form, by = c("period", "lu_id"))
  }

  sims_C_all <- sims_C_form |>
    rowwise() |>
    mutate(
      C_all = if_else(c_form == "DG_ratio", NA_real_, round(eval(parse(text=c_form)), 3))
      ) |>
    ungroup()

  ## CHECK
  # tt <- sims_C_all |> dplyr::filter(.data$sim_no == 1)

  ## 4. Calculate C for degraded land uses

  if ("DG_ratio" %in% unique(sims_C_all$c_form)) {

    ## + Get pools used for DG
    if (.usr$dg_pool == "ALL") {
      dg_pool <- "C_all"
    } else {
      dg_pool <- stringr::str_split(.usr$dg_pool, pattern = ",") |> purrr::map(stringr::str_trim) |> unlist()
    }
    dg_pool_intact <- paste0(dg_pool, "_intact")

    ## + Filter DG to modify formula and recalculate
    sims_DG <- sims_C_all |>
      dplyr::filter(.data$c_form == "DG_ratio") |>
      dplyr::mutate(
        lu_intact = stringr::str_remove(.data$lu_id, pattern = .usr$dg_ext)
      )

    sims_C_intact <- sims_C_all |>
      dplyr::filter(.data$lu_id %in% unique(sims_DG$lu_intact)) |>
      dplyr::select("sim_no", lu_intact = "lu_id", !!!rlang::syms(dg_pool))

    names(sims_C_intact)[!(names(sims_C_intact) %in% c("sim_no", "lu_intact"))] <- dg_pool_intact

    sims_DG2 <- sims_DG |>
      dplyr::left_join(sims_C_intact, by = c("sim_no", "lu_intact")) |>
      dplyr::rowwise() |>
      dplyr::mutate(
        c_form = paste0(.data$c_form, " * (", paste0(dg_pool_intact, collapse = " + "), ")"),
        C_all  = round(.data$DG_ratio * sum(!!!rlang::syms(dg_pool_intact)), 3)
      ) |>
      dplyr::ungroup() |>
      dplyr::select(-"lu_intact", -dplyr::all_of(dg_pool_intact))

    sims_C_all2 <- sims_C_all |>
      dplyr::filter(.data$c_form != "DG_ratio") |>
      dplyr::bind_rows(sims_DG2)

  } else {
    sims_C_all2 <- sims_C_all
  }

  sims_C_all2

  ## Check
  # tt <- sims_C_all2 |> dplyr::filter(.data$sim_no == 1)

}



