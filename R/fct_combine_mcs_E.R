#' Generate and propagate Monte Carlo Simulations based on a template input file.
#'
#' @description TBD
#'
#'
#' @param .checked_data The list returned by \code{fct_checkinput()}. Its
#'             \code{template_version} element drives version-specific behaviour and its
#'             \code{data} element supplies the \code{setup}, \code{time}, \code{area}
#'             and \code{carbon} tables.
#'
#' @return A data frame with Monte Carlo simulations of CO2 emissions for each land use
#'         transition, REDD+ activity or emission reductions level.
#'
#' @importFrom rlang .data
#'
#' @examples
#' path <- system.file("extdata/mocaredd-templatev2-simple.xlsx", package = "mocaredd")
#'
#' checked <- fct_checkinput(.path = path)
#'
#' res <- fct_combine_mcs_E(.checked_data = checked)
#'
#' get_trans <- sample(res$trans_id, 1)
#' res_sub <- res |> dplyr::filter(trans_id == get_trans)
#'
#' hist(res_sub$E)
#'
#' @export
fct_combine_mcs_E <- function(.checked_data){

  ## !!! FOR TESTING ONLY
  # .checked_data <- fct_checkinput(.path = path)
  ## !!!

  ## Unpack the checked input (the output of fct_checkinput())
  .setup  <- .checked_data$data$setup
  .time   <- .checked_data$data$time
  .area   <- .checked_data$data$area
  .carbon <- .checked_data$data$carbon

  ## Ensure the derived period length is present (needed when AD is not annual)
  if (!"nb_years" %in% names(.time)) {
    .time <- .time |> dplyr::mutate(nb_years = .data$year_end - .data$year_start + 1)
  }


  ## TOC ####
  ## Calculation steps:

  ## 1. Preparation of data:
  ## 1.1. For Cstock, make formula based on c_elements reported
  ## 1.2. Adjust formula for degradation ratio

  ## 2. Make simulations for all PDFs
  ## 2.1. Simulate carbon fraction (CF), if AGB and/or BGB expressed as dry matter (DM)
  ## 2.2. Simulate all C elements in Cstock table.
  ## 2.3. Simulate AD for each time period and land use change

  ## 3. Prepare SIMS for combining AD and EF
  ## 3.1. Make long table with 1 row a unique combination of sim_no, time_period and lu_id or trans_id
  ## 3.2. Add formulas and Cstock to CEL sims (ex. DG_ratio)
  ## 3.2.1. If DG_ratio used calulate Cstock of other land uses
  ## 3.2.2. Get Cstock of intact land uses associated with degradation
  ## 3.2.3. Add C of pools excluded from degradation
  ## 3.2.4. Combine intact C with DG_ratio and calculate Cstock
  ## 3.2.5. Combine all land uses

  ## 4. Make Cstock initial and final for each LU transition, calculate EF and E as ADxEF



  ##
  ## 1. Preparation of data ####################################################
  ##

  ## + 1.0. Template version handling ####
  ## v1: one carbon unit (.setup$c_unit) and one carbon fraction (.setup$c_fraction).
  ## v2: carbon unit is a per-row column of 'carbon' and CF is a dedicated 'carbon'
  ##     row (c_element == "CF") applied globally.
  is_v2 <- isTRUE(.checked_data$template_version == 2)

  ## Land-use carbon pools, excluding the global CF row in v2
  carbon_pools <- if (is_v2) dplyr::filter(.carbon, .data$c_element != "CF") else .carbon

  ## Map a degraded land-use id back to its intact id by stripping the degradation
  ## extension (and, in v2, a trailing separator left by the label-to-id conversion)
  to_intact_id <- function(lu_id) {
    out <- stringr::str_remove(lu_id, .setup$dg_ext)
    if (is_v2) out <- stringr::str_replace(out, "[^A-Za-z]$", "")
    out
  }

  ## + 1.1. Make Cstock formula from C elements in 'cs' table ####
  if (is_v2) {
    c_formula <- carbon_pools |>
      dplyr::summarise(
        c_el = list(c(.data$c_element)),
        c_un = list(c(.data$c_unit)),
        .by = c("c_period", "c_lu_id")
      ) |>
      dplyr::rowwise() |>
      dplyr::mutate(
        c_form = fct_make_formula2(.c_el = .data$c_el, .c_unit = .data$c_un, .version = 2)
      ) |>
      dplyr::ungroup() |>
      dplyr::select(time_period = "c_period", lu_id = "c_lu_id", "c_form")
  } else {
    c_formula <- carbon_pools |>
      dplyr::summarise(c_el = list(c(.data$c_element)), .by = c("c_period", "c_lu_id")) |>
      dplyr::rowwise() |>
      dplyr::mutate(
        c_form = fct_make_formula2(.c_el = .data$c_el, .c_unit = .setup$c_unit, .version = 1)
      ) |>
      dplyr::ungroup() |>
      dplyr::select(time_period = "c_period", lu_id = "c_lu_id", "c_form")
  }


  ## + 1.2. correct degradation formula if degradation elements with ratio ####
  degrat_lu <- carbon_pools |>
    dplyr::filter(.data$c_element == "DG_ratio") |>
    dplyr::pull("c_lu_id")

  degrat_lu_intact <- to_intact_id(degrat_lu)

  if (length(degrat_lu) > 0) {
    c_formula <- c_formula |>
      dplyr::mutate(
        c_form = dplyr::case_when(
          .data$lu_id %in% degrat_lu & .setup$dg_pool == "ALL" ~ "DG_ratio * c_intact",
          .data$lu_id %in% degrat_lu & .setup$dg_pool != "ALL" ~ "DG_ratio * c_intact + (1 - DG_ratio) * c_intact_noDG",
          TRUE ~ c_form
        )
      )
  }


  ##
  ## 2. Make simulations for all PDFs ##########################################
  ##

  ## + 2.1. simulate CF ####
  ## v2: carbon fraction comes from the 'carbon' row where c_element == "CF".
  ## v1: carbon fraction comes from .setup$c_fraction when the unit is dry matter.
  if (is_v2) {
    cf_row <- .carbon |> dplyr::filter(.data$c_element == "CF")
    has_cf <- nrow(cf_row) > 0
  } else {
    has_cf <- is.numeric(.setup[["c_fraction"]]) & isTRUE(.setup[["c_unit"]] == "DM")
  }

  if (is_v2 && has_cf) {
    sims_CF <- dplyr::tibble(
      sim_no = 1:.setup$n_iter,
      CF = round(fct_make_mcs(
        .n_iter = .setup$n_iter,
        .pdf    = cf_row$c_pdf[1],
        .mean   = round(cf_row$c_value[1], 3),
        .se     = round(cf_row$c_se[1], 3),
        .params = round(c(cf_row$c_pdf_a[1], cf_row$c_pdf_b[1], cf_row$c_pdf_c[1]), 3),
        .trunc  = .setup$trunc_pdf
      ), 3)
    )
  } else if (!is_v2 && has_cf) {
    sims_CF <- dplyr::tibble(
      sim_no = 1:.setup$n_iter,
      CF = round(fct_make_mcs(
        .n_iter = .setup$n_iter,
        .pdf    = .setup$c_fraction_pdf,
        .mean   = round(.setup$c_fraction, 3),
        .se     = round(.setup$c_fraction_se, 3),
        #.params = c(params$c_pdf_a, params$c_pdf_b, params$c_pdf_c),
        .trunc  = .setup$trunc_pdf
      ), 3)
    )
  } else {
    sims_CF <- dplyr::tibble(
      sim_no = 1:.setup$n_iter,
      CF     = rep("NA", .setup$n_iter)
    )
  }


  ## + 2.2. Make simulations for each carbon element as list columns ####
  sims_CEL <- carbon_pools |>
    dplyr::filter(!(is.na(.data$c_value) & is.na(.data$c_pdf_a))) |>
    dplyr::rowwise() |>
    dplyr::mutate(
      params_not_norm = list(round(c(.data$c_pdf_a, .data$c_pdf_b, .data$c_pdf_c), 3)),
      SIMS = list(fct_make_mcs(
        .n_iter = .setup$n_iter,
        .pdf    = .data$c_pdf,
        .mean   = round(.data$c_value, 3),
        .se     = round(.data$c_se, 3),
        .params = .data$params_not_norm,
        .trunc  = .setup$trunc_pdf
      ))
    ) |>
    dplyr::ungroup()


  ## + 2.3. Simulate AD for each time period and land use change ####

  sims_AD <- .area |>
    dplyr::filter(!(is.na(.data$trans_area) & is.na(.data$trans_pdf_a))) |>
    dplyr::rowwise() |>
    dplyr::mutate(
      params_not_norm = list(round(c(.data$trans_pdf_a, .data$trans_pdf_b, .data$trans_pdf_c), 3)),
      SIMS = list(fct_make_mcs(
        .n_iter = .setup$n_iter,
        .pdf    = .data$trans_pdf,
        .mean   = round(.data$trans_area, 3),
        .se     = round(.data$trans_se, 3),
        .params = .data$params_not_norm,
        .trunc  = .setup$trunc_pdf
      ))
    ) |>
    dplyr::ungroup()


  ##
  ## 3. Prepare SIMS for combining AD and EF ####
  ##

  ## + 3.1. Make long table with 1 row a unique combination of sim_no, time_period and lu_id or trans_id ####

  sims_AD_long <- sims_AD |>
    dplyr::select(time_period = "trans_period", "trans_id", "lu_initial_id", "lu_final_id", "redd_activity", AD = "SIMS") |>
    tidyr::unnest("AD") |>
    dplyr::mutate(sim_no = rep(1:.setup$n_iter, nrow(sims_AD)))

  sims_CEL_long <- sims_CEL |>
    dplyr::select(time_period = "c_period", lu_id = "c_lu_id", "c_element", "SIMS") |>
    tidyr::unnest("SIMS") |>
    dplyr::mutate(sim_no = rep(1:.setup$n_iter, nrow(sims_CEL))) |>
    tidyr::pivot_wider(names_from = "c_element", values_from = "SIMS") |>
    dplyr::left_join(sims_CF, by = "sim_no")

  ## + 3.2. Add formulas and Cstock to CEL sims (ex. DG_ratio) ####

  if ("DG_ratio" %in% carbon_pools$c_element) {

    ## ++ 3.2.1. If DG_ratio used calculate Cstock of other land uses ####
    sims_C_nodegrat <- sims_CEL_long |>
      dplyr::filter(is.na(.data$DG_ratio)) |>
      dplyr::left_join(c_formula, by = c("time_period", "lu_id")) |>
      dplyr::rowwise() |>
      dplyr::mutate(c_stock = round(eval(parse(text = .data$c_form)), 3)) |>
      dplyr::ungroup()

    ## ++ 3.2.2. Get Cstock of intact land uses associated with degradation ####
    sims_C_lu_intact <- sims_C_nodegrat |>
      dplyr::filter(.data$lu_id %in% degrat_lu_intact) |>
      dplyr::select("time_period", lu_intact = "lu_id", "sim_no", c_intact = "c_stock")

    ## ++ 3.2.3. Add C of pools excluded from degradation ####
    if (.setup$dg_pool == "ALL") {
      sims_C_lu_intact_expool <- sims_C_lu_intact |>
        dplyr::mutate(c_intact_noDG = NA)
    } else {
      ## DG not working when dg_pool != ALL
      # dg_expool <- .carbon |>
      #   dplyr::filter(.data$c_lu_id %in% degrat_lu_intact) |>
      #   dplyr::pull("c_element") |>
      #   unique()

      ## EXCLUDING dg_pool from list of all C pools
      dg_allpool <- carbon_pools |>
        dplyr::filter(.data$c_lu_id %in% degrat_lu_intact) |>
        dplyr::pull("c_element") |>
        unique()
      dg_pool <- stringr::str_split(stringr::str_trim(.setup$dg_pool), pattern = ",") |>
        purrr::map(stringr::str_trim) |>
        unlist()

      dg_expool <- setdiff(dg_allpool, dg_pool)

      sims_C_lu_intact_expool <- sims_C_nodegrat |>
        dplyr::filter(.data$lu_id %in% degrat_lu_intact) |>
        dplyr::select("time_period", lu_intact = "lu_id", "sim_no", c_intact = "c_stock", !!!rlang::syms(dg_expool)) |>
        dplyr::rowwise() |>
        dplyr::mutate(c_intact_noDG = sum(!!!rlang::syms(dg_expool))) |>
        dplyr::ungroup() |>
        dplyr::select("time_period", "lu_intact", "sim_no", "c_intact", "c_intact_noDG")
    }

    ## ++ 3.2.4. Combine intact C with DG_ratio and calculate Cstock ####
    sims_C_degrat <- sims_CEL_long |>
      dplyr::filter(!is.na(.data$DG_ratio)) |>
      dplyr::mutate(lu_intact = to_intact_id(.data$lu_id)) |>
      dplyr::left_join(sims_C_lu_intact_expool, by = c("time_period", "lu_intact", "sim_no")) |>
      dplyr::left_join(c_formula, by = c("time_period", "lu_id")) |>
      dplyr::rowwise() |>
      dplyr::mutate(c_stock = round(eval(parse(text = .data$c_form)), 3)) |>
      dplyr::ungroup()

    ## ++ 3.2.5. Combine all land uses ####
    sims_C <- sims_C_nodegrat |>
      dplyr::bind_rows(sims_C_degrat) |>
      dplyr::select(dplyr::where(~ !all(is.na(.)))) |>
      dplyr::select("time_period", "lu_id", "sim_no", "c_stock", "c_form", dplyr::everything())

    ## CHECK
    ## tt <- sims_C |> filter(sim_no == 1)

  } else {

    sims_C <- sims_CEL_long |>
      dplyr::left_join(c_formula, by = c("time_period", "lu_id")) |>
      dplyr::rowwise() |>
      dplyr::mutate(c_stock = round(eval(parse(text = .data$c_form)), 3)) |>
      dplyr::ungroup()

  }

  ##
  ## 4. Make Cstock initial and final for each LU transition ####
  ##

  if (all(carbon_pools$c_period == "ALL")) {

    c_cols <- names(sims_C)[!names(sims_C) %in% c("time_period", "lu_id", "sim_no")]

    sims_CI <- sims_C |>
      dplyr::select(-"time_period") |>
      dplyr::rename_with(.cols = dplyr::all_of(c_cols), paste0, "_i")

    sims_CF <- sims_C |>
      dplyr::select(-"time_period") |>
      dplyr::rename_with(.cols = dplyr::all_of(c_cols), paste0, "_f")

    sims_E <- sims_AD_long |>
      dplyr::left_join(sims_CI, by = c("lu_initial_id" = "lu_id", "sim_no")) |>
      dplyr::left_join(sims_CF, by = c("lu_final_id"   = "lu_id", "sim_no")) |>
      dplyr::mutate(
        EF = round((.data$c_stock_i - .data$c_stock_f) * 44 / 12, 3),
        E = round(.data$AD * .data$EF, 3)
        ) |>
      dplyr::select(
        "time_period", "trans_id", "lu_initial_id", "lu_final_id", "redd_activity",
        "sim_no", "E", "AD", "EF", "c_stock_i", "c_stock_f", dplyr::everything()
        )

  } else {

    ## TBD
    ## Get annual AD
    ## get initial CS at beginning of period, final CS and delta CS per year.
    ## Reconstruct AD and EF for est period REF adn MON_X
    sims_E <- 'Different CS for different time periods not yet implemented (C_period != "ALL")'

  }


  ##
  ## 5. prepare annualized emissions E_year ####
  ##

  ## If AD is annual, E is annual: scale to the transition's period so that
  ## E is always the period total and E_year always the annual value.
  sims_Eannual <- sims_E |>
    dplyr::left_join(
      dplyr::select(.time, "period_no", period_length = "nb_years", "period_type"),
      by = dplyr::join_by("time_period" == "period_no")
    ) |>
    ## E_year first: mutate() is sequential, so it must read E before E is rescaled
    dplyr::mutate(
      E_year = if (.setup$ad_annual) .data$E                       else round(.data$E / .data$period_length, .setup$digits),
      E =      if (.setup$ad_annual) .data$E * .data$period_length else .data$E
    ) |>
    dplyr::select(
      "time_period", "period_length", "period_type", "trans_id", "lu_initial_id", "lu_final_id", "redd_activity",
      "sim_no", "E_year", "E", dplyr::everything()
    )

  ## Regrouped action, if AD is annual period_length is 1. Added period_type.
  # if (.setup$ad_annual) {
  #   sims_Eannual <- sims_E |>
  #     dplyr::mutate(time_period_length = 1, E_year = .data$E) |>
  #     dplyr::select(
  #       "time_period", "time_period_length", "trans_id", "lu_initial_id", "lu_final_id", "redd_activity",
  #       "sim_no", "E_year", "E", dplyr::everything()
  #     )
  #   } else {
  #     sims_Eannual <- sims_E |>
  #       dplyr::left_join(
  #         dplyr::select(.time, "period_no", time_period_length = "nb_years"),
  #         by = dplyr::join_by("time_period" == "period_no")
  #         ) |>
  #       dplyr::mutate(
  #         E_year = round(.data$E / .data$time_period_length, .setup$digits)
  #       ) |>
  #       dplyr::select(
  #         "time_period", "time_period_length", "trans_id", "lu_initial_id", "lu_final_id", "redd_activity",
  #         "sim_no", "E_year", "E", dplyr::everything()
  #       )
  #   }


  ##
  ## 6. Output simulation table ####
  ##
  sims_Eannual

} ## END FUNCTION




  # ## + Make long table with C elements as columns
  # sims_cols_noDG <- sims_cols |> dplyr::filter(.data$c_element != "DG_ratio")
  #
  # sims_C_noDG <- sims_cols_noDG |>
  #   dplyr::select(period = "c_period", lu_id = "c_lu_id", "c_element", "SIMS") |>
  #   tidyr::unnest("SIMS") |>
  #   dplyr::mutate(sim_no = rep(1:.setup$n_iter, nrow(sims_cols_noDG))) |>
  #   tidyr::pivot_wider(names_from = "c_element", values_from = "SIMS") |>
  #   dplyr::left_join(sims_CF, by = "sim_no")
  #
  # ## CHECK
  # # sims_cols[1,]
  # # hist(sims_cols$SIMS[[1]])
  # # hist(sims_C_noDG$AGB[1:.setup$n_iter])
  #
  # ## 3. Calculate C stock - no degradation from ratios
  #
  # ## + Get C elements for each period x lu
  # c_elements <- .carbon |>
  #   dplyr::filter(.data$c_element != "DG_ratio") |>
  #   dplyr::summarise(c_el = list(c(.data$c_element)), .by = c("c_period", "c_lu_id")) |>
  #   dplyr::rowwise() |>
  #   dplyr::mutate(
  #     c_form = fct_make_formula2(.c_el = .data$c_el, .c_unit = .setup$c_unit)
  #   ) |>
  #   dplyr::ungroup() |>
  #   dplyr::select(period = "c_period", lu_id = "c_lu_id", "c_form")
  #
  # sims_C_noDG_calc <- sims_C_noDG |>
  #   dplyr::left_join(c_elements, by = c("time_period", "lu_id")) |>
  #   dplyr::rowwise() |>
  #   dplyr::mutate(c_stock = round(eval(parse(text = .data$c_form)), 3)) |>
  #   dplyr::ungroup() |>
  #   dplyr::select("sim_no", tidyr::everything())
  #
  #
  # ## 4. Calculate C for degraded land uses using ratio
  #
  # if (!"DG_ratio" %in% unique(sims_cols$c_element)) {
  #
  #   return(sims_C_noDG_calc)
  #
  # } else {
  #
  #   ## + Get C elements used for DG
  #   if (.setup$dg_pool == "ALL") {
  #     dg_pool <- "c_stock"
  #   } else {
  #     dg_pool <- stringr::str_split(.setup$dg_pool, pattern = ",") |>
  #       purrr::map(stringr::str_trim) |>
  #       unlist()
  #   }
  #
  #   dg_pool_before <- paste0(dg_pool, "_before")
  #
  #   ## + Get intact land use that are degraded
  #   dg_lu        <- stringr::str_subset(.carbon$c_lu_id, pattern = .setup$dg_ext)
  #   dg_lu_before <- stringr::str_remove(dg_lu, pattern = .setup$dg_ext)
  #
  #   ## + Get simulated Cstocks before degradation
  #   sims_C_before <- sims_C_noDG_calc |>
  #     dplyr::filter(.data$lu_id %in% dg_lu_before) |>
  #     purrr::discard(~all(is.na(.))) |>
  #     dplyr::rename_with(.cols = tidyr::all_of(dg_pool), .fn = paste0, "_before") |>
  #     dplyr::select("sim_no", "time_period", lu_before = "lu_id", tidyr::ends_with("_before"))
  #
  #   ## GS: NOT NEEDED / TO BE CHECKED USING DIRECTLY 'dg_pool'
  #   ## + Check if some pools are not affected by degradation
  #   # if (.setup$dg_pool == "ALL") {
  #   #   dg_excluded_pool <- NA
  #   # } else {
  #   #   dg_potential_pool <- .carbon |>
  #   #     filter(c_lu_id %in% lu_intact) |>
  #   #     pull(c_element) |>
  #   #     unique()
  #   #   dg_excluded_pool <- dg_potential_pool[!dg_potential_pool %in% dg_pool]
  #   #   ## GS: May need to revise when processing Ghana template
  #   # }
  #
  #   ## + Get DG_ratio simulations as long table
  #   sims_cols_DG <- sims_cols |> dplyr::filter(.data$c_element == "DG_ratio")
  #
  #   sims_DG <- sims_cols_DG |>
  #     dplyr::select(period = "c_period", lu_id = "c_lu_id", "c_element", "SIMS") |>
  #     tidyr::unnest("SIMS") |>
  #     dplyr::mutate(sim_no = rep(1:.setup$n_iter, nrow(sims_cols_DG))) |>
  #     tidyr::pivot_wider(names_from = "c_element", values_from = "SIMS") |>
  #     dplyr::mutate(lu_before = stringr::str_remove(.data$lu_id, .setup$dg_ext))
  #
  #   ## + Join intact pools C to degraded land uses, remake formula and calculate new Cstock
  #   sims_DG_calc <- sims_DG |>
  #     dplyr::left_join(sims_C_before, by = c("sim_no", "time_period", "lu_before")) |>
  #     dplyr::rowwise() |>
  #     dplyr::mutate(
  #       c_form = paste0("DG_ratio * (", paste0(dg_pool_before, collapse = " + "), ")"),
  #       c_stock  = round(.data$DG_ratio * sum(!!!rlang::syms(dg_pool_before)), 3)
  #     ) |>
  #     dplyr::ungroup() |>
  #     dplyr::select(-"lu_before") |>
  #     dplyr::select("sim_no", tidyr::everything())
  #
  #
  #   ## Output
  #   dplyr::bind_rows(sims_C_noDG_calc, sims_DG_calc)




#   ## Seed for random simulation
#   ## Implemented outside function now
#   # if (!is.na(.setup$ran_seed)){
#   #   set.seed(.setup$ran_seed)
#   #   message("Random simulations with seed: ", .setup$ran_seed)
#   # } else {
#   #   app_ran_seed <- sample(1:100, 1)
#   #   set.seed(app_ran_seed)
#   #   message("Seed for random simulations: ", app_ran_seed)
#   # }
#
#   ## Get all Cstock simulations
#   mcs_c <- fct_combine_mcs_cstock(.area = .area, .carbon = .carbon, .setup = .setup)
#
#   ## Get all land use transition
#   vec_trans <- unique(.area$trans_id)
#
#   ## For each transition, calculate simulations for each element of the calculation chain
#   mcs_trans <- purrr::map(vec_trans, function(x){
#
#     ## !! FOR TESTING ONLY
#     # x = "T1_H_H_deg" #"T1_P_Crop" #"T1_ev_wet_closed_dg_ev_wet_closed"
#     # x = "T1_EV_Crop"
#     ## !!
#
#     ad_x   <- .area |> dplyr::filter(.data$trans_id == x)
#     redd_x <- ad_x$redd_activity
#
#     ## AD - Activity Data
#     SIMS_AD <- dplyr::tibble(
#       sim_no = 1:.setup$n_iter,
#       redd_activity = ad_x$redd_activity,
#       trans_id = ad_x$trans_id,
#       trans_period = ad_x$trans_period,
#       AD = round(fct_make_mcs(
#         .n_iter = .setup$n_iter,
#         .pdf    = ad_x$trans_pdf,
#         .mean   = round(ad_x$trans_area, 0),
#         .se     = round(ad_x$trans_se, 0),
#         .params = c(ad_x$c_pdf_a, ad_x$c_pdf_b, ad_x$c_pdf_c),
#         .trunc  = .setup$trunc_pdf
#       ), 0)
#     )
#
#     ## EF - Emissions Factors decomposed for each carbon pool
#     ## Carbon stock of initial land use
#     SIMS_CI <- mcs_c |>
#       dplyr::filter(.data$lu_id == ad_x$lu_initial_id) |>
#       dplyr::select("sim_no", "time_period", lu_id_i = "lu_id", c_form_i = "c_form", c_stock_i = "c_stock")
#
#     ## Carbon stock of final land use
#     SIMS_CF <- mcs_c |>
#       dplyr::filter(.data$lu_id == ad_x$lu_final_id) |>
#       dplyr::select("sim_no", "time_period", lu_id_f = "lu_id", c_form_f = "c_form", c_stock_f = "c_stock")
#
#     ## Combine AD and EF by land use and if needed time period
#     if (unique(SIMS_CI$period) == "ALL" &  unique(SIMS_CF$period) == "ALL") {
#
#       SIMS_CI <- SIMS_CI |> dplyr::select(-"time_period")
#       SIMS_CF <- SIMS_CF |> dplyr::select(-"time_period")
#
#       combi <- SIMS_AD |>
#         dplyr::left_join(SIMS_CI, by = "sim_no") |>
#         dplyr::left_join(SIMS_CF, by = "sim_no")
#
#     } else {
#
#       combi <- SIMS_AD |>
#         dplyr::left_join(SIMS_CI, by = c("sim_no", "trans_period" = "time_period")) |>
#         dplyr::left_join(SIMS_CF, by = c("sim_no", "trans_period" = "time_period"))
#
#     }
#
#     combi
#
#   }) |> purrr::list_rbind()
#   ## END LOOP
#
#   ## Re-arrange columns and add EF and E (emissions at transition level)
#   mcs_trans |>
#     dplyr::mutate(
#       EF = round((.data$c_stock_i - .data$c_stock_f) * 44/12, 3),
#       E_sim  = round(.data$AD * .data$EF, 0)
#     ) |>
#     dplyr::select(
#       "sim_no", "redd_activity", time_period = "trans_period", "trans_id",
#       "AD", "EF", "E_sim", "c_form_i", "c_stock_i", "c_form_f",
#       "c_stock_f", dplyr::everything()
#     )
#
# }



