#
# fct_combine_mcs_E <- function(.ad, .cs, .usr){
#
#   ## !!! FOR TESTING ONLY - run example then assign ad, cs and usr to the input vars.
#   # .ad <- ad
#   # .cs <- cs
#   # .usr <- usr
#   ## !!!
#
#
#   ## TOC ####
#   ## Calculation steps:
#
#   ## 1. Preparation of data:
#   ## 1.1. For Cstock, make formula based on c_elements reported
#   ## 1.2. Adjust formula for degradation ratio
#
#   ## 2. Make simulations for all PDFs
#   ## 2.1. Simulate carbon fraction (CF), if AGB and/or BGB expressed as dry matter (DM)
#   ## 2.2. Simulate all C elements in Cstock table.
#   ## 2.3. Simulate AD for each time period and land use change
#
#   ## 3. Prepare SIMS for combining AD and EF
#   ## 3.1. Make long table with 1 row a unique combination of sim_no, time_period and lu_id or trans_id
#   ## 3.2. Add formulas and Cstock to CEL sims (ex. DG_ratio)
#   ## 3.2.1. If DG_ratio used calulate Cstock of other land uses
#   ## 3.2.2. Get Cstock of intact land uses associated with degradation
#   ## 3.2.3. Add C of pools excluded from degradation
#   ## 3.2.4. Combine intact C with DG_ratio and calculate Cstock
#   ## 3.2.5. Combine all land uses
#
#   ## 4. Make Cstock initial and final for each LU transition
#
#
#
#
#   ## 2. Make master data frame with all simulations per land use transition and time period
#   ## 2.1 Make long table with each row is a unique combination of time period, land use or land use transition and simulation number
#   ## 2.2 for Cstock, make formula based on c_elements reported
#
#
#   ## 3. Calculate Cstock for each time period and land use change
#   ## 3.1. Get Cstock formula from elements in the data.
#   ## 3.2. Calculate Cstock based on the formula for each simulation
#   ## 3.3. Calculate degraded C stocks if based on degradation ratios DG_ratio
#   ## 4. Calculate E as ADxEF
#
#
#   ##
#   ## 1. Preparation of data ####################################################
#   ##
#
#   ## + 1.1. Make Cstock formula from C elements in 'cs' table ####
#   c_formula <- .cs |>
#     #dplyr::filter(.data$c_element != "DG_ratio") |>
#     dplyr::summarise(c_el = list(c(.data$c_element)), .by = c("c_period", "c_lu_id")) |>
#     dplyr::rowwise() |>
#     dplyr::mutate(
#       c_form = fct_make_formula(.c_el = .data$c_el, .c_unit = .usr$c_unit)
#     ) |>
#     dplyr::ungroup() |>
#     dplyr::select(time_period = "c_period", lu_id = "c_lu_id", "c_form")
#
#
#   ## + 1.2. correct degradation formula if degradation elements with ratio ####
#   degrat_lu <- .cs |>
#     dplyr::filter(.data$c_element == "DG_ratio") |>
#     dplyr::pull("c_lu_id")
#
#   degrat_lu_intact <- stringr::str_remove(degrat_lu, .usr$dg_ext)
#
#   if (length(degrat_lu) > 0) {
#     c_formula <- c_formula |>
#       dplyr::mutate(
#         c_form = dplyr::case_when(
#           .data$lu_id %in% degrat_lu & .usr$dg_pool == "ALL" ~ "DG_ratio * c_intact",
#           .data$lu_id %in% degrat_lu & .usr$dg_pool != "ALL" ~ "DG_ratio * c_intact + (1 - DG_ratio) * c_intact_noDG",
#           TRUE ~ c_form
#         )
#       )
#   }
#
#
#   ##
#   ## 2. Make simulations for all PDFs ##########################################
#   ##
#
#   ## + 2.1. simulate CF ####
#
#   if (is.numeric(.usr$c_fraction) & .usr$c_unit == "DM") {
#     sims_CF <- dplyr::tibble(
#       sim_no = 1:.usr$n_iter,
#       CF = round(fct_make_mcs(
#         .n_iter = .usr$n_iter,
#         .pdf    = .usr$c_fraction_pdf,
#         .mean   = round(.usr$c_fraction, 3),
#         .se     = round(.usr$c_fraction_se, 3),
#         #.params = c(params$c_pdf_a, params$c_pdf_b, params$c_pdf_c),
#         .trunc  = .usr$trunc_pdf
#       ), 3)
#     )
#   } else {
#     sims_CF <- dplyr::tibble(
#       sim_no = 1:.usr$n_iter,
#       CF     = rep("NA", .usr$n_iter)
#     )
#   }
#
#
#   ## + 2.2. Make simulations for each carbon element as list columns ####
#   sims_CEL <- .cs |>
#     dplyr::filter(!(is.na(.data$c_value) & is.na(.data$c_pdf_a))) |>
#     dplyr::rowwise() |>
#     dplyr::mutate(
#       params_not_norm = list(round(c(.data$c_pdf_a, .data$c_pdf_b, .data$c_pdf_c), 3)),
#       SIMS = list(fct_make_mcs(
#         .n_iter = .usr$n_iter,
#         .pdf    = .data$c_pdf,
#         .mean   = round(.data$c_value, 3),
#         .se     = round(.data$c_se, 3),
#         .params = .data$params_not_norm,
#         .trunc  = .usr$trunc_pdf
#       ))
#     ) |>
#     dplyr::ungroup()
#
#
#   ## + 2.3. Simulate AD for each time period and land use change ####
#
#   sims_AD <- .ad |>
#     dplyr::filter(!(is.na(.data$trans_area) & is.na(.data$trans_pdf_a))) |>
#     dplyr::rowwise() |>
#     dplyr::mutate(
#       params_not_norm = list(round(c(.data$trans_pdf_a, .data$trans_pdf_b, .data$trans_pdf_c), 3)),
#       SIMS = list(fct_make_mcs(
#         .n_iter = .usr$n_iter,
#         .pdf    = .data$trans_pdf,
#         .mean   = round(.data$trans_area, 3),
#         .se     = round(.data$trans_se, 3),
#         .params = .data$params_not_norm,
#         .trunc  = .usr$trunc_pdf
#       ))
#     ) |>
#     dplyr::ungroup()
#
#
#   ##
#   ## 3. Prepare SIMS for combining AD and EF ####
#   ##
#
#   ## + 3.1. Make long table with 1 row a unique combination of sim_no, time_period and lu_id or trans_id ####
#
#   sims_AD_long <- sims_AD |>
#     dplyr::select(time_period = "trans_period", "trans_id", "lu_initial_id", "lu_final_id", "redd_activity", AD = "SIMS") |>
#     tidyr::unnest("AD") |>
#     dplyr::mutate(sim_no = rep(1:.usr$n_iter, nrow(sims_AD)))
#
#   sims_CEL_long <- sims_CEL |>
#     dplyr::select(time_period = "c_period", lu_id = "c_lu_id", "c_element", "SIMS") |>
#     tidyr::unnest("SIMS") |>
#     dplyr::mutate(sim_no = rep(1:.usr$n_iter, nrow(sims_CEL))) |>
#     tidyr::pivot_wider(names_from = "c_element", values_from = "SIMS") |>
#     dplyr::left_join(sims_CF, by = "sim_no")
#
#   ## + 3.2. Add formulas and Cstock to CEL sims (ex. DG_ratio) ####
#
#   if ("DG_ratio" %in% .cs$c_element) {
#
#     ## ++ 3.2.1. If DG_ratio used calulate Cstock of other land uses ####
#     sims_C_nodegrat <- sims_CEL_long |>
#       dplyr::filter(is.na(.data$DG_ratio)) |>
#       dplyr::left_join(c_formula, by = c("time_period", "lu_id")) |>
#       dplyr::rowwise() |>
#       dplyr::mutate(c_stock = round(eval(parse(text = .data$c_form)), 3)) |>
#       dplyr::ungroup()
#
#     ## ++ 3.2.2. Get Cstock of intact land uses associated with degradation ####
#     sims_C_lu_intact <- sims_C_nodegrat |>
#       dplyr::filter(.data$lu_id %in% degrat_lu_intact) |>
#       dplyr::select("time_period", lu_intact = "lu_id", "sim_no", c_intact = "c_stock")
#
#     ## ++ 3.2.3. Add C of pools excluded from degradation ####
#     if (.usr$dg_pool == "ALL") {
#       sims_C_lu_intact_expool <- sims_C_lu_intact |>
#         dplyr::mutate(c_intact_noDG = NA)
#     } else {
#       dg_expool <- .cs |>
#         dplyr::filter(.data$c_lu_id %in% degrat_lu_intact) |>
#         dplyr::pull("c_element") |>
#         unique()
#
#       sims_C_lu_intact_expool <- sims_C_nodegrat |>
#         dplyr::filter(.data$lu_id %in% degrat_lu_intact) |>
#         dplyr::select("time_period", lu_intact = "lu_id", "sim_no", c_intact = "c_stock", !!!rlang::syms(dg_expool)) |>
#         dplyr::rowwise() |>
#         dplyr::mutate(c_intact_noDG = sum(!!!rlang::syms(dg_expool))) |>
#         dplyr::ungroup() |>
#         dplyr::select("time_period", "lu_intact", "sim_no", "c_intact", "c_intact_noDG")
#     }
#
#     ## ++ 3.2.4. Combine intact C with DG_ratio and calculate Cstock ####
#     sims_C_degrat <- sims_CEL_long |>
#       dplyr::filter(!is.na(.data$DG_ratio)) |>
#       dplyr::mutate(lu_intact = stringr::str_remove(.data$lu_id, .usr$dg_ext)) |>
#       dplyr::left_join(sims_C_lu_intact_expool, by = c("time_period", "lu_intact", "sim_no")) |>
#       dplyr::left_join(c_formula, by = c("time_period", "lu_id")) |>
#       dplyr::rowwise() |>
#       dplyr::mutate(c_stock = round(eval(parse(text = .data$c_form)), 3)) |>
#       dplyr::ungroup()
#
#     ## ++ 3.2.5. Combine all land uses ####
#     sims_C <- sims_C_nodegrat |>
#       dplyr::bind_rows(sims_C_degrat) |>
#       dplyr::select(dplyr::where(~ !all(is.na(.)))) |>
#       dplyr::select("time_period", "lu_id", "sim_no", "c_stock", "c_form", dplyr::everything())
#
#     ## CHECK
#     ## tt <- sims_C |> filter(sim_no == 1)
#
#   } else {
#
#     sims_C <- sims_CEL_long |>
#       dplyr::left_join(c_formula, by = c("time_period", "lu_id")) |>
#       dplyr::rowwise() |>
#       dplyr::mutate(c_stock = round(eval(parse(text = .data$c_form)), 3)) |>
#       dplyr::ungroup()
#
#   }
#
#   ##
#   ## 4. Make Cstock initial and final for each LU transition ####
#   ##
#
#   if (all(.cs$c_period == "ALL")) {
#
#     c_cols <- names(sims_C)[!names(sims_C) %in% c("time_period", "lu_id", "sim_no")]
#
#     sims_CI <- sims_C |>
#       dplyr::select(-"time_period") |>
#       dplyr::rename_with(.cols = dplyr::all_of(c_cols), paste0, "_i")
#
#     sims_CF <- sims_C |>
#       dplyr::select(-"time_period") |>
#       dplyr::rename_with(.cols = dplyr::all_of(c_cols), paste0, "_f")
#
#     sims_E <- sims_AD_long |>
#       dplyr::left_join(sims_CI, by = c("lu_initial_id" = "lu_id", "sim_no")) |>
#       dplyr::left_join(sims_CF, by = c("lu_final_id"   = "lu_id", "sim_no")) |>
#       dplyr::mutate(
#         EF = round(.data$c_stock_i - .data$c_stock_f, 3),
#         E = round(.data$AD * .data$EF, 3)
#         ) |>
#       dplyr::select(
#         "time_period", "trans_id", "lu_initial_id", "lu_final_id", "redd_activity",
#         "sim_no", "E", "AD", "EF", "c_stock_i", "c_stock_f", dplyr::everything()
#         )
#
#   } else {
#
#     ## TBD
#     ## Get annual AD
#     ## get initial CS at beginning of period, final CS and delta CS per year.
#     ## Reconstruct AD and EF for est period REF adn MON_X
#
#   }
#
# } ## END FUNCTION
#
#
#
#
#   # ## + Make long table with C elements as columns
#   # sims_cols_noDG <- sims_cols |> dplyr::filter(.data$c_element != "DG_ratio")
#   #
#   # sims_C_noDG <- sims_cols_noDG |>
#   #   dplyr::select(period = "c_period", lu_id = "c_lu_id", "c_element", "SIMS") |>
#   #   tidyr::unnest("SIMS") |>
#   #   dplyr::mutate(sim_no = rep(1:.usr$n_iter, nrow(sims_cols_noDG))) |>
#   #   tidyr::pivot_wider(names_from = "c_element", values_from = "SIMS") |>
#   #   dplyr::left_join(sims_CF, by = "sim_no")
#   #
#   # ## CHECK
#   # # sims_cols[1,]
#   # # hist(sims_cols$SIMS[[1]])
#   # # hist(sims_C_noDG$AGB[1:.usr$n_iter])
#   #
#   # ## 3. Calculate C stock - no degradation from ratios ####
#   #
#   # ## + Get C elements for each period x lu
#   # c_elements <- .cs |>
#   #   dplyr::filter(.data$c_element != "DG_ratio") |>
#   #   dplyr::summarise(c_el = list(c(.data$c_element)), .by = c("c_period", "c_lu_id")) |>
#   #   dplyr::rowwise() |>
#   #   dplyr::mutate(
#   #     c_form = fct_make_formula(.c_el = .data$c_el, .c_unit = .usr$c_unit)
#   #   ) |>
#   #   dplyr::ungroup() |>
#   #   dplyr::select(period = "c_period", lu_id = "c_lu_id", "c_form")
#   #
#   # sims_C_noDG_calc <- sims_C_noDG |>
#   #   dplyr::left_join(c_elements, by = c("time_period", "lu_id")) |>
#   #   dplyr::rowwise() |>
#   #   dplyr::mutate(c_stock = round(eval(parse(text = .data$c_form)), 3)) |>
#   #   dplyr::ungroup() |>
#   #   dplyr::select("sim_no", tidyr::everything())
#   #
#   #
#   # ## 4. Calculate C for degraded land uses using ratio ####
#   #
#   # if (!"DG_ratio" %in% unique(sims_cols$c_element)) {
#   #
#   #   return(sims_C_noDG_calc)
#   #
#   # } else {
#   #
#   #   ## + Get C elements used for DG
#   #   if (.usr$dg_pool == "ALL") {
#   #     dg_pool <- "c_stock"
#   #   } else {
#   #     dg_pool <- stringr::str_split(.usr$dg_pool, pattern = ",") |>
#   #       purrr::map(stringr::str_trim) |>
#   #       unlist()
#   #   }
#   #
#   #   dg_pool_before <- paste0(dg_pool, "_before")
#   #
#   #   ## + Get intact land use that are degraded
#   #   dg_lu        <- stringr::str_subset(.cs$c_lu_id, pattern = .usr$dg_ext)
#   #   dg_lu_before <- stringr::str_remove(dg_lu, pattern = .usr$dg_ext)
#   #
#   #   ## + Get simulated Cstocks before degradation
#   #   sims_C_before <- sims_C_noDG_calc |>
#   #     dplyr::filter(.data$lu_id %in% dg_lu_before) |>
#   #     purrr::discard(~all(is.na(.))) |>
#   #     dplyr::rename_with(.cols = tidyr::all_of(dg_pool), .fn = paste0, "_before") |>
#   #     dplyr::select("sim_no", "time_period", lu_before = "lu_id", tidyr::ends_with("_before"))
#   #
#   #   ## GS: NOT NEEDED / TO BE CHECKED USING DIRECTLY 'dg_pool'
#   #   ## + Check if some pools are not affected by degradation
#   #   # if (.usr$dg_pool == "ALL") {
#   #   #   dg_excluded_pool <- NA
#   #   # } else {
#   #   #   dg_potential_pool <- .cs |>
#   #   #     filter(c_lu_id %in% lu_intact) |>
#   #   #     pull(c_element) |>
#   #   #     unique()
#   #   #   dg_excluded_pool <- dg_potential_pool[!dg_potential_pool %in% dg_pool]
#   #   #   ## GS: May need to revise when processing Ghana template
#   #   # }
#   #
#   #   ## + Get DG_ratio simulations as long table
#   #   sims_cols_DG <- sims_cols |> dplyr::filter(.data$c_element == "DG_ratio")
#   #
#   #   sims_DG <- sims_cols_DG |>
#   #     dplyr::select(period = "c_period", lu_id = "c_lu_id", "c_element", "SIMS") |>
#   #     tidyr::unnest("SIMS") |>
#   #     dplyr::mutate(sim_no = rep(1:.usr$n_iter, nrow(sims_cols_DG))) |>
#   #     tidyr::pivot_wider(names_from = "c_element", values_from = "SIMS") |>
#   #     dplyr::mutate(lu_before = stringr::str_remove(.data$lu_id, .usr$dg_ext))
#   #
#   #   ## + Join intact pools C to degraded land uses, remake formula and calculate new Cstock
#   #   sims_DG_calc <- sims_DG |>
#   #     dplyr::left_join(sims_C_before, by = c("sim_no", "time_period", "lu_before")) |>
#   #     dplyr::rowwise() |>
#   #     dplyr::mutate(
#   #       c_form = paste0("DG_ratio * (", paste0(dg_pool_before, collapse = " + "), ")"),
#   #       c_stock  = round(.data$DG_ratio * sum(!!!rlang::syms(dg_pool_before)), 3)
#   #     ) |>
#   #     dplyr::ungroup() |>
#   #     dplyr::select(-"lu_before") |>
#   #     dplyr::select("sim_no", tidyr::everything())
#   #
#   #
#   #   ## Output
#   #   dplyr::bind_rows(sims_C_noDG_calc, sims_DG_calc)
#
#
#
#
# #   ## Seed for random simulation
# #   ## Implemented outside function now
# #   # if (!is.na(.usr$ran_seed)){
# #   #   set.seed(.usr$ran_seed)
# #   #   message("Random simulations with seed: ", .usr$ran_seed)
# #   # } else {
# #   #   app_ran_seed <- sample(1:100, 1)
# #   #   set.seed(app_ran_seed)
# #   #   message("Seed for random simulations: ", app_ran_seed)
# #   # }
# #
# #   ## Get all Cstock simulations
# #   mcs_c <- fct_combine_mcs_cstock(.ad = .ad, .cs = .cs, .usr = .usr)
# #
# #   ## Get all land use transition
# #   vec_trans <- unique(.ad$trans_id)
# #
# #   ## For each transition, calculate simulations for each element of the calculation chain
# #   mcs_trans <- purrr::map(vec_trans, function(x){
# #
# #     ## !! FOR TESTING ONLY
# #     # x = "T1_H_H_deg" #"T1_P_Crop" #"T1_ev_wet_closed_dg_ev_wet_closed"
# #     # x = "T1_EV_Crop"
# #     ## !!
# #
# #     ad_x   <- .ad |> dplyr::filter(.data$trans_id == x)
# #     redd_x <- ad_x$redd_activity
# #
# #     ## AD - Activity Data
# #     SIMS_AD <- dplyr::tibble(
# #       sim_no = 1:.usr$n_iter,
# #       redd_activity = ad_x$redd_activity,
# #       trans_id = ad_x$trans_id,
# #       trans_period = ad_x$trans_period,
# #       AD = round(fct_make_mcs(
# #         .n_iter = .usr$n_iter,
# #         .pdf    = ad_x$trans_pdf,
# #         .mean   = round(ad_x$trans_area, 0),
# #         .se     = round(ad_x$trans_se, 0),
# #         .params = c(ad_x$c_pdf_a, ad_x$c_pdf_b, ad_x$c_pdf_c),
# #         .trunc  = .usr$trunc_pdf
# #       ), 0)
# #     )
# #
# #     ## EF - Emissions Factors decomposed for each carbon pool
# #     ## Carbon stock of initial land use
# #     SIMS_CI <- mcs_c |>
# #       dplyr::filter(.data$lu_id == ad_x$lu_initial_id) |>
# #       dplyr::select("sim_no", "time_period", lu_id_i = "lu_id", c_form_i = "c_form", c_stock_i = "c_stock")
# #
# #     ## Carbon stock of final land use
# #     SIMS_CF <- mcs_c |>
# #       dplyr::filter(.data$lu_id == ad_x$lu_final_id) |>
# #       dplyr::select("sim_no", "time_period", lu_id_f = "lu_id", c_form_f = "c_form", c_stock_f = "c_stock")
# #
# #     ## Combine AD and EF by land use and if needed time period
# #     if (unique(SIMS_CI$period) == "ALL" &  unique(SIMS_CF$period) == "ALL") {
# #
# #       SIMS_CI <- SIMS_CI |> dplyr::select(-"time_period")
# #       SIMS_CF <- SIMS_CF |> dplyr::select(-"time_period")
# #
# #       combi <- SIMS_AD |>
# #         dplyr::left_join(SIMS_CI, by = "sim_no") |>
# #         dplyr::left_join(SIMS_CF, by = "sim_no")
# #
# #     } else {
# #
# #       combi <- SIMS_AD |>
# #         dplyr::left_join(SIMS_CI, by = c("sim_no", "trans_period" = "time_period")) |>
# #         dplyr::left_join(SIMS_CF, by = c("sim_no", "trans_period" = "time_period"))
# #
# #     }
# #
# #     combi
# #
# #   }) |> purrr::list_rbind()
# #   ## END LOOP
# #
# #   ## Re-arrange columns and add EF and E (emissions at transition level)
# #   mcs_trans |>
# #     dplyr::mutate(
# #       EF = round((.data$c_stock_i - .data$c_stock_f) * 44/12, 3),
# #       E_sim  = round(.data$AD * .data$EF, 0)
# #     ) |>
# #     dplyr::select(
# #       "sim_no", "redd_activity", time_period = "trans_period", "trans_id",
# #       "AD", "EF", "E_sim", "c_form_i", "c_stock_i", "c_form_f",
# #       "c_stock_f", dplyr::everything()
# #     )
# #
# # }
#
#
#
