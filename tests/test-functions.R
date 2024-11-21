

# ## NEED TO CONVERT TO PROPER TESTING FROM TESTTHAT.
# ## SEE tests/testthat.
#
# library(tidyverse)
# library(readxl)
# library(mocaredd)
#
# ## LOAD DATA
# cs <- readxl::read_xlsx(system.file("extdata/example1.xlsx", package = "mocaredd"), sheet = "c_stocks", na = "NA")
# ad <- readxl::read_xlsx(system.file("extdata/example1.xlsx", package = "mocaredd"), sheet = "AD_lu_transitions", na = "NA")
# usr <- readxl::read_xlsx(system.file("extdata/example1.xlsx", package = "mocaredd"), sheet = "user_inputs", na = "NA")
# time <- readxl::read_xlsx(system.file("extdata/example1.xlsx", package = "mocaredd"), sheet = "time_periods", na = "NA")
#
# time <- time |> dplyr::mutate(nb_years = year_end - year_start + 1)
#
# usr$ci_alpha <- 1 - usr$conf_level
# usr$conf_level_txt = paste0(usr$conf_level * 100, "%")
#
#
# ##
# ## test whole calculation chain ######
# ##
#
# ari <- fct_arithmetic_mean(.ad = ad, .cs = cs, .usr = usr ,.time = time)
#
# sim_trans <- fct_combine_mcs_E(.ad = ad, .cs = cs, .usr = usr)
#
# ## Check
# tt <- sim_trans |> filter(sim_no == 1)
# tt
#
#
# res_trans <- sim_trans |> fct_calc_res(.id = trans_id, .sim = E_sim, .ci_alpha = usr$ci_alpha)
#
# gt_trans <- fct_forestplot(
#   .data = res_trans,
#   .id = trans_id,
#   .value = E,
#   .uperc = E_U,
#   .cilower = E_cilower,
#   .ciupper = E_ciupper,
#   .id_colname = "REDD+ Activity",
#   .conflevel = usr$conf_level_txt,
#   .filename = NA
#   )
#
# sim_redd <- sim_trans |>
#  dplyr::group_by(.data$sim_no, .data$time_period, .data$redd_activity) |>
#   dplyr::summarise(E_sim = sum(.data$E_sim), .groups = "drop")
#
# ## Check
# tt <- sim_redd |> filter(sim_no == 1)
# tt
# res_redd <- fct_calc_res(.data = sim_redd, .id = redd_activity, .sim = E_sim, .ci_alpha = usr$ci_alpha)
#
# ## FREL
# sim_REF <- fct_combine_mcs_P(
#   .data = sim_trans,
#   .time = time,
#   .period_type = "REF",
#   .ad_annual = usr$ad_annual
# )
#
# res_REF <- sim_REF |>
#   fct_calc_res(.id = period_type, .sim = E_sim, .ci_alpha = usr$ci_alpha)
#
# message("FREL is: ", res_REF$E, " ± ", res_REF$E_U, "%")
#
# ## Monitoring
# sim_MON <- fct_combine_mcs_P(
#     .data = sim_trans,
#     .time = time,
#     .period_type = "MON",
#     .ad_annual = usr$ad_annual
#   )
#
# res_MON <- sim_MON |>
#   fct_calc_res(.id = period_type, .sim = E_sim, .ci_alpha = usr$ci_alpha) |>
#   mutate(period_type = paste0("E-", period_type))
#
# sim_ER <- fct_combine_mcs_ER(.sim_ref = sim_REF, .sim_mon = sim_MON, .ad_annual = usr$ad_annual)
#
# res_ER <- sim_ER |>
#   fct_calc_res(.id = period_type, .sim = ER_sim, .ci_alpha = usr$ci_alpha) |>
#   mutate(period_type = paste0("ER-", period_type))

# ## Combine results
# gt_all <- res_REF |>
#   bind_rows(res_MON) |>
#   bind_rows(res_ER) |>
#   fct_forestplot(
#     .id = period_type,
#     .value = E,
#     .uperc = E_U,
#     .cilower = E_cilower,
#     .ciupper = E_ciupper,
#     .id_colname = "Period",
#     .conflevel = "90%",
#     .filename = NA
#   )
# gt_all



# sim_ER <- sim_FREL |>
#   bind_rows(sim_moni)

#
#
#
# res_ER <- sim_ER |>
#   fct_calc_res(.id = period_type, .sim = ER_sim, .ci_alpha = ci_alpha)
#
#
#
# tmp_ER <- time_clean |>
#   group_by(period_type) |>
#   summarise(
#     year_start = min(year_start),
#     year_end = max(year_end),
#     nb_years = sum(nb_years)
#   )
#
#
#
#
#
#
# res_ER2 <- tmp_ER |> inner_join(res_ER, by = join_by(period_type))
#
#
#
#
# gt_ER <- res_ER |> fct_forestplot(
#   .id = period_type,
#   .value = E,
#   .uperc = E_U,
#   .cilower = E_cilower,
#   .ciupper = E_ciupper,
#   .id_colname = "Monitoring period",
#   .conflevel = "90%",
#   .filename = NA
#   )
#
# gg_ER <- fct_histogram(
#   .dat = sim_ER,
#   .res = res_ER,
#   .id = period_type,
#   .value = ER_sim,
#   .value_type = "ER"
#   )

#gt::gtsave(gt_ER, filename = "test.png")

# ## !!! FOR TESTING INSIDE FUNCTIONS ONLY
# # .ad <- ad
# # .cs <- cs
# # .usr <- usr
# # .time <- time
# # .c_lu <- cs |> dplyr::filter(lu_id == "dg_ev_wet_closed")
# ## !!
#
# ## test fct_check_pool() and fct_make_formula()
# c_lu <- cs |> filter(lu_id == "ev_wet_closed")
#
# c_check <- fct_check_pool(.c_lu = c_lu, .c_unit = usr$c_unit, .c_fraction = usr$c_fraction)
# c_form  <- fct_make_formula(.c_check = c_check, .c_unit = usr$c_unit)
#
#
# ## test fct_make_mcs()
# c_sims <- c_lu |> filter(c_pool == "AGB")
#
# sims <- fct_make_mcs(.n_iter = 10000, .pdf = c_sims$c_pdf, .mean = c_sims$c_value, .se = c_sims$c_se, .trunc = F)
# hist(sims)
# median(sims)
# c_sims$c_value
#
# ## test fct_combine_mcs_C()
# res <- fct_combine_mcs_C(.usr = usr, .c_sub = .c_lu)
#
# check_cstock <- res |>
#   mutate(
#     C_all_check = eval(parse(text = c_form), res),
#     flag_cstock = C_all_check == C_all
#     )
#
# message("CSTOCK sims working: ", all(check_cstock$flag_cstock))
#
# ## Test fct_combine_mcs_E()
# sim_trans <- fct_combine_mcs_E(.ad = ad, .cs = cs, .usr = usr)
#
# ## Test fct_calc_res()
# res_trans <- fct_calc_res(.data = sim_trans, .id = trans_id, .sim = E_sim, .ci_alpha = ci_alpha)
#
# write_csv(res_trans, "tests/res_trans.csv")
#
# ## Test fct_forestplot()
# gt_trans <- res_trans |>
#   fct_forestplot(
#     .id = trans_id,
#     .value = E,
#     .uperc = E_U,
#     .cilower = E_cilower,
#     .ciupper = E_ciupper,
#     .id_colname = "Land use<br>transition code",
#     .conflevel = "90%",
#     .filename = "tests/gt_trans.png"
#   )
#
# gt_trans
#
#
# ##
# ## Comparison to arithmetic mean ######
# ##
#
# ad2 <- ad |> mutate(trans_se = 0)
# cs2 <- cs |>
#   mutate(
#     c_se = 0,
#     c_pdf = if_else(c_pdf != "normal" & !is.na(c_value), "normal", c_pdf)
#   )
# sim_trans <- fct_combine_mcs_all(.ad = ad2, .cs = cs2, .init = init, .usr = usr)
#
# res_trans <- sim_trans |> select(-sim_no) |> distinct()
#
# sim_FREL <- fct_combine_mcs_P(
#   .data = sim_trans,
#   .time = time,
#   .period_type = "reference",
#   .ad_annual = usr$ad_annual
# )
#
# mean_FREL <- sim_FREL |>
#   mutate(period_id = "FREL") |>
#   fct_calc_res(.id = period_id, .sim = E_sim, .ci_alpha = ci_alpha)
#
# message("Arithmetic FREL is: ", mean_FREL$E, " ± ", mean_FREL$E_U, "%")
#
#
#
# ##
# ## TEST NO FUNCTION ######
# ##
#
# ## trans to redd+ acti
# sim_redd <- sim_trans |>
#   group_by(sim_no, redd_activity, time_period) |>
#   summarize(E_sim = sum(E_sim), .groups = "drop")
#
# res_redd <- sim_redd |>
#   mutate(redd_id = paste0(time_period, " - ", redd_activity)) |>
#   fct_calc_res(.id = redd_id, .sim = E_sim, .ci_alpha = ci_alpha)
#
# gt_redd <- fct_forestplot(
#   .data = res_redd,
#   .id = redd_id,
#   .value = E,
#   .uperc = E_U,
#   .cilower = E_cilower,
#   .ciupper = E_ciupper,
#   .id_colname = "REDD+ activity<br>per time period",
#   .conflevel = "90%",
#   .filename = "tests/gt_redd.png"
# )
#
# gt_redd
#
# ## redd+ acti to time period
# sim_period <- sim_redd |>
#   group_by(sim_no, time_period) |>
#   summarise(E_sim = sum(E_sim), .groups = "drop")
#
# res_period <- sim_period |>
#   group_by(time_period) |>
#   summarise(
#     E = round(median(E_sim)),
#     E_ciupper = round(quantile(E_sim, 1 - ci_alpha/2)),
#     E_cilower = round(quantile(E_sim, ci_alpha/2)),
#     .groups = "drop"
#   ) |>
#   mutate(
#     E_ME  = round((E_ciupper - E_cilower) / 2),
#     E_U   = round(E_ME / E * 100),
#   ) |>
#   select(time_period, E, E_U, E_ME, E_cilower, E_ciupper)
#
# gt_period <- res_period |>
#   fct_forestplot(
#     .id = time_period,
#     .value = E,
#     .uperc = E_U,
#     .cilower = E_cilower,
#     .ciupper = E_ciupper,
#     .id_colname = "Time period",
#     .conflevel = "90%",
#     .filename = "tests/gt_period.png"
#   )
#
# gt_period
#
#
# sim_FREL <- fct_combine_mcs_P(
#   .data = sim_trans,
#   .time = time,
#   .period_type = "reference",
#   .ad_annual = usr$ad_annual
#   )
#
# res_FREL <- sim_FREL |>
#   mutate(period_id = "FREL") |>
#   fct_calc_res(.id = period_id, .sim = E_sim, .ci_alpha = ci_alpha)
#
# message("FREL is: ", res_FREL$E, " ± ", res_FREL$E_U, "%")
#
#
# ## Arithmetic mean
# ad2 <- ad |> mutate(trans_se = 0)
# cs2 <- cs |> mutate(c_se = 0)
# res_trans <- fct_combine_mcs_all(.ad = ad2, .cs = cs2, .init = init, .usr = usr)
# res_redd  <- res_trans
#
#
# ##
# ## Monitoring ######
# ##
#
# time_mon <- time |> filter(period_type == "monitoring")
# nb_ref <- length(unique(time_mon$period_combinations))
#
# res_res <- map(time_mon$period_no, function(x){
#
#   ## !! FOR TESTING ONLY
#   # x = "T2"
#   ## !!
#
#   if (usr$ad_annual) {
#     nb_years <- time |> filter(period_no == x) |> pull(nb_years)
#   } else {
#     nb_years <- 1
#   }
#
#   res_p <- res_redd |>
#     filter(time_period == x) |>
#     group_by(sim_no) |>
#     summarise(E_ref = sum(E_redd) / nb_years) |>
#     mutate(period_no = x) |>
#     left_join(time, by = join_by(period_no))
#
#   res_p
#
# }) |> list_rbind()
#
# res_res
#
# ## Combine multiple periods used for one reference period
# if (nrow(time_mon) > 1) {
#
#   ref_length <- sum(time_ref$nb_years)
#
#   res_res2 <- res_res |>
#     group_by(sim_no) |>
#     summarise(E_ref_total = sum(E_ref * nb_years) / ref_length)
#
# } else {
#
#   res_ref2 <- res_ref
#
# }
#
# E <- res_ref2$E_ref_total
#
# FREL <- round(median(E))
# FREL
#
# FREL_ME <- as.numeric((quantile(E, 1 - ci_alpha/2) - quantile(E, ci_alpha/2)) / 2)
# FREL_U  <- round(FREL_ME / FREL * 100, 0)
#
# message("FREL is: ", FREL, " ± ", FREL_U, "%")


# ## !!! FOR TESTING INSIDE FUNCTIONS ONLY
# # .ad <- ad
# # .cs <- cs
# # .usr <- usr
# # .time <- time
# # .c_lu <- cs |> dplyr::filter(lu_id == "dg_ev_wet_closed")
# ## !!
#
# ## test fct_check_pool() and fct_make_formula()
# c_lu <- cs |> filter(lu_id == "ev_wet_closed")
#
# c_check <- fct_check_pool(.c_lu = c_lu, .c_unit = usr$c_unit, .c_fraction = usr$c_fraction)
# c_form  <- fct_make_formula(.c_check = c_check, .c_unit = usr$c_unit)
#
#
# ## test fct_make_mcs()
# c_sims <- c_lu |> filter(c_pool == "AGB")
#
# sims <- fct_make_mcs(.n_iter = 10000, .pdf = c_sims$c_pdf, .mean = c_sims$c_value, .se = c_sims$c_se, .trunc = F)
# hist(sims)
# median(sims)
# c_sims$c_value
#
# ## test fct_combine_mcs_C()
# res <- fct_combine_mcs_C(.usr = usr, .c_sub = .c_lu)
#
# check_cstock <- res |>
#   mutate(
#     C_all_check = eval(parse(text = c_form), res),
#     flag_cstock = C_all_check == C_all
#     )
#
# message("CSTOCK sims working: ", all(check_cstock$flag_cstock))
#
# ## Test fct_combine_mcs_E()
# sim_trans <- fct_combine_mcs_E(.ad = ad, .cs = cs, .usr = usr)
#
# ## Test fct_calc_res()
# res_trans <- fct_calc_res(.data = sim_trans, .id = trans_id, .sim = E_sim, .ci_alpha = ci_alpha)
#
# write_csv(res_trans, "tests/res_trans.csv")
#
# ## Test fct_forestplot()
# gt_trans <- res_trans |>
#   fct_forestplot(
#     .id = trans_id,
#     .value = E,
#     .uperc = E_U,
#     .cilower = E_cilower,
#     .ciupper = E_ciupper,
#     .id_colname = "Land use<br>transition code",
#     .conflevel = "90%",
#     .filename = "tests/gt_trans.png"
#   )
#
# gt_trans
#
#
