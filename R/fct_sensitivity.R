#
#
#
# fct_sensitivity <- function(.sim_trans, .usr, .time){
#
#   ## FOR TESTING ONLY
#   # .sim_trans <- sim_trans
#   # .usr  <- usr
#   # .time <- time_clean
#   ##
#
#   ## REDD+ activity
#   redd_acti     <- sort(unique(.sim_trans$redd_activity))
#   # time_p        <- sort(unique(.sim_trans$time_period))
#   # lu_initial_id <- sort(unique(.sim_trans$lu_initial_id))
#   # lu_final_id   <- sort(unique(.sim_trans$lu_final_id))
#   # trans_ids     <- sort(unique(.sim_trans$trans_id))
#
#   ## DF vs DG
#   if ("DF" %in% redd_acti){
#
#     DF_trans_ids <- .sim_trans |> filter(redd_activity == "DF") |> pull(trans_id) |> unique()
#
#     ## REPLACE ALL SIMS DF BY MEDIAN
#     sim_trans_noDF_medianE <- map(DF_trans_ids, function(x){
#
#       E_filter <- .sim_trans |> filter(trans_id == x)
#
#       E_median <- median(E_filter$E)
#
#       E_filter |> mutate(E = E_median)
#
#     }) |> list_rbind()
#
#     #tt <- sim_trans_noDF |> filter(sim_no <= 5)
#
#     sim_trans_noDF <- .sim_trans |>
#       filter(!trans_id %in% DF_trans_ids) |>
#       bind_rows(sim_trans_noDF)
#
#     sim_REF <- sim_trans_noDF |>
#       fct_combine_mcs_P(.time = .time, .period_type = "REF", .ad_annual = .usr$ad_annual)
#
#     sim_MON <- sim_trans_noDF |>
#       fct_combine_mcs_P(.time = .time, .period_type = "MON", .ad_annual = .usr$ad_annual)
#
#     sim_ER <- fct_combine_mcs_ER(.sim_ref = sim_REF, .sim_mon = sim_MON, .ad_annual = .usr$ad_annual)
#
#     res_ER <- fct_calc_res(
#       .data = sim_ER,
#       .id = .data$period_type,
#       .sim = .data$ER_sim,
#       .ci_alpha = 1 - .usr$conf_level
#     )
#     res_ER
#
#
#     sim_REF <- .sim_trans |>
#       fct_combine_mcs_P(.time = .time, .period_type = "REF", .ad_annual = .usr$ad_annual)
#
#     sim_MON <- .sim_trans |>
#       fct_combine_mcs_P(.time = .time, .period_type = "MON", .ad_annual = .usr$ad_annual)
#
#     sim_ER <- fct_combine_mcs_ER(.sim_ref = sim_REF, .sim_mon = sim_MON, .ad_annual = .usr$ad_annual)
#
#     res_ER <- fct_calc_res(
#       .data = sim_ER,
#       .id = .data$period_type,
#       .sim = .data$ER_sim,
#       .ci_alpha = 1 - .usr$conf_level
#     )
#     res_ER
#
#
#
#   }
#
#
#
#
#
# }
