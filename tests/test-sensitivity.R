
# ## FIND SH***
#
# tt <- list.files("R", full.names = T)
#
# purrr::walk(tt, function(x){
#
#   script <- readLines(x)
#   res <- stringr::str_detect(script, pattern = "magrittr")
#   res2 <- any(res)
#
#   if (res2) print(x)
#
# })


## KEEP COMMENT BUT RUN ONE TIME
# devtools::load_all()
#
# library(tidyverse)

## LOAD DATA
# path <- system.file("extdata/example1.xlsx", package = "mocaredd")
# path <- "inst/extdata/example1-4pools.xlsx"
#
# cs <- readxl::read_xlsx(path, sheet = "c_stocks", na = "NA")
# ad <- readxl::read_xlsx(path, sheet = "AD_lu_transitions", na = "NA")
# usr <- readxl::read_xlsx(path, sheet = "user_inputs", na = "NA")
# time <- readxl::read_xlsx(path, sheet = "time_periods", na = "NA")
#
# time <- time |> dplyr::mutate(nb_years = year_end - year_start + 1)
#
# usr$ci_alpha <- 1 - usr$conf_level
# usr$conf_level_txt = paste0(usr$conf_level * 100, "%")
#
#
# rv <- list()
# rv$inputs <- list()
# rv$mcs    <- list()
# rv$sens   <- list()
#
# rv$inputs$usr  <- usr
# rv$inputs$ad   <- ad
# rv$inputs$cs   <- cs
# rv$inputs$time <- time
#
# ## WHAT TO CHECK
# ## - Turn off each pool and compare with all EF
# ## - Turn off all EF
# ## - Turn off AD
# ## - Turn off one REDD+ activity and compare
# ## - Turn off REF vs MON
#
# ## Create overall_US function
# fct_overall_UA <- function(.ad, .cs, .time, .usr, .seed = NA){
#
#   ## !! For testing only
#   # .ad = ad
#   # .cs = cs
#   # .time = time
#   # .usr = usr
#   ## !!
#
#   if (!is.na(.seed)) set.seed(.seed)
#
#   ## LU TRANSITIONS
#   sim_trans <- fct_combine_mcs_E(.ad = .ad, .cs = .cs, .usr = .usr)
#
#   ## Annualize REDD+ level
#   if (usr$ad_annual) {
#     time_periods <- unique(.time$period_type)
#     sim_trans2 <- purrr::map(time_periods, function(x){
#       nb_years <- .time |>
#         dplyr::filter(period_type == x) |>
#         dplyr::pull("nb_years") |>
#         sum()
#       period_ids <- .time |>
#         dplyr::filter(period_type == x) |>
#         dplyr::pull("period_no")
#       sim_trans |>
#         dplyr::filter(time_period %in% period_ids) |>
#         dplyr::mutate(E_sim = round(E_sim / nb_years, 0))
#     }) |> purrr::list_rbind()
#   } else {
#     sim_trans2 <- sim_trans
#   }
#
#   sim_redd <- sim_trans2 |>
#     dplyr::group_by(.data$sim_no, .data$time_period, .data$redd_activity) |>
#     dplyr::summarise(E_sim = sum(.data$E_sim), .groups = "drop") |>
#     dplyr::left_join(.time, by = c("time_period" = "period_no")) |>
#     dplyr::mutate(redd_id = paste0(.data$period_type, " - ", .data$redd_activity))
#
#   ## AGGREGATES
#   sim_REF <- fct_combine_mcs_P(
#     .data = sim_trans,
#     .time = time,
#     .period_type = "REF",
#     .ad_annual = .usr$ad_annual
#   )
#   sim_MON <- fct_combine_mcs_P(
#       .data = sim_trans,
#       .time = time,
#       .period_type = "MON",
#       .ad_annual = .usr$ad_annual
#     )
#   sim_ER <- fct_combine_mcs_ER(.sim_ref = sim_REF, .sim_mon = sim_MON, .ad_annual = .usr$ad_annual)
#
#   res_REF <- fct_calc_res(.data = sim_REF, .sim = E_sim, .id = period_type, .ci_alpha = .usr$ci_alpha)
#   res_MON <- fct_calc_res(.data = sim_MON, .sim = E_sim, .id = period_type, .ci_alpha = .usr$ci_alpha)# |>
#     #dplyr::mutate(period_type = paste0("E-", .data$period_type))
#   res_ER <- fct_calc_res(.data = sim_ER, .sim = ER_sim, .id = period_type, .ci_alpha = .usr$ci_alpha) |>
#     dplyr::mutate(period_type = paste0("ER-", .data$period_type))
#
#   res_ER2 <- res_REF |>
#     dplyr::bind_rows(res_MON) |>
#     dplyr::bind_rows(res_ER)
#
#   res_redd  <- fct_calc_res(
#     .data = sim_redd,
#     .id = .data$redd_id,
#     .sim = .data$E_sim,
#     .ci_alpha = usr$ci_alpha
#   ) |>
#     dplyr::rename(period_type = "redd_id")
#
#   res_ER3 <- res_redd |>
#     dplyr::bind_rows(res_ER2) |>
#     dplyr::arrange(dplyr::desc(.data$period_type))
#
#   ## OUTPUT
#   list(
#     res_ER = res_ER3, sim_trans = sim_trans, sim_ER = sim_ER
#   )
#
# }
#
# ## SEED
# if (!is.na(rv$inputs$usr$ran_seed)) rv$inputs$usr$app_seed <- rv$inputs$usr$ran_seed else rv$inputs$usr$app_seed <- sample(1:100, 1)
# message("Seed for random simulations: ", rv$inputs$usr$app_seed)
#
# ## GET Overall U
# tictoc::tic()
# sens_all <- fct_overall_UA(
#   .ad = rv$inputs$ad,
#   .cs = rv$inputs$cs,
#   .time = rv$inputs$time,
#   .usr = rv$inputs$usr,
#   .seed = rv$inputs$usr$app_seed
#   )
# tictoc::toc()
# sens_all$res_ER
#
# ## U with no variability of AD or EF
# rv$inputs$ad_novar  <- rv$inputs$ad |> dplyr::mutate(trans_se = 0, trans_pdf = "normal")
# rv$inputs$cs_novar  <- rv$inputs$cs |> dplyr::mutate(c_se = 0, c_pdf = "normal")
# rv$inputs$cs_varAGB <- rv$inputs$cs |>
#   dplyr::mutate(
#     c_se  = dplyr::if_else(c_pool != "AGB", 0, .data$c_se),
#     c_pdf = dplyr::if_else(c_pool != "AGB", "normal", .data$c_pdf)
#   )
# rv$inputs$cs_varBGB <- rv$inputs$cs |>
#   dplyr::mutate(
#     c_se  = dplyr::if_else(c_pool != "BGB", 0, .data$c_se),
#     c_pdf = dplyr::if_else(c_pool != "BGB", "normal", .data$c_pdf)
#   )
#
#
# sens_varEF <- fct_overall_UA(
#   .ad = rv$inputs$ad_novar,
#   .cs = rv$inputs$cs,
#   .time = rv$inputs$time,
#   .usr = rv$inputs$usr,
#   .seed = rv$inputs$usr$app_seed
# )
# sens_varEF$res_ER
#
# sens_varAD <- fct_overall_UA(
#   .ad = rv$inputs$ad,
#   .cs = rv$inputs$cs_novar,
#   .time = rv$inputs$time,
#   .usr = rv$inputs$usr,
#   .seed = rv$inputs$usr$app_seed
# )
# sens_varAD$res_ER
#
# sens_varAGB <- fct_overall_UA(
#   .ad = rv$inputs$ad_novar,
#   .cs = rv$inputs$cs_varAGB,
#   .time = rv$inputs$time,
#   .usr = rv$inputs$usr,
#   .seed = rv$inputs$usr$app_seed
# )
# sens_varAGB$res_ER
#
# sens_varBGB <- fct_overall_UA(
#   .ad = rv$inputs$ad_novar,
#   .cs = rv$inputs$cs_varBGB,
#   .time = rv$inputs$time,
#   .usr = rv$inputs$usr,
#   .seed = rv$inputs$usr$app_seed
# )
# sens_varBGB$res_ER
#
# sens_all$res_ER
# sens_varAD$res_ER
# sens_varEF$res_ER
# sens_varAGB$res_ER
# sens_varBGB$res_ER
#
# ## Combine
# list_sens <- stringr::str_subset(ls(), pattern = "sens_")
# list_sens
#
# res_sens <- purrr::map(list_sens, function(x){
#
#   name_U <- paste0("U_", stringr::str_remove(x, "sens_"))
#
#   input <- get(x)$res_ER
#
#   if (name_U == "U_all") dplyr::select(input, period_type, !!name_U := "E_U") else dplyr::select(input, !!name_U := "E_U")
#
# }) |>
#   purrr::list_cbind()|>
#   dplyr::select("period_type", "U_all", "U_varAD", "U_varEF", dplyr::everything())
#
# res_sens
#
#
#
# res_sens |>
#   tidyr::pivot_longer(cols = dplyr::starts_with("U_"), names_to = "U_cat", values_to = "U_perc") |>
#   dplyr::filter(.data$U_cat %in% c("U_all", "U_varAD", "U_varEF")) |>
#   ggplot(aes(x = period_type)) +
#   geom_col(aes(y = U_perc, fill = U_cat), position = position_dodge()) +
#   scale_x_discrete(guide = guide_axis(n.dodge = 2)) +
#   theme_bw()
#
# res_sens |>
#   tidyr::pivot_longer(cols = dplyr::starts_with("U_"), names_to = "U_cat", values_to = "U_perc") |>
#   dplyr::filter(.data$U_cat %in% c("U_varAD", "U_varEF")) |>
#   ggplot(aes(x = period_type)) +
#   geom_col(aes(y = U_perc, fill = U_cat)) +
#   scale_x_discrete(guide = guide_axis(n.dodge = 2), limits = res_sens$period_type) +
#   theme_bw()
#
# res_sens |>
#   tidyr::pivot_longer(cols = dplyr::starts_with("U_"), names_to = "U_cat", values_to = "U_perc") |>
#   dplyr::filter(.data$U_cat %in% c("U_varAGB", "U_varBGB")) |>
#   ggplot(aes(x = period_type)) +
#   geom_col(aes(y = U_perc, fill = U_cat)) +
#   scale_x_discrete(guide = guide_axis(n.dodge = 2), limits = res_sens$period_type) +
#   theme_bw()

##
## Densities ###################################################################
##

# ## Group all data
# list_sens <- stringr::str_subset(ls(), pattern = "sens_")
# list_sens
#
# sim_sens <- purrr::map(list_sens, function(x){
#
#   U_cat <- paste0("U_", stringr::str_remove(x, "sens_"))
#
#   REF <- get(x)$sim_ER |>
#     dplyr::mutate(U_cat = U_cat) |>
#     dplyr::select(period_type = "period_type_R", "U_cat", SIMS = "E_sim_R") |>
#     dplyr::distinct()
#
#   MON <- get(x)$sim_ER |>
#     dplyr::mutate(U_cat = U_cat) |>
#     dplyr::select("period_type", "U_cat", SIMS = "E_sim")
#
#   dplyr::bind_rows(REF, MON)
#
# }) |>
#   purrr::list_rbind()
#
# ## AD vs EF
# tab_select <- sim_sens |>
#   dplyr::filter(.data$U_cat %in% c("U_all", "U_varAD", "U_varEF"))
#
# tab_select |>
#   ggplot(aes(SIMS)) +
#   geom_vline(xintercept = 0, linetype = "dotted") +
#   geom_density(aes(color = .data$U_cat), alpha = 0.5) +
#   facet_wrap(~period_type, ncol = 1) +
#   theme_bw()

