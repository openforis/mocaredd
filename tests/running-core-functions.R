

# devtools::load_all()

path <- "tests/mocaredd-CUA-S4-V2_d2-details.xlsx"
path <- "inst/extdata/mocaredd-templatev2-4pools.xlsx"
path <- "inst/extdata/mocaredd-templatev2-4pools-DM.xlsx"

rv <- list(checks = list(), inputs = list(), sims = list(), res = list())

check_result <- fct_checkinput(.path = path, .minislow = 0.2)


rv$checks$all_ok <- isTRUE(check_result$all_ok)
rv$checked <- check_result       # full fct_checkinput() output (incl. template_version)
rv$inputs  <- check_result$data  # tables, for value boxes and period/ER aggregation

## FOR TESTS
.checked_data <- rv$checked
.setup  <- rv$inputs$setup
.time   <- rv$inputs$time
.carbon <- rv$inputs$carbon
.area   <- rv$inputs$area

rv$checks$ari_res <- fct_arithmetic_mean2(.checked_data = rv$checked)


## TEST GRAPHS ARITHMETIC MEAN ######

## Run internal of fct_arithmetic_mean2()
er_blocks <- out_yearly_mon |>
  dplyr::summarise(
    xmin     = min(.data$year) - 0.5,
    xmax     = max(.data$year) + 0.5,
    nb_years = dplyr::n(),
    E        = dplyr::first(.data$E),
    FREL     = dplyr::first(.data$FREL),
    .by = c("period_type")
  ) |>
  dplyr::left_join(er_stat, by = "period_type") |>
  dplyr::mutate(ER_tot = .data$ER_y * .data$nb_years)

out_gg <- out_yearly |>
  ggplot2::ggplot(ggplot2::aes(x = .data$year)) +
  ## 1. emission reductions as a volume --------------------------------------
ggplot2::geom_rect(
  data = er_blocks, inherit.aes = FALSE,
  ggplot2::aes(xmin = .data$xmin, xmax = .data$xmax, ymin = .data$E, ymax = .data$FREL),
  fill = "limegreen", alpha = 0.3
) +
  ggplot2::geom_text(
    data = er_blocks, inherit.aes = FALSE,
    ggplot2::aes(x = (.data$xmin + .data$xmax) / 2, y = (.data$E + .data$FREL) / 2,
                 label = paste0(round(.data$ER_tot, 2), "\nMtCO2e")),
    colour = "darkgreen", fontface = "bold", size = 5, lineheight = 0.9
  ) +
  ## 2. FREL -----------------------------------------------------------------
ggplot2::geom_line(ggplot2::aes(y = .data$FREL), col = "deeppink3", linewidth = 1) +
  ## 3. emissions ------------------------------------------------------------
ggplot2::geom_linerange(
  ggplot2::aes(ymin = .data$E_lower, ymax = .data$E_upper, colour = .data$period_type),
  linewidth = 0.8
) +
  ggplot2::geom_point(ggplot2::aes(y = .data$E, colour = .data$period_type), size = 4) +
  ## 4. scales and theme -----------------------------------------------------
ggplot2::scale_colour_discrete(na.value = "gray50") +
  ggplot2::scale_x_continuous(breaks = min(out_yearly$year):max(out_yearly$year), minor_breaks = NULL) +
  ggplot2::coord_cartesian(ylim = c(0, NA)) +
  ggplot2::theme_bw(base_size = 20) +
  ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 45, hjust = 1),
                 legend.position = "bottom") +
  ggplot2::labs(x = "Years", y = "Emissions (MtCO2e/y)", color = "")

## 1.4 Show check results ==================================================
## Hide progress div; reveal value boxes + arithmetic mean only when all_ok.
observeEvent(input$btn_show_checks, {
  shinyjs::hide("check_progress")

  if (isTRUE(rv$checks$all_ok)) {
    shinyjs::show("check_panel")
  }
})


##
## 2. CHECK PANEL OUTPUTS ##################################################
##

## 2.1 Value boxes =========================================================

## + Time periods ----------------------------------------------------------
output$vb_nb_time <- renderUI({
  req(rv$checks$all_ok)
  HTML(paste0(nrow(rv$inputs$time), "&nbsp;reporting periods"))
})

output$vb_nb_ref <- renderText({
  req(rv$checks$all_ok)
  time_sub <- rv$inputs$time |>
    dplyr::filter(stringr::str_detect(.data$period_type, "REF"))
  paste0(nrow(time_sub), " for reference")
})

output$vb_nb_mon <- renderText({
  req(rv$checks$all_ok)
  time_sub <- rv$inputs$time |>
    dplyr::filter(stringr::str_detect(.data$period_type, "M"))
  paste0(nrow(time_sub), " for monitoring")
})

## + Activity data ---------------------------------------------------------
output$vb_nb_trans <- renderUI({
  req(rv$checks$all_ok)
  HTML(paste0(nrow(rv$inputs$area), "&nbsp;land use transitions"))
})

output$vb_nb_lu <- renderText({
  req(rv$checks$all_ok)
  nb_lu <- length(unique(c(rv$inputs$area$lu_initial_id, rv$inputs$area$lu_final_id)))
  paste0(nb_lu, " land use categories")
})

output$vb_nb_redd <- renderText({
  req(rv$checks$all_ok)
  nb_redd <- unique(rv$inputs$area$redd_activity)
  paste0(length(nb_redd), " REDD+ activities: ", paste(nb_redd, collapse = ", "))
})

## + Carbon stock ----------------------------------------------------------
output$vb_nb_pools <- renderUI({
  req(rv$checks$all_ok)
  pools      <- unique(rv$inputs$carbon$c_element)
  real_pools <- pools[pools %in% c("AGB", "BGB", "DW", "LI", "SOC")]
  n_pools    <- length(real_pools)
  if ("RS"  %in% pools) n_pools <- n_pools + 1
  if (length(pools) == 1 && pools == "ALL") n_pools <- 1
  HTML(paste0(n_pools, "&nbsp;Carbon pools"))
})

output$vb_c_pools <- renderText({
  req(rv$checks$all_ok)
  pools      <- unique(rv$inputs$carbon$c_element)
  real_pools <- pools[pools %in% c("AGB", "BGB", "DW", "LI", "SOC")]
  if ("RS" %in% pools) real_pools <- c(real_pools, "BGB via R:S")
  if (length(pools) == 1 && pools == "ALL") real_pools <- "Ctotal"
  paste(real_pools, collapse = ", ")
})

output$vb_dg_method <- renderText({
  req(rv$checks$all_ok)
  if ("DG_ratio" %in% unique(rv$inputs$carbon$c_element)) {
    paste0("Degradation ratio applied to ", rv$inputs$setup$dg_pool)
  } else {
    "Carbon stock difference"
  }
})


## 2.2 Arithmetic mean result table ========================================
output$check_result_table <- gt::render_gt({
  req(rv$checks$all_ok, rv$checks$ari_res)

  rv$checks$ari_res$emissions_table |>
    gt::gt(rowname_col = "item", groupname_col = "grp") |>
    gt::tab_spanner(label = "Emissions (tCO2e/yr)", columns = c("DF", "DG", "total")) |>
    gt::cols_label(years = "Years", DF = "Deforestation", DG = "Degradation", total = "Total", U_pct = "U (%)") |>
    gt::fmt_number(columns = c("DF", "DG", "total"), decimals = 0, use_seps = TRUE) |>
    gt::fmt_number(columns = "U_pct", decimals = 1, pattern = "{x}%") |>
    gt::sub_missing(columns = gt::everything(), missing_text = "") |>
    gt::cols_align(align = "right", columns = c("DF", "DG", "total", "U_pct"))
})

## 2.2 Arithmetic mean plot ================================================
output$check_arithmetic_gg <- renderPlot({
  req(rv$checks$all_ok, rv$checks$ari_res)
  rv$checks$ari_res$gg_emissions
})


## 2.3 LU change matrix ====================================================
output$check_select_period_UI <- renderUI({
  req(rv$inputs$time)
  selectInput(
    inputId = ns("check_select_period"),
    label   = "Select a time period",
    choices = rv$inputs$time$period_no
  )
})

output$check_show_period_type <- renderText({
  req(input$check_select_period, rv$inputs$time)
  rv$inputs$time |>
    dplyr::filter(.data$period_no == input$check_select_period) |>
    dplyr::pull("period_type")
})

output$check_lumatrix <- gt::render_gt({
  req(rv$checks$all_ok, rv$inputs$area, input$check_select_period)

  year_start <- rv$inputs$time |>
    dplyr::filter(.data$period_no == input$check_select_period) |>
    dplyr::pull(.data$year_start)

  year_end <- rv$inputs$time |>
    dplyr::filter(.data$period_no == input$check_select_period) |>
    dplyr::pull(.data$year_end)

  rv$inputs$area |>
    dplyr::filter(.data$trans_period == input$check_select_period) |>
    dplyr::mutate(trans_area = round(.data$trans_area, 0)) |>
    dplyr::arrange(.data$lu_final) |>
    tidyr::pivot_wider(
      id_cols      = "lu_initial",
      names_from   = "lu_final",
      values_from  = "trans_area",
      values_fill  = 0
    ) |>
    dplyr::arrange(.data$lu_initial) |>
    gt::gt(rowname_col = "lu_initial") |>
    gt::tab_stubhead(label = "Area (ha)") |>
    gt::tab_row_group(
      label = gt::md(paste0("**Initial land use ", year_start, "**")),
      rows  = gt::everything()
    ) |>
    gt::tab_spanner(
      label   = gt::md(paste0("**Final land use ", year_end, "**")),
      columns = gt::everything()
    ) |>
    gt::fmt_number(columns = gt::everything(), decimals = 0, use_seps = TRUE)
})


##
## 3. SIDEBAR - RUN MCS ####################################################
##

observeEvent(input$btn_run_mcs, {

  session$sendCustomMessage("plausible", list(event = "run_mcs"))
  session$sendCustomMessage("activate-tab", list(id = ns("tool_tabs"), value = "res_tab"))

  shinyjs::hide("res_init")
  shinyjs::show("res_progress")
  shinyjs::hide("res_show")
  shinyjs::hide("res_cards")

  rv$mcs$all_done <- NULL

  ## Set seed ---------------------------------------------------------------
  shinyWidgets::updateProgressBar(
    session = session, id = "prog_res", value = 0,
    title = "Set seed for random simulations...", status = "primary"
  )

  if (!is.na(rv$inputs$setup$ran_seed)) {
    set.seed(rv$inputs$setup$ran_seed)
  } else {
    rv$inputs$setup$app_ran_seed <- sample(1:100, 1)
    set.seed(rv$inputs$setup$app_ran_seed)
  }

  Sys.sleep(0.1)

  ## LU transition level simulations ----------------------------------------
  shinyWidgets::updateProgressBar(
    session = session, id = "prog_res", value = 10,
    title = "Simulate emissions for each land use transition...", status = "primary"
  )

  rv$mcs$sim_trans <- fct_combine_mcs_E(.checked_data = rv$checked)

  Sys.sleep(0.1)

  ## Aggregate simulations --------------------------------------------------
  shinyWidgets::updateProgressBar(
    session = session, id = "prog_res", value = 40,
    title = "Calculate Emission Reductions...", status = "primary"
  )

  rv$mcs$sim_redd <- rv$mcs$sim_trans |>
    dplyr::group_by(.data$sim_no, .data$time_period, .data$redd_activity) |>
    dplyr::summarise(E_year = sum(.data$E_year), E = sum(.data$E), .groups = "drop") |>
    dplyr::mutate(redd_id = paste0(.data$time_period, " - ", .data$redd_activity))

  rv$mcs$sim_REF <- rv$mcs$sim_trans |>
    fct_combine_mcs_P(.period_type = "REF")

  rv$mcs$sim_MON <- rv$mcs$sim_trans |>
    fct_combine_mcs_P(.period_type = "MON")

  rv$mcs$sim_ER <- fct_combine_mcs_ER(
    .sim_ref   = rv$mcs$sim_REF,
    .sim_mon   = rv$mcs$sim_MON,
    .ad_annual = rv$inputs$setup$ad_annual
  )

  Sys.sleep(0.1)

  ## Stats from simulations -------------------------------------------------
  shinyWidgets::updateProgressBar(
    session = session, id = "prog_res", value = 60,
    title = "Get medians and confidence intervals...", status = "primary"
  )

  rv$mcs$res_trans <- fct_calc_res(
    .data     = rv$mcs$sim_trans,
    .id       = .data$trans_id,
    .sim      = .data$E_year,
    .ci_alpha = rv$inputs$setup$ci_alpha
  )

  rv$mcs$res_redd <- fct_calc_res(
    .data     = rv$mcs$sim_redd,
    .id       = .data$redd_id,
    .sim      = .data$E_year,
    .ci_alpha = rv$inputs$setup$ci_alpha
  )

  rv$mcs$res_REF <- fct_calc_res(
    .data     = rv$mcs$sim_REF,
    .id       = .data$period_type,
    .sim      = .data$E,
    .ci_alpha = rv$inputs$setup$ci_alpha
  )

  rv$mcs$res_MON <- fct_calc_res(
    .data     = rv$mcs$sim_MON,
    .id       = .data$period_type,
    .sim      = .data$E,
    .ci_alpha = rv$inputs$setup$ci_alpha
  )

  rv$mcs$res_MON2 <- rv$mcs$res_MON |>
    dplyr::mutate(period_type = paste0("E-", .data$period_type))

  rv$mcs$res_ER <- fct_calc_res(
    .data     = rv$mcs$sim_ER,
    .id       = .data$period_type,
    .sim      = .data$ER_sim,
    .ci_alpha = rv$inputs$setup$ci_alpha
  )

  rv$mcs$res_ER2 <- rv$mcs$res_ER |>
    dplyr::mutate(period_type = paste0("ER-", .data$period_type))

  rv$mcs$res_ER3 <- rv$mcs$res_REF |>
    dplyr::bind_rows(rv$mcs$res_MON2) |>
    dplyr::bind_rows(rv$mcs$res_ER2) |>
    dplyr::left_join(rv$checks$ari_res$ER, by = "period_type", suffix = c("", "_ari")) |>
    dplyr::select("period_type", "E_ari", dplyr::everything())

  Sys.sleep(0.1)

  ## Forest plots -----------------------------------------------------------
  shinyWidgets::updateProgressBar(
    session = session, id = "prog_res", value = 80,
    title = "Prepare outputs...", status = "primary"
  )

  ## No-binding hack for R CMD check
  trans_id <- redd_id <- period_type <- NULL
  E <- E_ari <- E_U_ari <- E_U <- E_cilower <- E_ciupper <- NULL
  E_year <- ER_sim <- NULL

  rv$mcs$fp_trans <- fct_forestplot(
    .data      = rv$mcs$res_trans,
    .id        = trans_id,
    .value     = E,
    .uperc     = E_U,
    .cilower   = E_cilower,
    .ciupper   = E_ciupper,
    .id_colname = "Land use transition",
    .conflevel = rv$inputs$setup$conf_level_txt
  )

  rv$mcs$fp_redd <- fct_forestplot(
    .data      = rv$mcs$res_redd,
    .id        = redd_id,
    .value     = E,
    .uperc     = E_U,
    .cilower   = E_cilower,
    .ciupper   = E_ciupper,
    .id_colname = "REDD+ activities",
    .conflevel = rv$inputs$setup$conf_level_txt
  )

  rv$mcs$fp_ER <- fct_forestplot(
    .data       = rv$mcs$res_ER3,
    .id         = period_type,
    .value      = E,
    .value_ari  = E_ari,
    .uperc_ari  = E_U_ari,
    .uperc      = E_U,
    .cilower    = E_cilower,
    .ciupper    = E_ciupper,
    .id_colname = "Time periods",
    .conflevel  = rv$inputs$setup$conf_level_txt
  )

  ## Finalise ---------------------------------------------------------------
  shinyWidgets::updateProgressBar(
    session = session, id = "prog_res", value = 100,
    title = "All steps completed!", status = "success"
  )

  rv$mcs$all_done <- TRUE

}) ## END observeEvent btn_run_mcs


##
## 4. RESULTS PANEL OUTPUTS ################################################
##

## 4.1 Show results after MCS completes ====================================
observe({
  req(rv$mcs$all_done)
  shinyjs::show("res_show")
})

observeEvent(input$btn_show_res, {
  shinyjs::hide("res_progress")
  shinyjs::hide("res_show")
  shinyjs::show("res_cards")
})


## 4.2 Downloads ===========================================================
output$dl_ari <- downloadHandler(
  filename = function() { "mocaredd - arithmetic mean based emission reductions.csv" },
  content  = function(file) { utils::write.csv(rv$checks$ari_res$ER, file) }
)

output$dl_res <- downloadHandler(
  filename = function() { "mocaredd - simulation based emissions reductions.csv" },
  content  = function(file) { utils::write.csv(rv$mcs$res_ER2, file) }
)

output$dl_sim_ER <- downloadHandler(
  filename = function() { "mocaredd - simulations at ER level.csv" },
  content  = function(file) { utils::write.csv(rv$mcs$sim_ER, file) }
)

output$dl_sim_trans <- downloadHandler(
  filename = function() { "mocaredd - BIGFILE - simulations at land use transition level.csv" },
  content  = function(file) { utils::write.csv(rv$mcs$sim_trans, file) }
)


## 4.3 Forest plots ========================================================
output$res_redd_fp <- gt::render_gt({
  req(rv$mcs$fp_redd)
  rv$mcs$fp_redd
})

output$res_ER_fp <- gt::render_gt({
  req(rv$mcs$fp_ER)
  rv$mcs$fp_ER
})


## 4.4 Histograms ==========================================================

output$res_select_ER_hist_UI <- renderUI({
  req(rv$mcs$res_ER3)
  selectInput(
    inputId = ns("res_select_ER_hist"),
    label   = "Select a period",
    choices = rv$mcs$res_ER3$period_type
  )
})

output$res_ER_hist <- renderPlot({
  req(input$res_select_ER_hist, rv$mcs$res_ER3)

  if (input$res_select_ER_hist == "REF") {
    sims       <- rv$mcs$sim_REF
    res        <- rv$mcs$res_REF
    value      <- rlang::quo(E)
    value_type <- "E"
  } else if (stringr::str_detect(input$res_select_ER_hist, "E-")) {
    input_short <- stringr::str_remove(input$res_select_ER_hist, "E-")
    sims        <- rv$mcs$sim_MON |> dplyr::filter(.data$period_type == input_short)
    res         <- rv$mcs$res_MON |> dplyr::filter(.data$period_type == input_short)
    value       <- rlang::quo(E)
    value_type  <- "E"
  } else if (stringr::str_detect(input$res_select_ER_hist, "ER-")) {
    input_short <- stringr::str_remove(input$res_select_ER_hist, "ER-")
    sims        <- rv$mcs$sim_ER |> dplyr::filter(.data$period_type == input_short)
    res         <- rv$mcs$res_ER |> dplyr::filter(.data$period_type == input_short)
    value       <- rlang::quo(ER_sim)
    value_type  <- "ER"
  }

  fct_histogram(
    .data       = sims,
    .res        = res,
    .id         = period_type,
    .value      = !!value,
    .value_type = value_type
  )
})

output$res_select_redd_hist_UI <- renderUI({
  req(rv$mcs$sim_redd)
  selectInput(
    inputId = ns("res_select_redd_hist"),
    label   = "Select a REDD+ activity",
    choices = unique(rv$mcs$sim_redd$redd_activity)
  )
})

output$res_select_period_hist_UI <- renderUI({
  req(rv$mcs$sim_redd)
  selectInput(
    inputId = ns("res_select_period_hist"),
    label   = "Select a time period",
    choices = sort(unique(rv$mcs$sim_redd$time_period))
  )
})

output$res_redd_hist <- renderPlot({
  req(input$res_select_redd_hist, input$res_select_period_hist, rv$mcs$res_redd)

  sel_redd_id <- paste0(input$res_select_period_hist, " - ", input$res_select_redd_hist)
  sims        <- rv$mcs$sim_redd |> dplyr::filter(.data$redd_id == sel_redd_id)
  res         <- rv$mcs$res_redd |> dplyr::filter(.data$redd_id == sel_redd_id)

  fct_histogram(
    .data       = sims,
    .res        = res,
    .id         = redd_id,
    .value      = !!rlang::quo(E_year),
    .value_type = "E"
  )
})


##
## 5. SENSITIVITY ##########################################################
##

output$sens_trans_fp <- gt::render_gt({
  req(rv$mcs$fp_trans)
  rv$mcs$fp_trans
})
