#' Tool module server function
#'
#' @importFrom rlang .data
#' @importFrom magrittr %>%
#'
#' @noRd
mod_tool_server <- function(id, rv) {

  moduleServer(id, function(input, output, session) {

    ns <- session$ns

    ##
    ## 1. Data upload and checks ###############################################
    ##

    ## 1.1 Download example 1 if needed ========================================
    output$dl_template <- downloadHandler(
      filename = function(){"template1.xlsx"},
      content  = function(file){file.copy(system.file("extdata/example1.xlsx", package = "mocaredd"), file)}
    )

    ## 1.2 Check uploaded file columns =========================================
    observeEvent(input$load_xlsx, {

      rv$inputs$xlsx_path <- input$load_xlsx$datapath
      rv$inputs$xlsx_tabs_ok <- all(rv$checklist$xlsx_tabs %in% readxl::excel_sheets(input$load_xlsx$datapath))

      if(rv$inputs$xlsx_tabs_ok) {
        shinyjs::hide("msg_no_data")
        shinyjs::show("msg_data_tabs_ok")
        shinyjs::hide("msg_data_tabs_wrong")
        shinyjs::enable("btn_run_checks")
      } else {
        shinyjs::hide("msg_no_data")
        shinyjs::hide("msg_data_tabs_ok")
        shinyjs::show("msg_data_tabs_wrong")
        shinyjs::disable("btn_run_checks")
      }

    })

    ## 1.3 Read data and run checks ============================================

    observeEvent(input$btn_run_checks, {

      ## For moving to sub-module?
      #rv$inputs$btn_run_checks <- input$btn_run_checks
      nav_select(id = "tool_tabs", selected = "check_panel")

      ## ++ Show progress bar --------------------------------------------------
      shinyjs::hide("check_init_msg")
      shinyjs::show("check_progress")
      shinyjs::hide("check_show")
      shinyjs::hide("check_vbs")
      shinyjs::hide("check_cards")

      ## Reset indicator that all checks are done
      rv$checks$all_done <- NULL

      ## ++ Read data ----------------------------------------------------------
      shinyWidgets::updateProgressBar(
        title = "Loading data...",
        session = session, id = "prog_allchecks", value = 0, status = "primary"
      )

      rv$inputs$usr  <- readxl::read_xlsx(rv$inputs$xlsx_path, sheet = "user_inputs", na = "NA")
      rv$inputs$time <- readxl::read_xlsx(rv$inputs$xlsx_path, sheet = "time_periods", na = "NA")
      rv$inputs$ad   <- readxl::read_xlsx(rv$inputs$xlsx_path, sheet = "AD_lu_transitions", na = "NA")
      rv$inputs$cs   <- readxl::read_xlsx(rv$inputs$xlsx_path, sheet = "c_stocks", na = "NA")

      Sys.sleep(0.1)

      ## ++ Run checks ---------------------------------------------------------
      shinyWidgets::updateProgressBar(
        title = "Checking input file...",
        session = session, id = "prog_allchecks", value = 25
      )

      ## Use fct_check_data2()
      rv$checks$check_data <- fct_check_data2(
        .usr =  rv$inputs$usr,
        .time = rv$inputs$time,
        .ad =   rv$inputs$ad,
        .cs =   rv$inputs$cs,
        .checklist = rv$checklist
      )

      Sys.sleep(0.1)

      ## ++ Calculations -------------------------------------------------------
      shinyWidgets::updateProgressBar(
        title = "Running Calculations...",
        session = session, id = "prog_allchecks", value = 50
      )

      ## !!! UPDATE INPUTS !!!
      rv$inputs$time               <- rv$inputs$time |> dplyr::mutate(nb_years = .data$year_end - .data$year_start + 1)
      rv$inputs$usr$ci_alpha       <- 1 - rv$inputs$usr$conf_level
      rv$inputs$usr$conf_level_txt <- paste0(rv$inputs$usr$conf_level * 100, "%")

      ## Calc arithmetic mean
      rv$checks$ari_res <- fct_arithmetic_mean(.ad = rv$inputs$ad, .cs = rv$inputs$cs, .usr = rv$inputs$usr, .time = rv$inputs$time)

      Sys.sleep(0.1)

      ## ++ Outputs ------------------------------------------------------------
      ## outputs are calculated once new data is uploaded so they are performed here
      ## instead of the render*({}) functions

      ## !!! NOT IMPLEMENTED - see outputs section
      shinyWidgets::updateProgressBar(
        title = "Preparing outputs...",
        session = session, id = "prog_allchecks", value = 50
      )

      Sys.sleep(0.5)

      ## ++ Finalize -----------------------------------------------------------
      shinyWidgets::updateProgressBar(
        title = "All steps completed...",
        session = session, id = "prog_allchecks", value = 100, status = "success"
      )

      rv$checks$all_done <- TRUE

      ## ++ Enable run MCS -----------------------------------------------------

      if(rv$checks$check_data$all_ok) {
        shinyjs::hide("msg_no_check")
        shinyjs::show("msg_checks_ok")
        shinyjs::hide("msg_checks_wrong")
        shinyjs::enable("btn_run_mcs")
      } else {
        shinyjs::hide("msg_no_check")
        shinyjs::hide("msg_checks_ok")
        shinyjs::show("msg_checks_wrong")
        shinyjs::disable("btn_run_mcs")
      }

    })

    ## 1.4 Prepare Outputs =====================================================

    ## !!! TMP: Show xlsx_tabs_ok
    output$ctrl_input <- renderText({
      rv$inputs$xlsx_tabs_ok
    })

    ## ++ value box content ----------------------------------------------------
    ## +++ Time VB ----
    output$vb_nb_time <- renderUI({
      req(rv$checks$check_data$all_ok)
      if (rv$checks$check_data$all_ok) {
        HTML(paste0(nrow(rv$inputs$time), "&nbsp;periods"))
      }
    })

    output$vb_nb_ref <- renderText({
      req(rv$checks$check_data$all_ok)
      if (rv$checks$check_data$all_ok) {
        time_sub <- rv$inputs$time |> dplyr::filter(stringr::str_detect(.data$period_type, pattern = "REF"))
        paste0(nrow(time_sub), " for reference")
      }
    })

    output$vb_nb_mon <- renderText({
      req(rv$checks$check_data$all_ok)
      if (rv$checks$check_data$all_ok) {
        time_sub <- rv$inputs$time |> dplyr::filter(stringr::str_detect(.data$period_type, pattern = "M"))
        paste0(nrow(time_sub), " for monitoring")
      }
    })

    ## +++ AD VB ----

    ## +++ CS VB ----

    ## ++ Cards content --------------------------------------------------------
    ## +++ Table of checks ----
    output$check_msg <- gt::render_gt({
      req(rv$checks$all_done)

      col_check <- c(
        "column names",
        "table sizes",
        "column data types",
        "category variables",
        "unique IDs",
        "matches between tables"
      )

      col_icon <- c()
      if (rv$checks$check_data$cols_ok)      col_icon[1] <- bsicons::bs_icon("check-circle", class = "text-success") else col_icon[1] <- bsicons::bs_icon("x-circle", class = "text-danger")
      if (rv$checks$check_data$size_ok)      col_icon[2] <- bsicons::bs_icon("check-circle", class = "text-success") else col_icon[2] <- bsicons::bs_icon("x-circle", class = "text-danger")
      if (rv$checks$check_data$datatypes_ok) col_icon[3] <- bsicons::bs_icon("check-circle", class = "text-success") else col_icon[3] <- bsicons::bs_icon("x-circle", class = "text-danger")
      if (rv$checks$check_data$cats_ok)      col_icon[4] <- bsicons::bs_icon("check-circle", class = "text-success") else col_icon[4] <- bsicons::bs_icon("x-circle", class = "text-danger")
      if (rv$checks$check_data$ids_ok)       col_icon[5] <- bsicons::bs_icon("check-circle", class = "text-success") else col_icon[5] <- bsicons::bs_icon("x-circle", class = "text-danger")
      if (rv$checks$check_data$matches_ok)   col_icon[6] <- bsicons::bs_icon("check-circle", class = "text-success") else col_icon[6] <- bsicons::bs_icon("x-circle", class = "text-danger")

      col_pb <- c()
      ## Error msgs
      if (rv$checks$check_data$cols_ok)      col_pb[1] <- "" else col_pb[1] <- rv$checks$check_data$pbs$cols_pb
      if (rv$checks$check_data$size_ok)      col_pb[2] <- "" else col_pb[2] <- rv$checks$check_data$pbs$size_pb
      if (rv$checks$check_data$datatypes_ok) col_pb[3] <- "" else col_pb[3] <- rv$checks$check_data$pbs$datatypes_pb
      if (rv$checks$check_data$cats_ok)      col_pb[4] <- "" else col_pb[4] <- rv$checks$check_data$pbs$cats_pb
      if (rv$checks$check_data$ids_ok)       col_pb[5] <- "" else col_pb[5] <- rv$checks$check_data$pbs$ids_pb
      if (rv$checks$check_data$matches_ok)   col_pb[6] <- "" else col_pb[6] <- rv$checks$check_data$pbs$matches_pb

      data.frame(check = col_check, status = col_icon, problems = col_pb) |>
        gt::gt() |>
        gt::fmt_markdown(columns = "status") |>
        gt::cols_label_with(columns = gt::everything(), fn = stringr::str_to_sentence)

    })

    ## +++ Graph of ERs ----
    output$check_arithmetic_gg <- renderPlot({
      req(rv$checks$check_data$all_ok, rv$checks$ari_res)

      if (rv$checks$check_data$all_ok) {
        rv$checks$ari_res$gg_emissions
      }

    })

    ## +++ LU change matrix ----
    output$check_select_period_UI <- renderUI({
      selectInput(
        inputId = ns("check_select_period"),
        label = "Select a time period",
        choices = rv$inputs$time$period_no
      )
    })

    output$check_lumatrix <- gt::render_gt({

      req(rv$inputs$ad, rv$checks$check_data$all_ok, input$check_select_period)

      if (rv$checks$check_data$all_ok) {

        year_start <- rv$inputs$time |>
          dplyr::filter(.data$period_no == input$check_select_period) |>
          dplyr::pull(.data$year_start)

        year_end <- rv$inputs$time |>
          dplyr::filter(.data$period_no == input$check_select_period) |>
          dplyr::pull(.data$year_end)

        rv$inputs$ad |>
          dplyr::filter(.data$trans_period == input$check_select_period) |>
          dplyr::mutate(trans_area = round(.data$trans_area, 0)) |>
          dplyr::arrange(.data$lu_final) |>
          tidyr::pivot_wider(id_cols = "lu_initial", names_from = "lu_final", values_from = "trans_area", values_fill = 0) |>
          dplyr::arrange(.data$lu_initial) |>
          gt::gt(rowname_col = "lu_initial") |>
          gt::tab_stubhead(label = "Area (ha)") |>
          gt::tab_row_group(label = gt::md(paste0("**Initial land use ", year_start, "**")), rows = gt::everything()) |>
          gt::tab_spanner(label = gt::md(paste0("**Final land use ", year_end, "**")), columns = gt::everything()) |>
          gt::fmt_number(columns = gt::everything(), decimals = 0, use_seps = TRUE)

      }

    })

    ## 1.5 Show checks after all prepared ==========================================

    ## Update show / hide panels
    observe({
      req(rv$checks$all_done)

      if (rv$checks$all_done) shinyjs::show("check_show") else shinyjs::hide("check_show")

    })

    observeEvent(input$btn_show_checks, {
      shinyjs::hide("check_progress")
      shinyjs::hide("check_show")
      shinyjs::show("check_vbs")
      shinyjs::show("check_cards")
    })

    ##
    ## 2. Run MCS ##############################################################
    ##

    ## 2.1 Enable button =======================================================

    ## !!! Now within run checks button event
    # observe({
    #
    #   req(rv$checks$check_data$all_ok)
    #
    #
    #   if(rv$checks$check_data$all_ok) {
    #     shinyjs::hide("msg_no_check")
    #     shinyjs::show("msg_checks_ok")
    #     shinyjs::hide("msg_checks_wrong")
    #     shinyjs::enable("btn_run_mcs")
    #   } else {
    #     shinyjs::hide("msg_no_check")
    #     shinyjs::hide("msg_checks_ok")
    #     shinyjs::show("msg_checks_wrong")
    #     shinyjs::disable("btn_run_mcs")
    #   }
    #
    # })

    ## 2.2 Run MCS and calculate res and graphs ================================
    observeEvent(input$btn_run_mcs, {

      ## ++ Move to res panel --------------------------------------------------
      nav_select(id = "tool_tabs", selected = "res_panel")

      ## ++ Show progress bar --------------------------------------------------
      shinyjs::hide("res_init")
      shinyjs::show("res_progress")
      shinyjs::hide("res_show")
      shinyjs::hide("res_cards")

      rv$mcs$all_done <- NULL

      ## ++ Set seed for simulations -------------------------------------------
      shinyWidgets::updateProgressBar(
        title = "Set seed for random simulations...",
        session = session, id = "prog_res", value = 0, status = "primary"
      )

      ## Seed for random simulation
      if (!is.na(rv$inputs$usr$ran_seed)){
        set.seed(rv$inputs$usr$ran_seed)
        message("Random simulations with seed: ", rv$inputs$usr$ran_seed)
      } else {
        rv$inputs$usr$app_ran_seed <- sample(1:100, 1)
        set.seed(rv$inputs$usr$app_ran_seed)
        message("Seed for random simulations: ", rv$inputs$usr$app_ran_seed)
      }

      Sys.sleep(0.1)

      ## ++ LU transitions sims ------------------------------------------------
      shinyWidgets::updateProgressBar(
        title = "Simulate emissions for each land use transition...",
        session = session, id = "prog_res", value = 10, status = "primary"
      )

      rv$mcs$sim_trans <- fct_combine_mcs_E(
        .ad = rv$inputs$ad,
        .cs = rv$inputs$cs,
        .usr = rv$inputs$usr
      )

      Sys.sleep(0.1)

      ## simulation aggregates -------------------------------------------------
      shinyWidgets::updateProgressBar(
        title = "Calculate Emission Reductions...",
        session = session, id = "prog_res", value = 40, status = "primary"
      )

      rv$mcs$sim_redd <- rv$mcs$sim_trans |>
        dplyr::group_by(.data$sim_no, .data$time_period, .data$redd_activity) |>
        dplyr::summarise(E_sim = sum(.data$E_sim), .groups = "drop")

      rv$mcs$sim_REF <- rv$mcs$sim_trans |>
        fct_combine_mcs_P(.time = rv$inputs$time, .period_type = "REF", .ad_annual = rv$inputs$usr$ad_annual)

      rv$mcs$sim_MON <- rv$mcs$sim_trans |>
        fct_combine_mcs_P(.time = rv$inputs$time, .period_type = "MON", .ad_annual = rv$inputs$usr$ad_annual)

      rv$mcs$sim_ER <- fct_combine_mcs_ER(.sim_ref = rv$mcs$sim_REF, .sim_mon = rv$mcs$sim_MON, .ad_annual = rv$inputs$usr$ad_annual)

      Sys.sleep(0.1)

      ## ++ Get stats from simulations -----------------------------------------
      shinyWidgets::updateProgressBar(
        title = "Get medians and confidence intervals...",
        session = session, id = "prog_res", value = 60, status = "primary"
      )

      ## LU transition level results
      rv$mcs$res_trans <- fct_calc_res(
        .data = rv$mcs$sim_trans,
        .id = .data$trans_id,
        .sim = .data$E_sim,
        .ci_alpha = rv$inputs$usr$ci_alpha
      )

      rv$mcs$res_redd <- rv$mcs$sim_redd |>
        dplyr::mutate(redd_id = paste0(.data$time_period, " - ", .data$redd_activity)) |>
        fct_calc_res(
        .id = .data$redd_id,
        .sim = .data$E_sim,
        .ci_alpha = rv$inputs$usr$ci_alpha
      )

      rv$mcs$res_REF <- rv$mcs$sim_REF |>
        fct_calc_res(.id = .data$period_type, .sim = .data$E_sim, .ci_alpha = rv$inputs$usr$ci_alpha)

      rv$mcs$res_MON <- rv$mcs$sim_MON |>
        fct_calc_res(.id = .data$period_type, .sim = .data$E_sim, .ci_alpha = rv$inputs$usr$ci_alpha) |>
        dplyr::mutate(period_type = paste0("E-", .data$period_type))

      rv$mcs$res_ER <- rv$mcs$sim_ER |>
        fct_calc_res(.id = .data$period_type, .sim = .data$ER_sim, .ci_alpha = rv$inputs$usr$ci_alpha) |>
        dplyr::mutate(period_type = paste0("ER-", .data$period_type))

      rv$mcs$res_ER2 <- rv$mcs$res_REF |>
          dplyr::bind_rows(rv$mcs$res_MON) |>
          dplyr::bind_rows(rv$mcs$res_ER)

      Sys.sleep(0.1)

      ## ++ Prepa forest plots -------------------------------------------------
      shinyWidgets::updateProgressBar(
        title = "Prepare outputs...",
        session = session, id = "prog_res", value = 80, status = "primary"
      )

      ## no binding hack in R cmd check
      trans_id <- redd_id <- period_type <- E <- E_U <- E_cilower <- E_ciupper <- NULL

      rv$mcs$fp_trans <- fct_forestplot(
        .data = rv$mcs$res_trans,
        .id = trans_id,
        .value = E,
        .uperc = E_U,
        .cilower = E_cilower,
        .ciupper = E_ciupper,
        .id_colname = "Land use transition",
        .conflevel = rv$inputs$usr$conf_level_txt,
        .filename = NA
      )

      rv$mcs$fp_redd <- fct_forestplot(
        .data = rv$mcs$res_redd,
        .id = redd_id,
        .value = E,
        .uperc = E_U,
        .cilower = E_cilower,
        .ciupper = E_ciupper,
        .id_colname = "Land use transition",
        .conflevel = rv$inputs$usr$conf_level_txt,
        .filename = NA
      )

      rv$mcs$fp_ER <- fct_forestplot(
        .data = rv$mcs$res_ER2,
        .id = period_type,
        .value = E,
        .uperc = E_U,
        .cilower = E_cilower,
        .ciupper = E_ciupper,
        .id_colname = "Land use transition",
        .conflevel = rv$inputs$usr$conf_level_txt,
        .filename = NA
      )


      ## ++ Finalize -----------------------------------------------------------
      shinyWidgets::updateProgressBar(
        title = "All steps completed!",
        session = session, id = "prog_res", value = 100, status = "success"
      )

      rv$mcs$all_done <- TRUE

    })


    ## 2.3 Outputs =============================================================

    ## ++ Downloads ------------------------------------------------------------
    output$dl_ari <- downloadHandler(
      filename = function(){"mocaredd - arithmetic mean based emission reductions.csv"},
      content  = function(file){write.csv(rv$checks$ari_res$ER, file)}
    )

    output$dl_res <- downloadHandler(
      filename = function(){"mocaredd - simulation based emissions reductions.csv"},
      content  = function(file){write.csv(rv$mcs$res_ER2, file)}
    )

    output$dl_sim_ER <- downloadHandler(
      filename = function(){"mocaredd - simulations at ER level.csv"},
      content  = function(file){write.csv(rv$mcs$sim_ER, file)}
    )

    output$dl_sim_trans <- downloadHandler(
      filename = function(){"mocaredd - BIGFILE - simulations at land use transition level.csv"},
      content  = function(file){write.csv(rv$mcs$sim_trans, file)}
    )

    # output$dl_fp <- downloadHandler(
    #   filename = function(){"mocaredd - all forest plots for reporting"},
    #   content  = function(file){write.csv(rv$checks$ari_res$ER, file)}
    # )

    ## ++ Forest plots ---------------------------------------------------------
    output$res_trans_fp <- gt::render_gt({
      req(rv$mcs$fp_trans)

      rv$mcs$fp_trans

    })

    output$res_trans_fp <- gt::render_gt({
      req(rv$mcs$fp_trans)

      rv$mcs$fp_trans

    })

    output$res_redd_fp <- gt::render_gt({
      req(rv$mcs$fp_redd)

      rv$mcs$fp_redd

    })

    output$res_ER_fp <- gt::render_gt({
      req(rv$mcs$fp_ER)

      rv$mcs$fp_ER

    })
    ## ++ Histograms -----------------------------------------------------------

    ## 2.4 Show res conditionally ==============================================

    ## Update show / hide panels
    observe({
      req(rv$mcs$all_done)

      if (rv$mcs$all_done) shinyjs::show("res_show") else shinyjs::hide("res_show")

    })

    observeEvent(input$btn_show_res, {
      shinyjs::hide("res_progress")
      shinyjs::hide("res_show")
      shinyjs::show("res_cards")
    })


  }) ## END module server function

}
