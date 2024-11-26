#' Tool module server function
#'
#' @importFrom rlang .data
#'
#' @noRd
mod_tool_server_fail <- function(id, rv) {

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

      shinyjs::hide("check_init_msg")

      ## Hide other modules - call to action in main module
      shinyjs::show("div_check_UI")
      shinyjs::hide("div_res_UI")
      shinyjs::hide("div_sens_UI")

      rv$actions$run_checks <- input$btn_run_checks

    })

    submod_check_server("tab_check", rv = rv)

    ## Remove initial msg


    ## 1.4 Enable MCS ==========================================================
    observeEvent(rv$actions$mcs_enable, {
      if(rv$actions$mcs_enable) {
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

    ##
    ## Trans submodules actions ################################################
    ##

    observeEvent({
      rv$actions$show_checks
      rv$actions$show_res
      rv$actions$show_sens
      }, {

      if (rv$actions$show_checks) {
        shinyjs::show("div_check_UI")
        shinyjs::hide("div_res_UI")
        shinyjs::hide("div_sens_UI")
      } else if (rv$actions$show_res) {
        shinyjs::hide("div_check_UI")
        shinyjs::show("div_res_UI")
        shinyjs::hide("div_sens_UI")
      } else if (rv$actions$show_sens) {
        shinyjs::hide("div_check_UI")
        shinyjs::hide("div_res_UI")
        shinyjs::show("div_sens_UI")
      }

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
      # nav_select(id = "tool_tabs", selected = "res_panel")

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
        dplyr::summarise(E_sim = sum(.data$E_sim), .groups = "drop") |>
        dplyr::mutate(redd_id = paste0(.data$time_period, " - ", .data$redd_activity))

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
        fct_calc_res(
        .id = .data$redd_id,
        .sim = .data$E_sim,
        .ci_alpha = rv$inputs$usr$ci_alpha
      )

      rv$mcs$res_REF <- rv$mcs$sim_REF |>
        fct_calc_res(.id = .data$period_type, .sim = .data$E_sim, .ci_alpha = rv$inputs$usr$ci_alpha)

      rv$mcs$res_MON <- rv$mcs$sim_MON |>
        fct_calc_res(.id = .data$period_type, .sim = .data$E_sim, .ci_alpha = rv$inputs$usr$ci_alpha)

      rv$mcs$res_MON2 <- rv$mcs$res_MON |>
        dplyr::mutate(period_type = paste0("E-", .data$period_type))

      rv$mcs$res_ER <- rv$mcs$sim_ER |>
        fct_calc_res(.id = .data$period_type, .sim = .data$ER_sim, .ci_alpha = rv$inputs$usr$ci_alpha)

      rv$mcs$res_ER2 <- rv$mcs$res_ER |>
        dplyr::mutate(period_type = paste0("ER-", .data$period_type))

      rv$mcs$res_ER3 <- rv$mcs$res_REF |>
          dplyr::bind_rows(rv$mcs$res_MON2) |>
          dplyr::bind_rows(rv$mcs$res_ER2) |>
          dplyr::left_join(rv$checks$ari_res$ER, by = "period_type", suffix = c("", "_ari")) |>
          dplyr::select("period_type", "E_ari", dplyr::everything())


      Sys.sleep(0.1)

      ## ++ Prepa forest plots -------------------------------------------------
      shinyWidgets::updateProgressBar(
        title = "Prepare outputs...",
        session = session, id = "prog_res", value = 80, status = "primary"
      )

      ## no binding hack in R cmd check
      redd_id <- period_type <- E <- E_ari <- E_U <- E_cilower <- E_ciupper <- NULL

      # rv$mcs$fp_trans <- fct_forestplot(
      #   .data = rv$mcs$res_trans,
      #   .id = trans_id,
      #   .value = E,
      #   .uperc = E_U,
      #   .cilower = E_cilower,
      #   .ciupper = E_ciupper,
      #   .id_colname = "Land use transition",
      #   .conflevel = rv$inputs$usr$conf_level_txt
      # )

      rv$mcs$fp_redd <- fct_forestplot(
        .data = rv$mcs$res_redd,
        .id = redd_id,
        .value = E,
        .uperc = E_U,
        .cilower = E_cilower,
        .ciupper = E_ciupper,
        .id_colname = "REDD+ activities",
        .conflevel = rv$inputs$usr$conf_level_txt
      )

      rv$mcs$fp_ER <- fct_forestplot(
        .data = rv$mcs$res_ER3,
        .id = period_type,
        .value = E,
        .value_ari = E_ari,
        .uperc = E_U,
        .cilower = E_cilower,
        .ciupper = E_ciupper,
        .id_colname = "Time periods",
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

    # output$res_trans_fp <- gt::render_gt({
    #   req(rv$mcs$fp_trans)
    #
    #   rv$mcs$fp_trans
    #
    # })

    output$res_redd_fp <- gt::render_gt({
      req(rv$mcs$fp_redd)

      rv$mcs$fp_redd

    })

    output$res_ER_fp <- gt::render_gt({
      req(rv$mcs$fp_ER)

      rv$mcs$fp_ER

    })
    ## ++ Histograms -----------------------------------------------------------

    output$res_select_ER_hist_UI <- renderUI({
      selectInput(
        inputId = ns("res_select_ER_hist"),
        label = "Select a period",
        choices = rv$mcs$res_ER3$period_type
      )
    })


    output$res_ER_hist <- renderPlot({
      req(input$res_select_ER_hist, rv$mcs$res_ER3)

      if (input$res_select_ER_hist == "REF") {

        sims <- rv$mcs$sim_REF
        res  <- rv$mcs$res_REF
        value <- rlang::quo(E_sim)
        value_type <- "E"

      } else if (stringr::str_detect(input$res_select_ER_hist, "E-")) {

        input_short <- stringr::str_remove(input$res_select_ER_hist, "E-")

        sims <- rv$mcs$sim_MON |> dplyr::filter(.data$period_type == input_short)
        res  <- rv$mcs$res_MON |> dplyr::filter(.data$period_type == input_short)
        value <- rlang::quo(E_sim)
        value_type <- "E"

      } else if (stringr::str_detect(input$res_select_ER_hist, "ER-")) {

        input_short <- stringr::str_remove(input$res_select_ER_hist, "ER-")

        sims <- rv$mcs$sim_ER |> dplyr::filter(.data$period_type == input_short)
        res  <- rv$mcs$res_ER |> dplyr::filter(.data$period_type == input_short)
        value <- rlang::quo(ER_sim)
        value_type <- "ER"

      }

      fct_histogram(.data = sims, .res = res, .id = period_type, .value = !!value, .value_type = value_type)

    })


    ## 2.4 Show res conditionally ==============================================

    ## Update show / hide panels
    # observe({
    #   req(rv$mcs$all_done)
    #
    #   if (rv$mcs$all_done) shinyjs::show("res_show") else shinyjs::hide("res_show")
    #
    # })

    observeEvent(input$btn_show_res, {
      #shinyjs::hide("res_progress")
      #shinyjs::hide("res_show")
      #shinyjs::show("res_cards")

      rv$actions$to_tool_res <- input$btn_show_res
      # nav_select(id = "tool_tabs", selected = "res_panel")

    })

    observeEvent(rv$actions$to_tool_res, {
      #shinyjs::hide("res_progress")
      #shinyjs::hide("res_show")
      #shinyjs::show("res_cards")

      #nav_select(id = "tool_tabs", selected = "res_panel")

      updateTabsetPanel(session, "tool_tabs", "res_panel")

    })

  }) ## END module server function

}
