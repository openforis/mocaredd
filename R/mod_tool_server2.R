#' Tool module server function (v2)
#'
#' Changes from v1:
#' - btn_run_checks now calls fct_checkinput() with withCallingHandlers() so
#'   every message() emitted by the function is appended line-by-line to the
#'   console div in the Check panel (shinyjs::html, add = TRUE).
#' - Data is read inside fct_checkinput(); the server stores the returned
#'   tables into rv$inputs (same names as v1 for downstream compatibility).
#' - The "Show check results" button (btn_show_checks) is enabled after checks
#'   complete regardless of outcome, so the user can always inspect the log.
#'   Results panel (value boxes + arithmetic mean) is only shown when all_ok.
#' - rv$checks$all_ok replaces rv$checks$check_data$all_ok throughout.
#' - Removed: old observe(rv$checks$all_done), check_msg gt render (replaced
#'   by the console), and all redundant req()+if() guard pairs in vb outputs.
#'
#' Text built here goes through i18n$t(.tr$key): inside render functions it
#' follows the language selector; in observers it uses the language at run time.
#'
#' @importFrom rlang .data
#'
#' @noRd
mod_tool_server2 <- function(id, rv, i18n, .tr) {

  moduleServer(id, function(input, output, session) {

    ns <- session$ns

    ## !!! FOR TESTING ONLY
    ## To run test on core elements, see tests/running-core-functions
    ## !!!

    ## Step status bar: set one step's state class (todo, running, done, error).
    ## shinyjs messages are sent right away, so "running" shows during a process.
    set_step <- function(step, state) {
      shinyjs::removeClass(paste0("step_", step), "step-todo step-running step-done step-error")
      shinyjs::addClass(paste0("step_", step), paste0("step-", state))
    }

    ##
    ## 1. SIDEBAR - LOAD & CHECK ###############################################
    ##

    ## 1.1 Download templates ==================================================
    output$dl_template_v2_simple <- downloadHandler(
      filename = function() { "mocaredd-templatev2-simple.xlsx" },
      content  = function(file) {
        file.copy(
          system.file("extdata/mocaredd-templatev2-simple.xlsx", package = "mocaredd"),
          file
        )
      }
    )

    output$dl_template_v2_inter <- downloadHandler(
      filename = function() { "mocaredd-templatev2-4pools.xlsx" },
      content  = function(file) {
        file.copy(
          system.file("extdata/mocaredd-templatev2-4pools.xlsx", package = "mocaredd"),
          file
        )
      }
    )

    output$dl_template_v1_simple <- downloadHandler(
      filename = function() { "mocaredd-templatev1-simple.xlsx" },
      content  = function(file) {
        file.copy(
          system.file("extdata/example2-with-sims.xlsx", package = "mocaredd"),
          file
        )
      }
    )

    ## 1.2 Upload file =========================================================
    observeEvent(input$load_xlsx, {

      rv$inputs$xlsx_path    <- input$load_xlsx$datapath
      rv$inputs$xlsx_tabs_ok <- all(
        c("user_inputs", "time_periods", "AD_lu_transitions", "c_stocks") %in%
          readxl::excel_sheets(input$load_xlsx$datapath) |
        c("setup", "time", "area", "carbon") %in%
          readxl::excel_sheets(input$load_xlsx$datapath)
      )

      shinyjs::hide("msg_no_data")
      shinyjs::toggle("msg_data_tabs_ok",    condition =  rv$inputs$xlsx_tabs_ok)
      shinyjs::toggle("msg_data_tabs_wrong", condition = !rv$inputs$xlsx_tabs_ok)
      shinyjs::toggleState("btn_run_checks", condition =  rv$inputs$xlsx_tabs_ok)

      ## New data: reset checks, simulations and sensitivity, and their panels
      rv$checks$all_ok  <- NULL
      rv$checks$ari_res <- NULL
      rv$mcs            <- NULL
      rv$sens           <- NULL

      shinyjs::show("check_init_msg")
      shinyjs::hide("check_progress")
      shinyjs::hide("check_panel")
      shinyjs::show("res_init")
      shinyjs::hide("res_progress")
      shinyjs::hide("res_show")
      shinyjs::hide("res_cards")
      shinyjs::show("sens_init")
      shinyjs::hide("sens_cards")

      shinyjs::show("msg_no_check")
      shinyjs::hide("msg_checks_ok")
      shinyjs::hide("msg_checks_wrong")
      shinyjs::disable("btn_run_mcs")
      shinyjs::show("msg_sens_no_check")
      shinyjs::hide("msg_sens_ready")
      shinyjs::disable("btn_run_sens")

      set_step("data", if (rv$inputs$xlsx_tabs_ok) "done" else "error")
      purrr::walk(c("check", "mcs", "sens"), set_step, state = "todo")

    })

    ## 1.3 Run checks ==========================================================
    observeEvent(input$btn_run_checks, {

      ## Navigate to check tab
      session$sendCustomMessage("activate-tab", list(id = ns("tool_tabs"), value = "check_tab"))

      ## 1.3.1 Reset UI ------
      ## Reset UI: show progress div, hide everything else in check panel
      shinyjs::hide("check_init_msg")
      shinyjs::show("check_progress")
      shinyjs::hide("check_panel")

      ## Clear the console and reset the progress bar
      shinyjs::html("check_console", "")
      shinyjs::disable("btn_show_checks")
      shinyWidgets::updateProgressBar(
        session = session,
        id      = "prog_allchecks",
        value   = 0,
        title   = i18n$t(.tr$check_progress_start),
        status  = "primary"
      )

      ## Reset the check results only: simulations and sensitivity are reset on
      ## new data (1.2), not when checks are run again on the same file
      rv$checks$all_ok  <- NULL
      rv$checks$ari_res <- NULL
      set_step("check", "running")

      ## 1.3.2 Check and read tables ------
      ## Run fct_checkinput(): each message() call is appended to the console
      ## div as it fires; errors are appended with an ERROR prefix.
      check_result <- withCallingHandlers(
        fct_checkinput(
          .path       = rv$inputs$xlsx_path,
          .pb_session = session,
          .pb_id      = ns("prog_allchecks"),
          .pb_max     = 80,
          .minislow   = 0.2
        ),
        message = function(m) {
          shinyjs::html(
            id   = "check_console",
            html = paste0(conditionMessage(m), "<br>"),
            add  = TRUE
          )
          invokeRestart("muffleMessage")
        }
      )

      rv$checks$all_ok <- isTRUE(check_result$all_ok)
      rv$checked <- check_result       # full fct_checkinput() output (incl. template_version)
      ## Tables, for value boxes and period/ER aggregation; keep the uploaded file
      ## info so checks can run again on the same file
      rv$inputs  <- c(
        check_result$data,
        list(xlsx_path = rv$inputs$xlsx_path, xlsx_tabs_ok = rv$inputs$xlsx_tabs_ok)
      )

      set_step("check", if (rv$checks$all_ok) "done" else "error")

      ## 1.3.3 Make calc chain and calc arithmetic mean ------
      if (rv$checks$all_ok) {

        ## Enable show check only if all checks passed
        shinyjs::enable("btn_show_checks")

        ## Add period length from input time
        rv$inputs$time <- rv$inputs$time |>
          dplyr::mutate(nb_years = .data$year_end - .data$year_start + 1)

        ## Derived settings used by MCS functions
        rv$inputs$setup$ci_alpha       <- 1 - rv$inputs$setup$conf_level
        rv$inputs$setup$conf_level_txt <- paste0(rv$inputs$setup$conf_level * 100, "%")

        ## 1.3.3.1 Build calculation chain -----
        ## Compute arithmetic mean emission reductions
        rv$checks$ari_res <- fct_arithmetic_mean2(.checked_data = rv$checked)

        ## Update sidebar MCS accordion
        shinyjs::hide("msg_no_check")
        shinyjs::show("msg_checks_ok")
        shinyjs::hide("msg_checks_wrong")
        shinyjs::enable("btn_run_mcs")

        ## Update sidebar sensitivity accordion
        shinyjs::hide("msg_sens_no_check")
        shinyjs::show("msg_sens_ready")
        shinyjs::enable("btn_run_sens")

      } else {

        shinyjs::hide("msg_no_check")
        shinyjs::hide("msg_checks_ok")
        shinyjs::show("msg_checks_wrong")
        shinyjs::disable("btn_run_mcs")

        shinyjs::show("msg_sens_no_check")
        shinyjs::hide("msg_sens_ready")
        shinyjs::disable("btn_run_sens")

      }

    }) ## END observeEvent btn_run_checks


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
      HTML(paste0(nrow(rv$inputs$time), "&nbsp;", i18n$t(.tr$vb_time_value)))
    })

    output$vb_nb_ref <- renderText({
      req(rv$checks$all_ok)
      time_sub <- rv$inputs$time |>
        dplyr::filter(stringr::str_detect(.data$period_type, "REF"))
      paste0(nrow(time_sub), " ", i18n$t(.tr$vb_time_ref))
    })

    output$vb_nb_mon <- renderText({
      req(rv$checks$all_ok)
      time_sub <- rv$inputs$time |>
        dplyr::filter(stringr::str_detect(.data$period_type, "M"))
      paste0(nrow(time_sub), " ", i18n$t(.tr$vb_time_mon))
    })

    ## + Activity data ---------------------------------------------------------
    output$vb_nb_trans <- renderUI({
      req(rv$checks$all_ok)
      HTML(paste0(nrow(rv$inputs$area), "&nbsp;", i18n$t(.tr$vb_ad_value)))
    })

    output$vb_nb_lu <- renderText({
      req(rv$checks$all_ok)
      nb_lu <- length(unique(c(rv$inputs$area$lu_initial_id, rv$inputs$area$lu_final_id)))
      paste0(nb_lu, " ", i18n$t(.tr$vb_ad_lu))
    })

    output$vb_nb_redd <- renderText({
      req(rv$checks$all_ok)
      nb_redd <- unique(rv$inputs$area$redd_activity)
      paste0(length(nb_redd), " ", i18n$t(.tr$vb_ad_redd), " ", paste(nb_redd, collapse = ", "))
    })

    ## + Carbon stock ----------------------------------------------------------
    output$vb_nb_pools <- renderUI({
      req(rv$checks$all_ok)
      pools      <- unique(rv$inputs$carbon$c_element)
      real_pools <- pools[pools %in% c("AGB", "BGB", "DW", "LI", "SOC")]
      n_pools    <- length(real_pools)
      if ("RS"  %in% pools) n_pools <- n_pools + 1
      if (length(pools) == 1 && pools == "ALL") n_pools <- 1
      HTML(paste0(n_pools, "&nbsp;", i18n$t(.tr$vb_cs_value)))
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
        paste0(i18n$t(.tr$vb_cs_dg_ratio), " ", rv$inputs$setup$dg_pool)
      } else {
        i18n$t(.tr$vb_cs_dg_diff)
      }
    })


    ## 2.2 Arithmetic mean result table ========================================
    output$check_result_table <- gt::render_gt({
      req(rv$checks$all_ok, rv$checks$ari_res)

      ## Group and row labels of fct_arithmetic_mean2() are the en values of the keys
      rv$checks$ari_res$emissions_table |>
        dplyr::mutate(
          grp = dplyr::case_when(
            .data$grp == .tr$check_tbl_grp_period ~ i18n$t(.tr$check_tbl_grp_period),
            .data$grp == .tr$check_tbl_grp_er     ~ i18n$t(.tr$check_tbl_grp_er),
            TRUE                                  ~ .data$grp
          ),
          item = .data$item |>
            stringr::str_replace(paste0("^", .tr$check_tbl_ref), i18n$t(.tr$check_tbl_ref)) |>
            stringr::str_replace(paste0("^", .tr$check_tbl_emi, " "), paste0(i18n$t(.tr$check_tbl_emi), " ")) |>
            stringr::str_replace(paste0("^", .tr$check_tbl_red, " "), paste0(i18n$t(.tr$check_tbl_red), " ")) |>
            stringr::str_replace(paste0("^", .tr$check_tbl_noperiod, "$"), i18n$t(.tr$check_tbl_noperiod))
        ) |>
        gt::gt(rowname_col = "item", groupname_col = "grp") |>
        gt::tab_spanner(label = i18n$t(.tr$check_tbl_spanner), columns = c("DF", "DG", "total")) |>
        gt::cols_label(
          years = i18n$t(.tr$check_tbl_years), DF = i18n$t(.tr$check_tbl_df), DG = i18n$t(.tr$check_tbl_dg),
          total = i18n$t(.tr$check_tbl_total), U_pct = "U (%)"
        ) |>
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
        inputId  = ns("check_select_period"),
        label    = i18n$t(.tr$check_select_period),
        choices  = rv$inputs$time$period_no,
        selected = if (isTRUE(isolate(input$check_select_period) %in% rv$inputs$time$period_no)) isolate(input$check_select_period)
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
        gt::tab_stubhead(label = i18n$t(.tr$check_lum_area)) |>
        gt::tab_row_group(
          label = gt::md(paste0("**", i18n$t(.tr$check_lum_initial), " ", year_start, "**")),
          rows  = gt::everything()
        ) |>
        gt::tab_spanner(
          label   = gt::md(paste0("**", i18n$t(.tr$check_lum_final), " ", year_end, "**")),
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
      set_step("mcs", "running")

      ## Set seed ---------------------------------------------------------------
      shinyWidgets::updateProgressBar(
        session = session, id = "prog_res", value = 0,
        title = i18n$t(.tr$res_prog_seed), status = "primary"
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
        title = i18n$t(.tr$res_prog_trans), status = "primary"
      )

      rv$mcs$sim_trans <- fct_combine_mcs_E(.checked_data = rv$checked)

      Sys.sleep(0.1)

      ## Aggregate simulations --------------------------------------------------
      shinyWidgets::updateProgressBar(
        session = session, id = "prog_res", value = 40,
        title = i18n$t(.tr$res_prog_er), status = "primary"
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
        title = i18n$t(.tr$res_prog_stats), status = "primary"
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
        title = i18n$t(.tr$res_prog_out), status = "primary"
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
        .id_colname = i18n$t(.tr$res_fp_trans),
        .conflevel = rv$inputs$setup$conf_level_txt
      )

      rv$mcs$fp_redd <- fct_forestplot(
        .data      = rv$mcs$res_redd,
        .id        = redd_id,
        .value     = E,
        .uperc     = E_U,
        .cilower   = E_cilower,
        .ciupper   = E_ciupper,
        .id_colname = i18n$t(.tr$res_fp_redd),
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
        .id_colname = i18n$t(.tr$res_fp_time),
        .conflevel  = rv$inputs$setup$conf_level_txt
      )

      ## Finalise ---------------------------------------------------------------
      shinyWidgets::updateProgressBar(
        session = session, id = "prog_res", value = 100,
        title = i18n$t(.tr$res_prog_done), status = "success"
      )

      rv$mcs$all_done <- TRUE
      set_step("mcs", "done")

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
        inputId  = ns("res_select_ER_hist"),
        label    = i18n$t(.tr$res_select_period),
        choices  = rv$mcs$res_ER3$period_type,
        selected = if (isTRUE(isolate(input$res_select_ER_hist) %in% rv$mcs$res_ER3$period_type)) isolate(input$res_select_ER_hist)
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
        inputId  = ns("res_select_redd_hist"),
        label    = i18n$t(.tr$res_select_redd),
        choices  = unique(rv$mcs$sim_redd$redd_activity),
        selected = if (isTRUE(isolate(input$res_select_redd_hist) %in% rv$mcs$sim_redd$redd_activity)) isolate(input$res_select_redd_hist)
      )
    })

    output$res_select_period_hist_UI <- renderUI({
      req(rv$mcs$sim_redd)
      selectInput(
        inputId  = ns("res_select_period_hist"),
        label    = i18n$t(.tr$res_select_time),
        choices  = sort(unique(rv$mcs$sim_redd$time_period)),
        selected = if (isTRUE(isolate(input$res_select_period_hist) %in% rv$mcs$sim_redd$time_period)) isolate(input$res_select_period_hist)
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

    ## 5.1 Run delta method ====================================================
    observeEvent(input$btn_run_sens, {

      session$sendCustomMessage("activate-tab", list(id = ns("tool_tabs"), value = "sensi_tab"))
      set_step("sens", "running")

      rv$sens <- tryCatch(
        fct_sensitivity2(.checked_data = rv$checked),
        error = function(e) {
          showNotification(paste(i18n$t(.tr$sens_error), conditionMessage(e)), type = "error", duration = NULL)
          NULL
        }
      )
      if (is.null(rv$sens)) {
        shinyjs::show("sens_init")
        shinyjs::hide("sens_cards")
        set_step("sens", "error")
      }
      req(rv$sens)

      ## Default result: first emission reduction, else first output
      outs <- rv$sens$outputs$output
      sel  <- c(grep("^ER-", outs, value = TRUE), outs)[1]
      updateSelectInput(session, "sens_select_output", choices = outs, selected = sel)

      shinyjs::hide("sens_init")
      shinyjs::show("sens_cards")
      set_step("sens", "done")

    })

    ## 5.2 Variance shares =====================================================
    output$sens_var_gg <- renderPlot({
      req(rv$sens)
      rv$sens$gg_variance
    })

    ## 5.3 AD tornado ==========================================================
    ## Height follows the number of bars shown
    sens_tornado_height <- function() {
      if (is.null(rv$sens) || !isTruthy(input$sens_select_output)) return(400)
      n_out <- sum(rv$sens$ad_tornado$output == input$sens_select_output)
      n     <- min(dplyr::coalesce(input$sens_top_n, 20), n_out)
      150 + 28 * n
    }

    output$sens_tornado_gg <- renderPlot({
      req(rv$sens, input$sens_select_output, input$sens_top_n)
      fct_tornado(
        .ad_tornado = rv$sens$ad_tornado,
        .output     = input$sens_select_output,
        .top_n      = input$sens_top_n
      )
    }, height = sens_tornado_height)

    ## 5.4 Downloads ===========================================================
    output$dl_sens_var <- downloadHandler(
      filename = function() { "mocaredd - sensitivity - variance shares.csv" },
      content  = function(file) { utils::write.csv(rv$sens$variance, file, row.names = FALSE) }
    )

    output$dl_sens_ad <- downloadHandler(
      filename = function() { "mocaredd - sensitivity - activity data sources.csv" },
      content  = function(file) { utils::write.csv(rv$sens$ad_tornado, file, row.names = FALSE) }
    )

    ## 5.5 Transition details (from the simulations) ===========================
    observe({
      shinyjs::toggle("sens_trans_init", condition = is.null(rv$mcs$fp_trans))
    })

    output$sens_trans_fp <- gt::render_gt({
      req(rv$mcs$fp_trans)
      rv$mcs$fp_trans
    })

  }) ## END moduleServer

}
