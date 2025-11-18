#' Tool module server function
#'
#' @importFrom rlang .data
#'
#' @noRd
mod_tool_server <- function(id, rv) {

  moduleServer(id, function(input, output, session) {

    ns <- session$ns

    ##
    ## 1. SIDEBAR CHECK ACTIONS ################################################
    ##

    ## 1.1 BTN Download ========================================================
    ## Download example 1 if needed
    output$dl_template <- downloadHandler(
      filename = function(){"template1-4pools.xlsx"},
      content  = function(file){file.copy(system.file("extdata/example1-4pools.xlsx", package = "mocaredd"), file)}
    )

    ## 1.2 BTN Browse ==========================================================
    ## Find file and check columns
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

    ## 1.3 BTN Run checks ======================================================
    ## Read data and run checks

    observeEvent(input$btn_run_checks, {

      ## + 1.3.1 Move to tabset CHECK ----
      # updateTabsetPanel(
      #   session,
      #   inputId = ns("tool_tabs"),
      #   selected = "check_tab"
      # )
      session$sendCustomMessage("activate-tab", list(id = ns("tool_tabs"), value = "check_tab"))

      ## + 1.3.2 Show progress bar ----
      shinyjs::hide("check_init_msg")
      shinyjs::show("check_progress")
      shinyjs::hide("check_show")
      shinyjs::hide("check_vbs")
      shinyjs::hide("check_cards")

      ## Reset indicator that all checks are done
      rv$checks$all_done <- NULL

      ## + 1.3.3 Read data ----
      shinyWidgets::updateProgressBar(
        title = "Loading data...",
        session = session, id = "prog_allchecks", value = 0, status = "primary"
      )

      rv$inputs$usr  <- readxl::read_xlsx(rv$inputs$xlsx_path, sheet = "user_inputs", na = "NA")
      rv$inputs$time <- readxl::read_xlsx(rv$inputs$xlsx_path, sheet = "time_periods", na = "NA")
      rv$inputs$ad   <- readxl::read_xlsx(rv$inputs$xlsx_path, sheet = "AD_lu_transitions", na = "NA")
      rv$inputs$cs   <- readxl::read_xlsx(rv$inputs$xlsx_path, sheet = "c_stocks", na = "NA")

      Sys.sleep(0.1)

      ## FOR TESTING ONLY
      # rv <- list()
      # rv$inputs <- list()
      # rv$mcs <- list()
      # path <- system.file("extdata/example1-4pools.xlsx", package = "mocaredd")
      # path <- system.file("extdata/example2-with-sims.xlsx", package = "mocaredd")
      # .cs <- rv$inputs$cs <- readxl::read_xlsx(path = path, sheet = "c_stocks", na = "NA")
      # .ad <- rv$inputs$ad <- readxl::read_xlsx(path = path, sheet = "AD_lu_transitions", na = "NA")
      # .usr <- rv$inputs$usr <- readxl::read_xlsx(path = path, sheet = "user_inputs", na = "NA")
      # .time <- rv$inputs$time <- readxl::read_xlsx(path = path, sheet = "time_periods", na = "NA")
      ##


      ## + 1.3.4 Run checks -----
      shinyWidgets::updateProgressBar(
        title = "Checking input file...",
        session = session, id = "prog_allchecks", value = 50
      )

      ## Use fct_check_data2()
      rv$checks$check_data <- fct_check_data2(
        .usr =  rv$inputs$usr,
        .time = rv$inputs$time,
        .ad =   rv$inputs$ad,
        .cs =   rv$inputs$cs
      )

      Sys.sleep(0.1)

      ## + 1.3.5 Run calculations -------------------------------------------------------
      shinyWidgets::updateProgressBar(
        title = "Running Calculations...",
        session = session, id = "prog_allchecks", value = 70
      )

      ## !!! UPDATE INPUTS !!!
      rv$inputs$time               <- rv$inputs$time |> dplyr::mutate(nb_years = .data$year_end - .data$year_start + 1)
      rv$inputs$usr$ci_alpha       <- 1 - rv$inputs$usr$conf_level
      rv$inputs$usr$conf_level_txt <- paste0(rv$inputs$usr$conf_level * 100, "%")

      ## Calc arithmetic mean
      rv$checks$ari_res <- fct_arithmetic_mean(.ad = rv$inputs$ad, .cs = rv$inputs$cs, .usr = rv$inputs$usr, .time = rv$inputs$time)

      Sys.sleep(0.1)

      ## + 1.3.6 PLACEHOLDER Prepare outputs ----
      ## !!! NOT IMPLEMENTED - see outputs section

      # shinyWidgets::updateProgressBar(
      #   title = "Preparing outputs...",
      #   session = session, id = "prog_allchecks", value = 80
      # )
      #
      # Sys.sleep(0.5)

      ## + 1.3.7 Finalize Check -----
      shinyWidgets::updateProgressBar(
        title = "All steps completed...",
        session = session, id = "prog_allchecks", value = 100, status = "success"
      )

      rv$checks$all_done <- TRUE

      ## + 1.3.8 Enable run MCS ----

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

    }) ## END 1.3 observeEvent()

    ##
    ## 2. CHECK PANEL OUTPUTS ##################################################
    ##

    # ## !!! TMP: Show xlsx_tabs_ok
    # output$ctrl_input <- renderText({
    #   rv$inputs$xlsx_tabs_ok
    # })

    ## 2.1 Show check panel ====================================================

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

    ## 2.2 Value boxes =========================================================



    ## + 2.2.1 VB Time ----
    output$vb_nb_time <- renderUI({
      req(rv$checks$check_data$all_ok)
      if (rv$checks$check_data$all_ok) {
        HTML(paste0(nrow(rv$inputs$time), "&nbsp;reporting periods"))
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


    ## + 2.2.2 VB Activity data ----
    output$vb_nb_trans <- renderUI({
      req(rv$checks$check_data$all_ok)
      if (rv$checks$check_data$all_ok) {
        HTML(paste0(nrow(rv$inputs$ad), "&nbsp;land use transitions"))
      }
    })


    output$vb_nb_lu <- renderText({
      req(rv$checks$check_data$all_ok)
      if (rv$checks$check_data$all_ok) {
        nb_lu <- length(unique(c(rv$inputs$ad$lu_initial_id, rv$inputs$ad$lu_initial_id)))
        paste0(nb_lu, " land use categories")
      }
    })

    output$vb_nb_redd <- renderText({
      req(rv$checks$check_data$all_ok)
      if (rv$checks$check_data$all_ok) {
        nb_redd <- unique(rv$inputs$ad$redd_activity)
        paste0(length(nb_redd), " REDD+ activities: ", paste(nb_redd, collapse = ", "))
      }
    })

    ## + 2.2.3 VB Carbon stock ----
    output$vb_nb_pools <- renderUI({
      req(rv$checks$check_data$all_ok)
      if (rv$checks$check_data$all_ok) {
        pools <- unique(rv$inputs$cs$c_element)
        pools <- pools[pools %in% c("AGB", "BGB", "DW", "LI", "SOC")]
        HTML(paste0(length(pools), "&nbsp;Carbon pools"))
      }
    })

    output$vb_c_pools <- renderText({
      req(rv$checks$check_data$all_ok)
      if (rv$checks$check_data$all_ok) {
        pools <- unique(rv$inputs$cs$c_element)
        pools <- pools[pools %in% c("AGB", "BGB", "DW", "LI", "SOC")]
        paste(pools, collapse = ", ")
      }
    })

    output$vb_dg_method <- renderText({
      req(rv$checks$check_data$all_ok)
      if (rv$checks$check_data$all_ok) {
        if ("DG_ratio" %in% unique(rv$inputs$cs$c_element)){
          paste0("Degradation ratio applied to ", rv$inputs$usr$dg_pool)
        } else {
          "Carbon stock difference"
        }
      }
    })

    ## 2.3 Cards ===============================================================

    ## + 2.3.1 Checks table ----
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

    ## + 2.3.2 Arithmetic FREL/E/ERs figure ----
    output$check_arithmetic_gg <- renderPlot({
      req(rv$checks$check_data$all_ok, rv$checks$ari_res)

      if (rv$checks$check_data$all_ok) {
        rv$checks$ari_res$gg_emissions
      }

    })

    ## + 2.3.3 LU change matrix ----
    output$check_select_period_UI <- renderUI({
      selectInput(
        inputId = ns("check_select_period"),
        label = "Select a time period",
        choices = rv$inputs$time$period_no
      )
    })

    output$check_show_period_type <- renderText({
      req(input$check_select_period)

      rv$inputs$time |>
        dplyr::filter(.data$period_no == input$check_select_period) |>
        dplyr::pull("period_type")

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



    ##
    ## 3. SIDEBAR MCS ##########################################################
    ##

    ## Run MCS button enabled when all checks are done, see 1.3.8

    ## 3.1 BTN Run MCS =========================================================

    observeEvent(input$btn_run_mcs, {

      ## + 3.1.1 Move to tabset RES ----
      # updateTabsetPanel(
      #   session  = session,
      #   inputId  = ns("tool_tabs"),
      #   selected = "res_tab"
      # )
      session$sendCustomMessage("activate-tab", list(id = ns("tool_tabs"), value = "res_tab"))

      ## + 3.1.2 Show progress bar -----
      shinyjs::hide("res_init")
      shinyjs::show("res_progress")
      shinyjs::hide("res_show")
      shinyjs::hide("res_cards")

      rv$mcs$all_done <- NULL

      ## + 3.1.3 Set seed for simulations ------
      shinyWidgets::updateProgressBar(
        title = "Set seed for random simulations...",
        session = session, id = "prog_res", value = 0, status = "primary"
      )

      if (!is.na(rv$inputs$usr$ran_seed)){
        set.seed(rv$inputs$usr$ran_seed)
        message("Seed for random simulations: ", rv$inputs$usr$ran_seed)
      } else {
        rv$inputs$usr$app_ran_seed <- sample(1:100, 1)
        set.seed(rv$inputs$usr$app_ran_seed)
        message("New seed for random simulations: ", rv$inputs$usr$app_ran_seed)
      }

      Sys.sleep(0.1)

      ## + 3.1.4 Run LU transition level sims ------
      shinyWidgets::updateProgressBar(
        title = "Simulate emissions for each land use transition...",
        session = session, id = "prog_res", value = 10, status = "primary"
      )

      rv$mcs$sim_trans <- fct_combine_mcs_E(
        .ad = rv$inputs$ad,
        .cs = rv$inputs$cs,
        .usr = rv$inputs$usr
      )

      ## Annualize REDD+ activity data
      if (!rv$inputs$usr$ad_annual) {
        time_periods <- unique(rv$inputs$time$period_type)
        rv$mcs$sim_trans2 <- purrr::map(time_periods, function(x){
          nb_years <- rv$inputs$time |>
            dplyr::filter(period_type == x) |>
            dplyr::pull("nb_years") |>
            sum()
          period_ids <- rv$inputs$time |>
            dplyr::filter(period_type == x) |>
            dplyr::pull("period_no")
          rv$mcs$sim_trans |>
            dplyr::filter(.data$time_period %in% period_ids) |>
            dplyr::mutate(E = round(E / nb_years, 0))
        }) |> purrr::list_rbind()
      } else {
        tt <- rv$mcs$sim_trans2 <- rv$mcs$sim_trans
      }

      Sys.sleep(0.1)

      ## + 3.1.5 Aggregate simulations ------
      shinyWidgets::updateProgressBar(
        title = "Calculate Emission Reductions...",
        session = session, id = "prog_res", value = 40, status = "primary"
      )

      rv$mcs$sim_redd <- rv$mcs$sim_trans2 |>
        dplyr::group_by(.data$sim_no, .data$time_period, .data$redd_activity) |>
        dplyr::summarise(E = sum(.data$E), .groups = "drop") |>
        dplyr::mutate(redd_id = paste0(.data$time_period, " - ", .data$redd_activity))

      rv$mcs$sim_REF <- rv$mcs$sim_trans2 |>
        fct_combine_mcs_P(.time = rv$inputs$time, .period_type = "REF", .ad_annual = rv$inputs$usr$ad_annual)

      rv$mcs$sim_MON <- rv$mcs$sim_trans2 |>
        fct_combine_mcs_P(.time = rv$inputs$time, .period_type = "MON", .ad_annual = rv$inputs$usr$ad_annual)

      rv$mcs$sim_ER <- fct_combine_mcs_ER(.sim_ref = rv$mcs$sim_REF, .sim_mon = rv$mcs$sim_MON, .ad_annual = rv$inputs$usr$ad_annual)

      Sys.sleep(0.1)

      ## + 3.1.6 Get stats from simulations -------
      shinyWidgets::updateProgressBar(
        title = "Get medians and confidence intervals...",
        session = session, id = "prog_res", value = 60, status = "primary"
      )

      ## LU transition level results
      rv$mcs$res_trans <- fct_calc_res(
        .data = rv$mcs$sim_trans2,
        .id = .data$trans_id,
        .sim = .data$E,
        .ci_alpha = rv$inputs$usr$ci_alpha
      )

      rv$mcs$res_redd <- fct_calc_res(
        .data = rv$mcs$sim_redd,
        .id = .data$redd_id,
        .sim = .data$E,
        .ci_alpha = rv$inputs$usr$ci_alpha
      )

      rv$mcs$res_REF <- fct_calc_res(
        .data = rv$mcs$sim_REF,
        .id = .data$period_type,
        .sim = .data$E,
        .ci_alpha = rv$inputs$usr$ci_alpha
        )

      rv$mcs$res_MON <- fct_calc_res(
        .data = rv$mcs$sim_MON,
        .id = .data$period_type,
        .sim = .data$E,
        .ci_alpha = rv$inputs$usr$ci_alpha
        )

      rv$mcs$res_MON2 <- rv$mcs$res_MON |>
        dplyr::mutate(period_type = paste0("E-", .data$period_type))

      rv$mcs$res_ER <- fct_calc_res(
        .data = rv$mcs$sim_ER,
        .id = .data$period_type,
        .sim = .data$ER_sim,
        .ci_alpha = rv$inputs$usr$ci_alpha
        )

      rv$mcs$res_ER2 <- rv$mcs$res_ER |>
        dplyr::mutate(period_type = paste0("ER-", .data$period_type))

      rv$mcs$res_ER3 <- rv$mcs$res_REF |>
        dplyr::bind_rows(rv$mcs$res_MON2) |>
        dplyr::bind_rows(rv$mcs$res_ER2) |>
        dplyr::left_join(rv$checks$ari_res$ER, by = "period_type", suffix = c("", "_ari")) |>
        dplyr::select("period_type", "E_ari", dplyr::everything())

      Sys.sleep(0.1)

      ## + 3.1.7 Prepare forest plots -------------------------------------------------
      shinyWidgets::updateProgressBar(
        title = "Prepare outputs...",
        session = session, id = "prog_res", value = 80, status = "primary"
      )

      ## no binding hack in R cmd check
      trans_id <- redd_id <- period_type <- NULL
      E <- E_ari <- E_U <- E_cilower <- E_ciupper <- NULL
      E <- ER_sim <- NULL


      rv$mcs$fp_trans <- fct_forestplot(
        .data = rv$mcs$res_trans,
        .id = trans_id,
        .value = E,
        .uperc = E_U,
        .cilower = E_cilower,
        .ciupper = E_ciupper,
        .id_colname = "Land use transition",
        .conflevel = rv$inputs$usr$conf_level_txt
      )

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


      ## + 3.1.8 Finalize ------
      shinyWidgets::updateProgressBar(
        title = "All steps completed!",
        session = session, id = "prog_res", value = 100, status = "success"
      )

      rv$mcs$all_done <- TRUE

    })


    ##
    ## 4. RES TAB OUTPUTS ######################################################
    ##

    ## 4.1 Show res conditionally ==============================================

    # Update show / hide panels
    observe({
      req(rv$mcs$all_done)

      if (rv$mcs$all_done) shinyjs::show("res_show") else shinyjs::hide("res_show")

    })

    observeEvent(input$btn_show_res, {
      shinyjs::hide("res_progress")
      shinyjs::hide("res_show")
      shinyjs::show("res_cards")

    })


    ## 4.2 Downloads ===========================================================

    output$dl_ari <- downloadHandler(
      filename = function(){"mocaredd - arithmetic mean based emission reductions.csv"},
      content  = function(file){utils::write.csv(rv$checks$ari_res$ER, file)}
    )

    output$dl_res <- downloadHandler(
      filename = function(){"mocaredd - simulation based emissions reductions.csv"},
      content  = function(file){utils::write.csv(rv$mcs$res_ER2, file)}
    )

    output$dl_sim_ER <- downloadHandler(
      filename = function(){"mocaredd - simulations at ER level.csv"},
      content  = function(file){utils::write.csv(rv$mcs$sim_ER, file)}
    )

    output$dl_sim_trans <- downloadHandler(
      filename = function(){"mocaredd - BIGFILE - simulations at land use transition level.csv"},
      content  = function(file){utils::write.csv(rv$mcs$sim_trans, file)}
    )

    # output$dl_fp <- downloadHandler(
    #   filename = function(){"mocaredd - all forest plots for reporting"},
    #   content  = function(file){utils::write.csv(rv$checks$ari_res$ER, file)}
    # )

    ## 4.3 Forest plots ========================================================

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
    ## 4.4 Histograms ==========================================================

    ## + 4.4.1 Final emissions and ER simulations ----
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
        value <- rlang::quo(E)
        value_type <- "E"

      } else if (stringr::str_detect(input$res_select_ER_hist, "E-")) {

        input_short <- stringr::str_remove(input$res_select_ER_hist, "E-")

        sims <- rv$mcs$sim_MON |> dplyr::filter(.data$period_type == input_short)
        res  <- rv$mcs$res_MON |> dplyr::filter(.data$period_type == input_short)
        value <- rlang::quo(E)
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

    ## + 4.4.2 REDD+ and time period level emissions ----
    output$res_select_redd_hist_UI <- renderUI({
      selectInput(
        inputId = ns("res_select_redd_hist"),
        label = "Select a REDD+ activity",
        choices = unique(rv$mcs$sim_redd$redd_activity)
      )
    })

    output$res_select_period_hist_UI <- renderUI({
      selectInput(
        inputId = ns("res_select_period_hist"),
        label = "Select a time period",
        choices = sort(unique(rv$mcs$sim_redd$time_period))
      )
    })

    output$res_redd_hist <- renderPlot({
      req(input$res_select_redd_hist, input$res_select_period_hist, rv$mcs$res_redd)

      sel_redd_id <- paste0(input$res_select_period_hist, " - ", input$res_select_redd_hist)

      sims <- rv$mcs$sim_redd |> dplyr::filter(.data$redd_id == sel_redd_id)
      res  <- rv$mcs$res_redd |> dplyr::filter(.data$redd_id == sel_redd_id)
      value <- rlang::quo(E)
      value_type <- "E"


      fct_histogram(.data = sims, .res = res, .id = redd_id, .value = !!value, .value_type = value_type)

    })

    # ## 2.4 Show res conditionally
    #
    # # Update show / hide panels
    # observe({
    #   req(rv$mcs$all_done)
    #
    #   if (rv$mcs$all_done) shinyjs::show("res_show") else shinyjs::hide("res_show")
    #
    # })
    #
    # observeEvent(input$btn_show_res, {
    #   shinyjs::hide("res_progress")
    #   shinyjs::hide("res_show")
    #   shinyjs::show("res_cards")
    #
    # })

    ##
    ## 3. Sensitivity analysis #################################################
    ##

    output$sens_trans_fp <- gt::render_gt({
      req(rv$mcs$fp_trans)

      rv$mcs$fp_trans

    })

  }) ## END module server function

}
