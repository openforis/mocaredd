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
      filename <- function() { "template1.xlsx" },
      content  <- function(file) { file.copy(system.file("extdata/example1.xlsx", package = "mocaredd"), file) }
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

      ## Calc length of periods
      rv$inputs$time_clean <- rv$inputs$time |>
        dplyr::mutate(nb_years = .data$year_end - .data$year_start + 1)

      ## Calc arithmetic mean
      rv$checks$ari_res <- fct_arithmetic_mean(.ad = rv$inputs$ad, .cs = rv$inputs$cs, .usr = rv$inputs$usr, .time = rv$inputs$time_clean)

      ## Conf int alpha
      rv$inputs$ci_alpha <- 1 - rv$inputs$usr$conf_level

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

    # submod_check_server("tab_check", rv = rv)

    ##
    ## 2. Run MCS ##############################################################
    ##

    ## 2.1 Enable button =======================================================

    observe({

      req(rv$checks$all_done, rv$checks$check_data$all_ok)


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

    ## 2.2 Run MCS and calculate res and graphs ================================
    observeEvent(input$btn_run_mcs, {

      # shinyjs::hide("mcs_init_msg")
      # shinyjs::show("mcs_progress")
      # shinyjs::hide("mcs_show")
      # shinyjs::hide("mcs_vbs")
      # shinyjs::hide("mcs_cards")

      ## LU transition sims
      rv$mcs$sim_trans <- fct_combine_mcs_E(
        .ad = rv$inputs$ad,
        .cs = rv$inputs$cs,
        .usr = rv$inputs$usr
        )

      ## LU transition level results
      rv$mcs$res_trans <- fct_calc_res(
        .data = rv$mcs$sim_trans,
        .id = .data$trans_id,
        .sim = .data$E_sim,
        .ci_alpha = rv$inputs$ci_alpha
        )

      # ## FREL sims
      # rv$mcs$sim_FREL <- fct_combine_mcs_P(
      #   .data = rv$mcs$sim_trans,
      #   .time = rv$inputs$time_clean,
      #   .period_type = "REF",
      #   .ad_annual = rv$inputs$usr$ad_annual
      #   )


    })


    ## 2.3 Prepare Outputs =====================================================

output$mcs_fp_trans <- gt::render_gt({

  ## no binding hack in R cmd check
  trans_id <- E <- E_U <- E_cilower <- E_ciupper <- NULL
  fct_forestplot(
    .data = rv$mcs$res_trans,
    .id = trans_id,
    .value = E,
    .uperc = E_U,
    .cilower = E_cilower,
    .ciupper = E_ciupper,
    .id_colname = "Land use transition",
    .conflevel = "90%",
    .filename = NA
    )

})


  }) ## END module server function

}
