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
    ## 1. Data upload ##########################################################
    ##

    ## + Events ================================================================

    ## ++ Check uploaded file columns ------------------------------------------
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

    ## + Outputs ===============================================================

    ## Download example 1 if needed
    output$dl_template <- downloadHandler(
      filename <- function() { "template1.xlsx" },
      content  <- function(file) { file.copy(system.file("extdata/example1.xlsx", package = "mocaredd"), file) }
    )

    output$ctrl_input <- renderText({
      rv$inputs$xlsx_tabs_ok
    })


    ##
    ## 2. Read data and run checks #############################################
    ##

    ## + Events ================================================================

    ## Save Run checks to reactiveValues()
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

      # rv$checks$sim_trans_ar <- fct_combine_mcs_E(.ad = rv$inputs$ad, .cs = rv$inputs$cs, .usr = rv$inputs$usr)
      # rv$checks$sim_FREL_ar  <- fct_combine_mcs_P(
      #   .data = rv$checks$sim_trans_ar,
      #   .time = rv$inputs$time,
      #   .period_type = "reference",
      #   .ad_annual = rv$inputs$usr$ad_annual
      # )

      Sys.sleep(0.5)

      ## ++ Outputs ------------------------------------------------------------
      ## outputs are calculated once new data is uploaded so they are performed here
      ## instead of the render*({}) functions
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

    ## + Outputs ===============================================================

    ## ++ value box content ----------------------------------------------------
    ## +++ Time related ----
    output$vb_nb_time <- renderUI({
      req(rv$checks$check_data$all_ok)
      if (rv$checks$check_data$all_ok) {
        HTML(paste0(nrow(rv$inputs$time), "&nbsp;periods"))
      }
    })

    output$vb_nb_ref <- renderText({
      req(rv$checks$check_data$all_ok)
      if (rv$checks$check_data$all_ok) {
        time_sub <- rv$inputs$time |> dplyr::filter(stringr::str_detect(period_type, pattern = "REF"))
        paste0(nrow(time_sub), " for reference")
      }
    })

    output$vb_nb_mon <- renderText({
      req(rv$checks$check_data$all_ok)
      if (rv$checks$check_data$all_ok) {
        time_sub <- rv$inputs$time |> dplyr::filter(stringr::str_detect(period_type, pattern = "M"))
        paste0(nrow(time_sub), " for monitoring")
      }
    })

    ## +++ AD related ----

    ## +++ CS related ----

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

    output$check_arithmetic_gg <- renderPlot({ })

    ## +++ LU change matrix ----
    output$check_slider_UI <- renderUI({
      selectInput(
        inputId = ns("check_slider_period"),
        label = "Select a time period",
        choices = rv$inputs$time$period_no
          )
    })

    #output$check_lumatrix <- gt::render_gt({ })

    # observe({
    #   req(input$check_slider_period)
    #   rv$checks$check_slider_period <- input$check_slider_period
    # })

    output$check_lumatrix <- renderText({ input$check_slider_period })

    ## + UI changes ============================================================

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


  }) ## END module server function

}
