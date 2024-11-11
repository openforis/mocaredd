#' Tool module server function
#'
#' @noRd
mod_tool_server <- function(id, rv) {

  moduleServer(id, function(input, output, session) {

    ns <- session$ns

    ##
    ## 1. Data upload ##########################################################
    ##

    ## + Events ================================================================

    ## Read the XLSX files
    observeEvent(input$load_xlsx, {

      rv$inputs$xlsx_path <- input$load_xlsx$datapath

      rv$inputs$xlsx_tabs <- readxl::excel_sheets(input$load_xlsx$datapath)

      rv$inputs$xlsx_tabs_ok <- all(rv$checklist$xlsx_tabs %in% rv$inputs$xlsx_tabs)

    })

    ## + Outputs ===============================================================

    ## Download example 1 if needed
    output$dl_template <- downloadHandler(
      filename <- function() { "template1.xlsx" },
      content  <- function(file) { file.copy(system.file("extdata/example1.xlsx", package = "mocaredd"), file) }
    )

    ## + UI changes ============================================================

    ## Show hide data ok if tabs are correct

    observe({
      req(rv$inputs$xlsx_tabs)

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

      shinyWidgets::updateProgressBar(session = session, id = "prog_allchecks", value = 0, status = "primary")

      ## ++ Read data ----------------------------------------------------------
      rv$inputs$usr  <- readxl::read_xlsx(rv$inputs$xlsx_path, sheet = "user_inputs", na = "NA")
      rv$inputs$time <- readxl::read_xlsx(rv$inputs$xlsx_path, sheet = "time_periods", na = "NA")
      rv$inputs$ad   <- readxl::read_xlsx(rv$inputs$xlsx_path, sheet = "AD_lu_transitions", na = "NA")
      rv$inputs$cs   <- readxl::read_xlsx(rv$inputs$xlsx_path, sheet = "c_stock", na = "NA")

      shinyWidgets::updateProgressBar(session = session, id = "prog_allchecks", value = 25)

      ## ++ Run checks ---------------------------------------------------------
      ## Check tables have at least the correct columns
      rv$checks$cols_ok <- all(
        rv$checklist$col_usr %in% names(rv$inputs$usr),
        rv$checklist$col_time %in% names(rv$inputs$time),
        rv$checklist$col_ad %in% names(rv$inputs$ad),
        rv$checklist$col_cs %in% names(rv$inputs$cs)
      )

      ## Check category variables are correct
      rv$check_cats_ok <- all(

      )


      ## Check data types are correct

      ## !!! NEED TO REVISE CHECK FUNCTION TO INCLUDE ALL CHECKS
      rv$check$check_data_ok <- fct_check_data(.ad = rv$inputs$ad, .cs = rv$inputs$cs, .init = rv$checklist)

      ## !!! FOR NOW ONLY, needs to be true or false based on checks
      rv$checks$all_ok <- TRUE

      shinyWidgets::updateProgressBar(session = session, id = "prog_allchecks", value = 50)

      ## ++ Calculations -------------------------------------------------------
      Sys.sleep(2)

      shinyWidgets::updateProgressBar(session = session, id = "prog_allchecks", value = 75)

      ## ++ Outputs ------------------------------------------------------------
      ## outputs are calculated once new data is uploaded so they are performed here
      ## instead of the render*({}) functions

      Sys.sleep(2)

      shinyWidgets::updateProgressBar(session = session, id = "prog_allchecks", value = 100, status = "success")

      ## ++ Finalize -----------------------------------------------------------
      rv$checks$all_done <- TRUE


    })

    ## + Outputs ===============================================================
    output$vb_nb_time <- renderText({
      req(rv$checks$all_ok)
      if (rv$checks$all_ok) {
        paste0(nrow(rv$inputs$time), " time periods reported.")
      }
    })

    output$vb_nb_ref <- renderText({
      req(rv$checks$all_ok)
      if (rv$checks$all_ok) {
        time_sub <- rv$inputs$time |> dplyr::filter(stringr::str_detect(period_type, pattern = "REF"))
        paste0(nrow(time_sub), " time periods are for reference.")
      }
    })

    output$vb_nb_mon <- renderText({
      req(rv$checks$all_ok)
      if (rv$checks$all_ok) {
        time_sub <- rv$inputs$time |> dplyr::filter(stringr::str_detect(period_type, pattern = "M"))
        paste0(nrow(time_sub), " time periods are for monitoring.")
      }
    })

    # vb_ad <- value_box(
    #   title = "Land use transitions",
    #   value = textOutput(ns("vb_nb_trans")),
    #   showcase = bsicons::bs_icon("pin-map"),
    #   theme = "secondary",
    #   textOutput(ns("vb_nb_lu")),
    #   textOutput(ns("vb_nb_mon"))
    # )
    #
    # vb_cs <- value_box(
    #   title = "Carbon stock",
    #   value = textOutput(ns("vb_nb_pools")),
    #   showcase = bsicons::bs_icon("arrow-repeat"),
    #   theme = "warning",
    #   #textOutput(ns("vb_dg_method")),
    # )




    ## Update show / hide panels
    observe({
      req(rv$checks$all_done)
      shinyjs::show("check_show")
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
