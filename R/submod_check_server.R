#' Tool sub-module for checks server function
#'
#' @noRd
submod_check_server <- function(id, rv) {

  moduleServer(id, function(input, output, session) {

    ns <- session$ns

    ##
    ## Events ##################################################################
    ##

    ## + Read data =============================================================

    observeEvent(rv$inputs$btn_run_checks, {

      rv$inputs$time <- readxl::read_xlsx(input$load_xlsx$datapath, sheet = "time_period", na = "NA")
      rv$inputs$time <- readxl::read_xlsx(input$load_xlsx$datapath, sheet = "time_period", na = "NA")
      rv$inputs$time <- readxl::read_xlsx(input$load_xlsx$datapath, sheet = "time_period", na = "NA")
      rv$inputs$time <- readxl::read_xlsx(input$load_xlsx$datapath, sheet = "time_period", na = "NA")

      #shinyWidgets::updateProgressBar(session = session, id = "prog_checks", value = 100, status = "success")



    })


    ## + Events ================================================================

    ## + Outputs ===============================================================

    ## + UI changes ============================================================

    ## Show hide data ok if tabs are correct

    observe({
      req(rv$inputs$xlsx_tabs)

      if(rv$inputs$xlsx_tabs_ok) {
        shinyjs::hide("msg_no_data")
        shinyjs::show("msg_data_tabs_ok")
        shinyjs::hide("msg_data_tabs_wrong")
        shinyjs::enable("run_check")
      } else {
        shinyjs::hide("msg_no_data")
        shinyjs::hide("msg_data_tabs_ok")
        shinyjs::show("msg_data_tabs_wrong")
        shinyjs::disable("run_check")
      }

    })





  }) ## END module server function

}
