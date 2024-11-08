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

      rv$inputs$xlsx_tabs <- readxl::excel_sheets(input$load_xlsx$datapath)

      rv$inputs$xlsx_tabs_ok <- all(app_checklist$xlsx_tabs %in% rv$inputs$xlsx_tabs)

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
        #shinyjs::enable("calc_cv")
      } else {
        shinyjs::hide("msg_no_data")
        shinyjs::hide("msg_data_tabs_ok")
        shinyjs::show("msg_data_tabs_wrong")
        #shinyjs::disable("calc_cv")
      }

    })





  }) ## END module server function

}
