#' Tool sub-module for checks server function
#'
#' @noRd
submod_check_server <- function(id, rv) {

  moduleServer(id, function(input, output, session) {

    ns <- session$ns

    output$vb_nb_time <- renderText({
      req(rv$inputs$time)

      paste0(nrow(rv$inputs$time), "Time periods reported")

    })

    ##
    ## Events ##################################################################
    ##

    ## + Read data =============================================================

    # observeEvent(rv$inputs$btn_run_checks, {
    #
    #   rv$inputs$time <- readxl::read_xlsx(rv$inputs$xlsx_path, sheet = "time_periods", na = "NA")
    #   rv$inputs$ad <- readxl::read_xlsx(rv$inputs$xlsx_path, sheet = "AD_lu_transitions", na = "NA")
    #   rv$inputs$cs <- readxl::read_xlsx(rv$inputs$xlsx_path, sheet = "c_stock", na = "NA")
    #   rv$inputs$usr <- readxl::read_xlsx(rv$inputs$xlsx_path, sheet = "user_inputs", na = "NA")
    #
    #   shinyWidgets::updateProgressBar(session = session, id = "prog_checks", value = 25, status = "success")
    #
    #
    #
    # })


    ## + Events ================================================================

    ## + Outputs ===============================================================

    ## + UI changes ============================================================

    # observeEvent(rv$inputs$btn_run_checks, {
    #
    #   shinyjs::hide("check_init_msg")
    #   shinyjs::show("check_progress")
    #   shinyjs::hide("check_vbs")
    #   shinyjs::hide("check_cards")
    #
    #
    # })





  }) ## END module server function

}
