#' Tool module UI function
#'
#' @noRd
mod_tool_UI <- function(id, i18n){

  ## From https://shiny.rstudio.com/articles/modules.html
  # `NS(id)` returns a namespace function, which was save as `ns` and will
  # invoke later.
  ns <- NS(id)

  ##
  ## UI Elements ###############################################################
  ##

  ## + Sidebar =================================================================

  ## ++ Accordion 1: load data -------------------------------------------------
  ac_load <- accordion_panel(
    title = i18n$t("Upload your data"),
    icon = bsicons::bs_icon("1-circle"),
    value = ns("ac_load"),

    ## Accordion content
    div(
      p(
        "{mocaredd} only accepts XLSX files that follow a specific template.
         Download the template here if you haven't converted you data yet.",
      ),
      downloadButton(
        outputId = ns("dl_template"),
        label = "Download the template", class = "btn-outline-secondary btn-small form-group"
      ),
      style = "margin-bottom: 0.5rem;"
    ),
    div(
      p(
        "Once your data has converted to the app's template, you can upload your
         data here:",
      ),
      shiny::fileInput(
        inputId = ns("load_xlsx"),
        accept = ".xlsx",
        label = NULL
      )
    ),
    div(
      id = ns("msg_no_data"),
      "No data uploaded.",
      class = "text-warning",
      style = "font-style: italic;"
    ),
    shinyjs::hidden(div(
      id = ns("msg_data_tabs_ok"),
      "Data uploaded with correct tabs.",
      class = "text-success",
      style = "font-style: italic;"
    )),
    shinyjs::hidden(div(
      id = ns("msg_data_tabs_wrong"),
      "Data uploaded with incorrect tabs.",
      class = "text-danger",
      style = "font-style: italic;"
    )),
    br(),
    div(
      shinyjs::disabled(actionButton(
        inputId = ns("btn_run_checks"),
        label = "Run checks"
      )),
      style = "margin-top: 1rem;"
    )
  )

  ## ++ Accordion 2: Run checks ------------------------------------------------
  ac_check <- accordion_panel(
    title = i18n$t("Run data checks"),
    icon = bsicons::bs_icon("2-circle"),
    value = ns("ac_check"),

    ## Content
    h4("TEXT")

  )

  ## ++ Accordion 3: Run MCS ---------------------------------------------------
  ac_mcs <-  accordion_panel(
    title = i18n$t("Create Monte Carlo Simulations"),
    icon = bsicons::bs_icon("3-circle"),
    value = ns("ac_mcs"),

    ## Content
    h4("TEXT")

  )

  ## ++ Accordion 4: Run Sensitivity -------------------------------------------
  ac_sens <-  accordion_panel(
    title = i18n$t("Perform senstivity analysis"),
    icon = bsicons::bs_icon("4-circle"),
    value = ns("ac_sens"),

    ## Content
    h4("TEXT")
  )



  ##
  ## Layout UI elements with tagList() function ################################
  ##

  tagList(

    h2(i18n$t("Run the uncertainty analysis")),

    br(),

    navset_card_tab(

      ## + Sidebar =============================================================

      sidebar = sidebar(
        width = "300px",
        accordion(
          ac_load,
          ac_check,
          ac_mcs,
          ac_sens
          )
      ),

      ## + Checks panel ========================================================

      nav_panel(
        title = i18n$t("Check your data"),
        value = "about",
        icon = icon("circle-check"),
        submod_check_UI("tab_check", i18n = i18n)
      ),

      ## + MCS panel ===========================================================

      nav_panel(
        title = i18n$t("Results"),
        value = "res",
        icon = icon("chart-simple"),
        submod_res_UI("tab_res", i18n = i18n)
      ),

      ## + Sensitivity analysis panel ============================================

      nav_panel(
        title = i18n$t("Sensitivity"),
        value = "sensitivity",
        icon = icon("magnifying-glass"),
        submod_sensitivity_UI("tab_sensitivity", i18n = i18n)
      )

    ) ## END navset_card_tab()

  ) ## END tagList

} ## END module UI function
