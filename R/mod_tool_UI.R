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

  ac_load <- accordion_panel(
    title = i18n$t("Upload your data"),
    icon = bsicons::bs_icon("1-circle"),
    value = ns("sb_load"),
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
  )



  ##
  ## Layout UI elements with tagList() function ######
  ##

  tagList(

    h2(i18n$t("Run the uncertainty analysis")),

    br(),

    navset_card_tab(

      sidebar = sidebar(
        width = "300px",
        accordion(
          ac_load,
          hr()

        ),
        hr(),
        "Data check 1",
        "Data check 2",
        "..."
      ),

      nav_panel(
        title = i18n$t("Check your data"),
        value = "about",
        icon = icon("circle-check"),
        submod_upload_UI("tab_upload_UI", i18n = i18n)
      ),

      nav_panel(
        title = i18n$t("Results"),
        value = "res",
        icon = icon("chart-simple"),
        submod_res_UI("tab_res_UI", i18n = i18n)
      ),

      nav_panel(
        title = i18n$t("Sensitivity"),
        value = "sensitivity",
        icon = icon("magnifying-glass"),
        mod_about_UI("tab_sensitivity_UI", i18n = i18n)
      )
    )

  ) ## END tagList

} ## END module UI function
