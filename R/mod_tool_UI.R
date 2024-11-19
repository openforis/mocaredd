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
      ),
      shiny::verbatimTextOutput(outputId = ns("ctrl_input"))
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
    div(
      shinyjs::disabled(
        actionButton(
          inputId = ns("btn_run_checks"),
          label = "Run checks"
       )
      ),
      style = "margin-top: 1rem;"
    )

  )


  ## ++ Accordion 2: Run MCS ---------------------------------------------------
  ac_mcs <-  accordion_panel(
    title = i18n$t("Create Monte Carlo Simulations"),
    icon = bsicons::bs_icon("2-circle"),
    value = ns("ac_mcs"),

    ## Content
    div(
      id = ns("msg_no_check"),
      "Run checks first.",
      class = "text-warning",
      style = "font-style: italic;"
    ),
    shinyjs::hidden(div(
      id = ns("msg_checks_ok"),
      "All checks passed.",
      class = "text-success",
      style = "font-style: italic;"
    )),
    shinyjs::hidden(div(
      id = ns("msg_checks_wrong"),
      "Checks not passed.",
      class = "text-danger",
      style = "font-style: italic;"
    )),
    div(
      shinyjs::disabled(
        actionButton(
          inputId = ns("btn_run_mcs"),
          label = "Run simulations"
        )
      ),
      style = "margin-top: 1rem;"
    )

  )

  ## ++ Accordion 3: Run Sensitivity -------------------------------------------
  ac_sens <-  accordion_panel(
    title = i18n$t("Perform senstivity analysis"),
    icon = bsicons::bs_icon("3-circle"),
    value = ns("ac_sens"),

    ## Content
    h4("Not implemented yet")
  )

  ## + Check panel =============================================================

  ## ++ Initial text -----------------------------------------------------------
  div_check_init <- div(
    id = ns("check_init_msg"),
    bsicons::bs_icon("arrow-left"), " Start with uploading your data in the sidebar.",
    class = "text-warning",
    style = "font-style: italic;"
  )

  ## ++ Progress bar -----------------------------------------------------------
  div_check_progress <- shinyjs::hidden(div(
    id = ns("check_progress"),
    shinyWidgets::progressBar(
      id = ns("prog_allchecks"),
      value = 0,
      title = "Data checks progress",
      display_pct = TRUE
    )
  ))

  div_btn_show_check <- shinyjs::hidden(div(
    id = ns("check_show"),
    actionButton(inputId = ns("btn_show_checks"), label = "Show data checks")
  ))

  ## ++ Value boxes ------------------------------------------------------------
  vb_time <- value_box(
    title = "Time periods",
    value = htmlOutput(ns("vb_nb_time")),
    showcase = bsicons::bs_icon("calendar3", size = "40px"),
    theme = "primary",
    textOutput(ns("vb_nb_ref")),
    textOutput(ns("vb_nb_mon"))
  )

  vb_ad <- value_box(
    title = "Land use transitions",
    value = textOutput(ns("vb_nb_trans")),
    showcase = bsicons::bs_icon("pin-map", size = "40px"),
    theme = "secondary",
    textOutput(ns("vb_nb_lu")),
    textOutput(ns("vb_nb_redd"))
  )

  vb_cs <- value_box(
    title = "Carbon stock",
    value = textOutput(ns("vb_nb_pools")),
    showcase = bsicons::bs_icon("arrow-repeat", size = "48px"),
    theme = "warning",
    textOutput(ns("vb_c_level")),
    textOutput(ns("vb_dg_method")),
  )

  ## Combine value boxes
  div_value_boxes <- shinyjs::hidden(div(
    id = ns("check_vbs"),
    layout_column_wrap(
      #width = "200px",
      fill = FALSE,
      vb_time, vb_ad, vb_cs
    )
  ))

  ## ++ Cards ------------------------------------------------------------------
  card_check_msg <- card(
    h5(i18n$t("Check all column names are valid")),
      gt::gt_output(ns("check_msg"))
  )

  card_arithmetic_gg <- card(
    h5(i18n$t("Arithmetic mean emission reductions per period (tCO2e/y)")),
    plotOutput(ns("check_arithmetic_gg"))
  )

  card_lumatrix <- card(
    h5(i18n$t("Land use change matrix")),
    uiOutput(outputId = ns("check_select_period_UI")),
    gt::gt_output(ns("check_lumatrix"))
  )

  ## combine cards
  div_cards <- shinyjs::hidden(div(
    id = ns("check_cards"),
    layout_columns(
      card_check_msg, card_arithmetic_gg
    ),
    card_lumatrix
  ))


  ## + Results panel ===========================================================

  ## ++ Res initial text -------------------------------------------------------
  div_res_init <- div(
    id = ns("res_init_msg"),
    bsicons::bs_icon("gear"), i18n$t(" Run simulations in the sidebar."),
    class = "text-warning",
    style = "font-style: italic;"
  )

  ## ++ Res progress bar -------------------------------------------------------
  div_res_progress <- shinyjs::hidden(div(
    id = ns("div_res_progress"),
    shinyWidgets::progressBar(
      id = ns("prog_res"),
      value = 0,
      title = "Simulations progress",
      display_pct = TRUE
    )
  ))

  div_btn_show_res <- shinyjs::hidden(div(
    id = ns("res_show"),
    actionButton(inputId = ns("btn_show_res"), label = "Show simulation results")
  ))

  ## ++ Res Cards --------------------------------------------------------------
  card_tab_frl <- card(
    h5(i18n$t("Forest Reference Level")),
    gt::gt_output(ns("res_frl_tab"))
  )

  card_hist_frl <- card(
    h5(i18n$t("Forest Reference Level")),
    plotOutput(ns("res_frl_hist"))
  )

  card_trans_tab <- card(
    h5(i18n$t("Forest plot of simulation based results")),
    gt::gt_output(ns("res_trans_tab"))
  )

  ## +++ combine cards

  ##
  ## Layout UI elements with tagList() function ################################
  ##

  tagList(

    h2(i18n$t("Run the uncertainty analysis")),

    br(),

    navset_card_tab(
      id = ns("tool_panels"),

      ## + Sidebar =============================================================

      sidebar = sidebar(
        width = "300px",
        accordion(
          open = TRUE,
          multiple = TRUE,
          ac_load,
          ac_mcs,
          ac_sens
          )
      ),

      ## Spacer to right align menu items
      nav_spacer(),

      ## + Checks panel ========================================================

      nav_panel(
        title = i18n$t("Check your data"),
        value = ns("check_panel"),
        icon = icon("circle-check"),
        #submod_check_UI("tab_check", i18n = i18n)
        ## Initial msg
        div_check_init,
        ## progress bar
        div_check_progress,
        div_btn_show_check,
        ## Checks
        div_value_boxes,
        br(),
        div_cards
      ),

      ## + MCS panel ===========================================================

      nav_panel(
        title = i18n$t("Results"),
        value = ns("res_panel"),
        icon = icon("chart-simple"),
        #submod_res_UI("tab_res", i18n = i18n)
        div_res_init,
        ## progress bar
        div_res_progress,
        div_btn_show_res,
        card_trans_tab
      ),

      ## + Sensitivity analysis panel ============================================

      nav_panel(
        title = i18n$t("Sensitivity"),
        value = ns("sensi_panel"),
        icon = icon("magnifying-glass"),
        submod_sensitivity_UI("tab_sensitivity", i18n = i18n)
      )

    ) ## END navset_card_tab()

  ) ## END tagList

} ## END module UI function
