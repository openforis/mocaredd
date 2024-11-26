#' Tool module UI function
#'
#' @noRd
mod_tool_UI_fail <- function(id, i18n){

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
      #shiny::verbatimTextOutput(outputId = ns("ctrl_input"))
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
    ),
    div(
      #id = ns("res_show"),
      actionButton(inputId = ns("btn_show_res"), label = "Show simulation results")
    )

  )

  ## ++ Accordion 3: Run Sensitivity -------------------------------------------
  ac_sens <-  accordion_panel(
    title = i18n$t("Perform senstivity analysis"),
    icon = bsicons::bs_icon("3-circle"),
    value = ns("ac_sens"),

    ## Content
    h4("Coming Soon")
  )

  ## + Initial message =========================================================

  div_check_init <- div(
    id = ns("check_init_msg"),
    bsicons::bs_icon("arrow-left"), " Start with uploading your data in the sidebar.",
    class = "text-warning",
    style = "font-style: italic;"
  )




  ## + Results panel ===========================================================

  ## ++ Res initial text -------------------------------------------------------
  div_res_init <- div(
    id = ns("res_init"),
    bsicons::bs_icon("gear"), i18n$t(" Run simulations in the sidebar."),
    class = "text-warning",
    style = "font-style: italic;"
  )

  ## ++ Res progress bar -------------------------------------------------------
  div_res_progress <- shinyjs::hidden(div(
    id = ns("res_progress"),
    shinyWidgets::progressBar(
      id = ns("prog_res"),
      value = 0,
      title = "Simulations progress",
      display_pct = TRUE
    )
  ))

  # div_res_show <- shinyjs::hidden(div(
  #   id = ns("res_show"),
  #   actionButton(inputId = ns("btn_show_res"), label = "Show simulation results")
  # ))

  ## ++ Res cards --------------------------------------------------------------
  card_res_dl <- card(
    card_body(
      fillable = FALSE,
      h5(i18n$t("Download the simulations and aggregated results")),
      downloadButton(
        outputId = ns("dl_ari"),
        label = "Download the arithmetic mean ERs", class = "btn-outline-secondary btn-small form-group"
      ),
      downloadButton(
        outputId = ns("dl_res"),
        label = "Download the simulated ERs", class = "btn-outline-secondary btn-small form-group"
      ),
      downloadButton(
        outputId = ns("dl_sim_ER"),
        label = "Download all the ER simulations", class = "btn-outline-warning btn-small form-group"
      ),
      downloadButton(
        outputId = ns("dl_sim_trans"),
        label = "Download all the land use transition simulations", class = "btn-outline-warning btn-small form-group"
      ),
      # downloadButton(
      #   outputId = ns("dl_fp"),
      #   label = "Download all the forest plots", class = "btn-outline-primary btn-small form-group"
      # )
    )
  )

  card_res_fp <- card(
    h5(i18n$t("Emission reductions details")),
    gt::gt_output(ns("res_ER_fp"))
  )

  card_res_gg <- card(
    h5(i18n$t("Emission reductions histogram")),
    uiOutput(outputId = ns("res_select_ER_hist_UI")),
    plotOutput(ns("res_ER_hist"))
  )

  card_redd_fp <- card(
    h5(i18n$t("REDD+ Activity details")),
    gt::gt_output(ns("res_redd_fp"))
  )

  card_redd_hist <- card(
    h5(i18n$t("REDD+ activity histograms")),
    uiOutput(outputId = ns("res_select_redd_hist_UI")),
    uiOutput(outputId = ns("res_select_period_hist_UI")),
    plotOutput(ns("res_redd_hist"))
  )

  # card_trans_fp <- card(
  #   h5(i18n$t("Land use transition period")),
  #   gt::gt_output(ns("res_trans_fp"))
  # )

  ## +++ combine cards
  div_res_cards <- shinyjs::hidden(div(
    id = ns("res_cards"),
    card_res_dl,
    layout_columns(col_widths = c(6, 6), card_res_fp, card_res_gg),
    layout_columns(col_widths = c(8, 4), card_redd_fp, card_redd_hist),
    #card_trans_fp
  ))

  ##
  ## Layout UI elements with tagList() function ################################
  ##

  tagList(

    h2(i18n$t("Run the uncertainty analysis")),

    br(),

    layout_sidebar(
      id = ns("tool_tabs"),

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

      div_check_init,

      shinyjs::hidden(div(
        id = ns("div_check_UI"),
        submod_check_UI("tab_check", i18n = i18n)
      )),

      # shinyjs::hidden(div(
      #   id = ns("div_res_UI"),
      #   submod_res_UI("tab_res", i18n = i18n)
      # )),

      # shinyjs::hidden(div(
      #   id = ns("div_sens_UI"),
      #   submod_sensitivity_UI("tab_sens", i18n = i18n)
      #   )),


      ##########


      # nav_panel(
      #   title = i18n$t("Check your data"),
      #   value = ns("check_panel"),
      #   icon = icon("circle-check"),
      #   #submod_check_UI("tab_check", i18n = i18n)
      #   ## Initial msg
      #   div_check_init,
      #   ## progress bar
      #   div_check_progress,
      #   div_btn_show_check,
      #   ## Checks
      #   div_check_vbs,
      #   br(),
      #   div_check_cards
      # ),

      ## + MCS panel ===========================================================

      # nav_panel(
      #   title = i18n$t("Results"),
      #   value = ns("res_panel"),
      #   icon = icon("chart-simple"),
      #   #submod_res_UI("tab_res", i18n = i18n)
      #   div_res_init,
      #   ## progress bar
      #   div_res_progress,
      #   #div_res_show,
      #   ## cards
      #   div_res_cards
      # ),
      #
      # ## + Sensitivity analysis panel ============================================
      #
      # nav_panel(
      #   title = i18n$t("Sensitivity"),
      #   value = ns("sensi_panel"),
      #   icon = icon("magnifying-glass"),
      #   submod_sensitivity_UI("tab_sensitivity", i18n = i18n)
      # )

    ) ## END layout_sidebar()

  ) ## END tagList

} ## END module UI function
