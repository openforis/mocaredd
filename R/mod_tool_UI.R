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
    value = textOutput(ns("vb_nb_time")),
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
    textOutput(ns("vb_nb_lu"))
  )

  vb_cs <- value_box(
    title = "Carbon stock",
    value = textOutput(ns("vb_nb_pools")),
    showcase = bsicons::bs_icon("arrow-repeat", size = "48px"),
    theme = "warning",
    #textOutput(ns("vb_dg_method")),
  )

  ## Combine value boxes
  div_value_boxes <- shinyjs::hidden(div(
    id = ns("check_vbs"),
    layout_column_wrap(
      width = "200px",
      fill = FALSE,
      vb_time, vb_ad, vb_cs
    )
  ))

  ## ++ Cards ------------------------------------------------------------------
  ## All check types
  # rv$checks$cols_ok,
  # rv$checks$size_ok,
  # rv$checks$datatypes_ok,
  # rv$checks$cats_ok,
  # rv$checks$ids_ok,
  # rv$checks$matches_ok

  card_cols <- card(
    h4("Check all column names are valid"),
      verbatimTextOutput(ns("card_cols"))
  )

  card_size <- card(
    h4("Check all column names are valid"),
    textOutput(ns("card_size"), container = p)
  )

  card_datatypes <- card(
    h4("Check all column names are valid"),
    textOutput(ns("card_datatypes"), container = p)
  )

  card_cats <- card(
    h4("Check all categories are valid"),
    textOutput(ns("card_cats"), container = p)
  )

  card_ids <- card(
    h4("Check all categories are valid"),
    textOutput(ns("card_ids"), container = p)
  )

  card_matches <- card(
    h4("Land use categories match"),
    textOutput(ns("card_ids"), container = p)
  )

  ## combine cards
  div_cards <- shinyjs::hidden(div(
    id = ns("check_cards"),
    # layout_column_wrap(
    #   width = "200px",
      card_cols, card_size, card_datatypes, card_cats, card_ids, card_matches
    # )
  ))



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

      ## Spacer to right align menu items
      nav_spacer(),

      ## + Checks panel ========================================================

      nav_panel(
        title = i18n$t("Check your data"),
        value = "about",
        icon = icon("circle-check"),
        #submod_check_UI("tab_check", i18n = i18n)
        ## Initial msg
        div_check_init,
        ## progress bar
        div_check_progress,
        div_btn_show_check,
        div_value_boxes,
        br(),
        div_cards
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
