


submod_check_UI <- function(id, i18n){

  ## From https://shiny.rstudio.com/articles/modules.html
  # `NS(id)` returns a namespace function, which was save as `ns` and will
  # invoke later.
  ns <- NS(id)

  ##
  ## UI Elements ###############################################################
  ##

  ## + Progress bar ============================================================

  div_check_progress <- div(
    id = ns("check_progress"),
    shinyWidgets::progressBar(
      id = ns("prog_allchecks"),
      value = 0,
      title = "Data checks progress",
      display_pct = TRUE
    )
  )

  div_btn_show_check <- div(
    id = ns("check_show"),
    actionButton(inputId = ns("btn_show_checks"), label = "Show data checks")
  )

  ## + Value boxes =============================================================

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
  div_check_vbs <- div(
    id = ns("check_vbs"),
    layout_column_wrap(
      #width = "200px",
      fill = FALSE,
      vb_time, vb_ad, vb_cs
    )
  )

  ## + Cards ===================================================================

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
    layout_column_wrap(
      width = "200px", fixed_width = TRUE,
      uiOutput(outputId = ns("check_select_period_UI")),
      div(
        verbatimTextOutput(outputId = ns("check_show_period_type")),
        style = "margin-top:29px;" ## 21px txt + 8px margin to align with selectInput
      )
    ),
    gt::gt_output(ns("check_lumatrix"))
  )

  ## combine cards
  div_check_cards <- div(
    id = ns("check_cards"),
    layout_columns(
      card_check_msg, card_arithmetic_gg
    ),
    card_lumatrix
  )


  ##
  ## Layout UI elements with tagList() function ################################
  ##

  tagList(

    h4(icon("circle-check"), i18n$t("Check your data")),
    ## progress bar
    div_check_progress,
    div_btn_show_check,
    ## Checks
    div_check_vbs,
    br(),
    div_check_cards

  ) ## END tagList

} ## END module UI function
