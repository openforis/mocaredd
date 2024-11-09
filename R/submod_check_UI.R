


submod_check_UI <- function(id, i18n){

  ## From https://shiny.rstudio.com/articles/modules.html
  # `NS(id)` returns a namespace function, which was save as `ns` and will
  # invoke later.
  ns <- NS(id)

  ##
  ## UI Elements ###############################################################
  ##

  ## + Progress bar ============================================================

  div_check_progress <- shinyjs::hidden(div(
    id = ns("check_progress"),

    shinyWidgets::progressBar(
      id = ns("prog_allchecks"),
      value = 0,
      title = "Data checks progress",
      display_pct = TRUE
    )

  ))

  div_btn_to_check <- shinyjs::hidden(div(
    id = ns("progress_to_checks"),

    actionButton(inputId = ns("btn_show_checks"), label = "Show data checks")

  ))

  ## + Value boxes =============================================================

  vb_time <- value_box(
    title = "Time periods",
    value = textOutput(ns("vb_nb_time")),
    showcase = bsicons::bs_icon("calendar3"),
    theme = "primary",
    textOutput(ns("vb_nb_ref")),
    textOutput(ns("vb_nb_mon"))
  )

  vb_ad <- value_box(
    title = "Land use transtions",
    value = textOutput(ns("vb_nb_trans")),
    showcase = bsicons::bs_icon("pin-map"),
    theme = "secondary",
    textOutput(ns("vb_nb_lu")),
    textOutput(ns("vb_nb_mon"))
  )

  vb_cs <- value_box(
    title = "Carbon stock",
    value = textOutput(ns("vb_nb_pools")),
    showcase = bsicons::bs_icon("arrow-repeat"),
    theme = "info",
    #textOutput(ns("vb_dg_method")),
  )

  ## + Cards ===================================================================

  card_cat <- card(
    h4("Check all categories are valid"),
    p(
      "TEXT"
    ),
  )

  card_lu <- card(
    h4("Land use categories match"),
    p(
      "TEXT"
    )
  )

  card_dg <- card(
    h4("Forest degradation calculation method"),
    p(
      "TEXT"
    )
  )



  ##
  ## Layout UI elements with tagList() function ################################
  ##

  tagList(

    ## Initial msg
    div(
      id = ns("check_init_msg"),
      bsicons::bs_icon("arrow-left"), " Start with uploading your data in the sidebar.",
      class = "text-warning",
      style = "font-style: italic;"
    ),

    ## progress bar
    div_check_progress,
    div_btn_to_check,

    layout_column_wrap(
      width = "200px",
      vb_time, vb_ad, vb_cs
    ),

    layout_column_wrap(
      width = "200px",
      card_cat, card_lu, card_dg
    ),

  ) ## END tagList

} ## END module UI function
