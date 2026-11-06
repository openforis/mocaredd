#' Tool module UI function (v2)
#'
#' Changes from v1:
#' - div_check_progress now includes a scrollable console div and a
#'   "Show check results" button, mirroring the arenalytics data-upload pattern.
#' - div_btn_show_check removed (button absorbed into div_check_progress).
#' - card_check_msg removed from the check panel: check outcomes are now
#'   displayed in the console div rather than a separate gt table.
#'
#' @noRd
mod_tool_UI2 <- function(id, i18n, .tr){

  ns <- NS(id)


  ##
  ## UI Elements ###############################################################
  ##

  ## + Sidebar =================================================================

  ## ++ Accordion 1: load data -------------------------------------------------
  ac_load <- accordion_panel(
    title = i18n$t(.tr$ac1_title),
    icon  = bsicons::bs_icon("1-circle"),
    value = ns("ac_load"),

    div(
      p(i18n$t(.tr$ac1_p1)),
      downloadButton(
        outputId = ns("dl_template_v2_simple"),
        label    = i18n$t(.tr$ac1_dl_v2_simple),
        class    = "btn-primary btn-sm form-group"
      ),
      downloadButton(
        outputId = ns("dl_template_v2_inter"),
        label    = i18n$t(.tr$ac1_dl_v2_inter),
        class    = "btn-primary btn-sm form-group"
      ),
      div(
        downloadButton(
          outputId = ns("dl_template_v1_simple"),
          label    = i18n$t(.tr$ac1_dl_v1),
          class    = "btn-primary btn-sm form-group"
        ),
        style = "color: grey;"
      ),
      style = "margin-bottom: 0.5rem;"
    ),

    div(
      p(i18n$t(.tr$ac1_p2)),
      fileInput(
        inputId = ns("load_xlsx"),
        accept  = ".xlsx",
        label   = NULL
      ) |>
        ## FAO primary button for "Browse..." (fileInput() has no button class argument)
        htmltools::tagQuery() |>
        (\(x) x$find(".btn-file")$removeClass("btn-default")$addClass("btn-primary")$allTags())()
    ),

    ## Status messages
    div(
      id    = ns("msg_no_data"),
      i18n$t(.tr$ac1_msg_nodata),
      class = "text-warning",
      style = "font-style: italic;"
    ),
    shinyjs::hidden(div(
      id    = ns("msg_data_tabs_ok"),
      i18n$t(.tr$ac1_msg_ok),
      class = "text-success",
      style = "font-style: italic;"
    )),
    shinyjs::hidden(div(
      id    = ns("msg_data_tabs_wrong"),
      i18n$t(.tr$ac1_msg_err),
      class = "text-danger",
      style = "font-style: italic;"
    )),

    div(
      shinyjs::disabled(
        actionButton(inputId = ns("btn_run_checks"), label = i18n$t(.tr$ac1_btn), class = "btn-primary")
      ),
      style = "margin-top: 1rem;"
    )
  )


  ## ++ Accordion 2: Run MCS ---------------------------------------------------
  ac_mcs <- accordion_panel(
    title = i18n$t(.tr$ac2_title),
    icon  = bsicons::bs_icon("2-circle"),
    value = ns("ac_mcs"),

    div(
      id    = ns("msg_no_check"),
      i18n$t(.tr$ac2_msg_nocheck),
      class = "text-warning",
      style = "font-style: italic;"
    ),
    shinyjs::hidden(div(
      id    = ns("msg_checks_ok"),
      i18n$t(.tr$ac2_msg_ok),
      class = "text-success",
      style = "font-style: italic;"
    )),
    shinyjs::hidden(div(
      id    = ns("msg_checks_wrong"),
      i18n$t(.tr$ac2_msg_err),
      class = "text-danger",
      style = "font-style: italic;"
    )),

    div(
      shinyjs::disabled(
        actionButton(inputId = ns("btn_run_mcs"), label = i18n$t(.tr$ac2_btn), class = "btn-primary")
      ),
      style = "margin-top: 1rem;"
    )
  )


  ## ++ Accordion 3: Sensitivity -----------------------------------------------
  ac_sens <- accordion_panel(
    title = i18n$t(.tr$ac3_title),
    icon  = bsicons::bs_icon("3-circle"),
    value = ns("ac_sens"),

    div(
      id    = ns("msg_sens_no_check"),
      i18n$t(.tr$ac3_msg_nocheck),
      class = "text-warning",
      style = "font-style: italic;"
    ),
    shinyjs::hidden(div(
      id    = ns("msg_sens_ready"),
      i18n$t(.tr$ac3_msg_ready),
      class = "text-success",
      style = "font-style: italic;"
    )),

    div(
      shinyjs::disabled(
        actionButton(inputId = ns("btn_run_sens"), label = i18n$t(.tr$ac3_btn), class = "btn-primary")
      ),
      style = "margin-top: 1rem;"
    )
  )


  ## + Step status bar =========================================================
  ## One box per step. The server sets its state class (step-todo, step-running,
  ## step-done, step-error) and style.css shows the matching icon and status.
  step_box <- function(step, num, label) {
    div(
      id    = ns(paste0("step_", step)),
      class = "step-box step-todo",
      span(
        class = "step-icon", `aria-hidden` = "true",
        span(class = "step-num", num),
        bsicons::bs_icon("check-lg", class = "step-tick"),
        bsicons::bs_icon("exclamation-lg", class = "step-cross"),
        span(class = "step-spin")
      ),
      div(
        class = "step-text",
        span(class = "step-label", label),
        span(class = "step-status st-todo",    i18n$t(.tr$step_todo)),
        span(class = "step-status st-running", i18n$t(.tr$step_running)),
        span(class = "step-status st-done",    i18n$t(.tr$step_done)),
        span(class = "step-status st-error",   i18n$t(.tr$step_error))
      )
    )
  }

  div_steps <- div(
    class = "step-bar", `aria-live` = "polite",
    step_box("data",  1, i18n$t(.tr$step_data)),
    step_box("check", 2, i18n$t(.tr$step_check)),
    step_box("mcs",   3, i18n$t(.tr$step_mcs)),
    step_box("sens",  4, i18n$t(.tr$tab_sens))
  )


  ## + Check panel =============================================================

  ## ++ Initial message --------------------------------------------------------
  div_check_init <- div(
    id    = ns("check_init_msg"),
    bsicons::bs_icon("arrow-left"), i18n$t(.tr$check_msg_init),
    class = "text-warning",
    style = "font-style: italic;"
  )

  ## ++ Progress bar + console -------------------------------------------------
  ## Hidden on load; revealed when btn_run_checks is clicked.
  ## The console div receives one <br>-terminated line per message() emitted
  ## by fct_checkinput() via shinyjs::html(..., add = TRUE).
  ## The "Show check results" button is enabled by the server once all checks
  ## have completed; clicking it hides this div and reveals the results below.
  div_check_progress <- shinyjs::hidden(div(
    id = ns("check_progress"),

    h4(i18n$t(.tr$check_progress_title)),

    shinyWidgets::progressBar(
      id          = ns("prog_allchecks"),
      value       = 0,
      title       = i18n$t(.tr$check_progress_start),
      display_pct = TRUE
    ),

    br(),

    ## Scrollable console output
    div(id = ns("check_console"), class = "app-console"),

    br(),

    shinyjs::disabled(
      actionButton(inputId = ns("btn_show_checks"), label = i18n$t(.tr$check_btn_show), class = "btn-primary")
    )
  ))


  ## ++ Check panel ------------------------------------------------------------

  ## Value boxes
  vb_time <- value_box(
    title    = i18n$t(.tr$vb_time_title),
    value    = htmlOutput(ns("vb_nb_time")),
    showcase = bsicons::bs_icon("calendar3", size = "40px"),
    theme    = "primary",
    textOutput(ns("vb_nb_ref")),
    textOutput(ns("vb_nb_mon"))
  )

  vb_ad <- value_box(
    title    = i18n$t(.tr$vb_ad_title),
    value    = htmlOutput(ns("vb_nb_trans")),
    showcase = bsicons::bs_icon("pin-map", size = "40px"),
    theme    = "secondary",
    textOutput(ns("vb_nb_lu")),
    textOutput(ns("vb_nb_redd"))
  )

  vb_cs <- value_box(
    title    = i18n$t(.tr$vb_cs_title),
    value    = htmlOutput(ns("vb_nb_pools")),
    showcase = bsicons::bs_icon("arrow-repeat", size = "48px"),
    theme    = "warning",
    textOutput(ns("vb_c_pools")),
    textOutput(ns("vb_dg_method"))
  )

  ## Check cards
  card_result_table <- card(
    card_header(i18n$t(.tr$check_card_table)),
    gt::gt_output(ns("check_result_table"))
  )

  card_arithmetic_gg <- card(
    card_header(i18n$t(.tr$check_card_gg)),
    plotOutput(ns("check_arithmetic_gg"), height = "500px")
  )

  card_lumatrix <- card(
    card_header(i18n$t(.tr$check_card_lumatrix)),
    layout_column_wrap(
      width = "200px", fixed_width = TRUE,
      uiOutput(outputId = ns("check_select_period_UI")),
      div(
        verbatimTextOutput(outputId = ns("check_show_period_type")),
        style = "margin-top: 29px;"
      )
    ),
    gt::gt_output(ns("check_lumatrix"))
  )

  ## Check panel layout: 3 rows
  ##  1. value boxes
  ##  2. result table and arithmetic figure (50% width each)
  ##  3. land use change matrix
  div_check_panel <- shinyjs::hidden(div(
    id = ns("check_panel"),
    layout_column_wrap(
      fill = FALSE,
      vb_time, vb_ad, vb_cs
    ),
    br(),
    layout_column_wrap(
      width = 1/2, fill = FALSE,
      card_result_table,
      card_arithmetic_gg
    ),
    br(),
    card_lumatrix
  ))


  ## + Results panel ===========================================================

  div_res_init <- div(
    id    = ns("res_init"),
    bsicons::bs_icon("gear"), i18n$t(.tr$res_msg_init),
    class = "text-warning",
    style = "font-style: italic;"
  )

  div_res_progress <- shinyjs::hidden(div(
    id = ns("res_progress"),
    shinyWidgets::progressBar(
      id          = ns("prog_res"),
      value       = 0,
      title       = i18n$t(.tr$res_progress_title),
      display_pct = TRUE
    )
  ))

  div_res_show <- shinyjs::hidden(div(
    id = ns("res_show"),
    actionButton(inputId = ns("btn_show_res"), label = i18n$t(.tr$res_btn_show), class = "btn-primary")
  ))

  ## ++ Res cards --------------------------------------------------------------
  card_res_dl <- card(
    card_header(i18n$t(.tr$res_card_dl)),
    card_body(
      fillable = FALSE,
      downloadButton(
        outputId = ns("dl_ari"),
        label    = i18n$t(.tr$res_dl_ari),
        class    = "btn-primary btn-sm form-group"
      ),
      downloadButton(
        outputId = ns("dl_res"),
        label    = i18n$t(.tr$res_dl_res),
        class    = "btn-primary btn-sm form-group"
      ),
      downloadButton(
        outputId = ns("dl_sim_ER"),
        label    = i18n$t(.tr$res_dl_sim_er),
        class    = "btn-primary btn-sm form-group"
      ),
      downloadButton(
        outputId = ns("dl_sim_trans"),
        label    = i18n$t(.tr$res_dl_sim_trans),
        class    = "btn-primary btn-sm form-group"
      )
    )
  )

  card_res_fp <- card(
    full_screen = TRUE,
    card_header(i18n$t(.tr$res_card_er_fp)),
    gt::gt_output(ns("res_ER_fp"))
  )

  card_res_gg <- card(
    full_screen = TRUE,
    card_header(i18n$t(.tr$res_card_er_hist)),
    uiOutput(outputId = ns("res_select_ER_hist_UI")),
    plotOutput(ns("res_ER_hist"))
  )

  card_redd_fp <- card(
    full_screen = TRUE,
    card_header(i18n$t(.tr$res_card_redd_fp)),
    gt::gt_output(ns("res_redd_fp"))
  )

  card_redd_hist <- card(
    full_screen = TRUE,
    card_header(i18n$t(.tr$res_card_redd_hist)),
    uiOutput(outputId = ns("res_select_redd_hist_UI")),
    uiOutput(outputId = ns("res_select_period_hist_UI")),
    plotOutput(ns("res_redd_hist"))
  )

  div_res_cards <- shinyjs::hidden(div(
    id = ns("res_cards"),
    card_res_dl,
    layout_columns(col_widths = c(8, 4), card_res_fp, card_res_gg),
    layout_columns(col_widths = c(8, 4), card_redd_fp, card_redd_hist)
  ))


  ## + Sensitivity panel =======================================================
  div_sens_init <- div(
    id    = ns("sens_init"),
    bsicons::bs_icon("gear"), i18n$t(.tr$sens_msg_init),
    class = "text-warning",
    style = "font-style: italic;"
  )

  ## ++ Sens cards -------------------------------------------------------------
  card_sens_var <- card(
    full_screen = TRUE,
    card_header(i18n$t(.tr$sens_card_var)),
    p(
      i18n$t(.tr$sens_card_var_txt),
      class = "text-muted small"
    ),
    plotOutput(ns("sens_var_gg"))
  )

  card_sens_tornado <- card(
    full_screen = TRUE,
    card_header(i18n$t(.tr$sens_card_ad)),
    p(
      i18n$t(.tr$sens_card_ad_txt),
      class = "text-muted small"
    ),
    layout_column_wrap(
      width = "200px", fixed_width = TRUE,
      selectInput(inputId = ns("sens_select_output"), label = i18n$t(.tr$sens_select_output), choices = NULL),
      numericInput(inputId = ns("sens_top_n"), label = i18n$t(.tr$sens_top_n), value = 20, min = 5, max = 200, step = 5)
    ),
    plotOutput(ns("sens_tornado_gg"), height = "auto")
  )

  card_sens_dl <- card(
    card_header(i18n$t(.tr$sens_card_dl)),
    card_body(
      fillable = FALSE,
      downloadButton(
        outputId = ns("dl_sens_var"),
        label    = i18n$t(.tr$sens_dl_var),
        class    = "btn-primary btn-sm form-group"
      ),
      downloadButton(
        outputId = ns("dl_sens_ad"),
        label    = i18n$t(.tr$sens_dl_ad),
        class    = "btn-primary btn-sm form-group"
      )
    )
  )

  ## ++ Transition details (from the simulations) ------------------------------
  card_sens_trans <- card(
    full_screen = TRUE,
    card_header(i18n$t(.tr$sens_card_trans)),
    div(
      id    = ns("sens_trans_init"),
      i18n$t(.tr$sens_trans_init),
      class = "text-warning",
      style = "font-style: italic;"
    ),
    gt::gt_output(ns("sens_trans_fp"))
  )

  div_sens_cards <- shinyjs::hidden(div(
    id = ns("sens_cards"),
    card_sens_dl,
    layout_columns(col_widths = c(5, 7), card_sens_var, card_sens_tornado),
    card_sens_trans
  ))


  ##
  ## Layout ####################################################################
  ##

  tagList(

    div_steps,

    navset_card_tab(
      id = ns("tool_tabs"),

      sidebar = sidebar(
        width = "300px",
        accordion(
          open     = TRUE,
          multiple = TRUE,
          ac_load, ac_mcs, ac_sens
        )
      ),

      nav_spacer(),

      ## + Check panel =========================================================
      nav_panel(
        title = i18n$t(.tr$tab_check),
        value = "check_tab",
        icon  = icon("circle-check"),
        div_check_init,
        div_check_progress, ## progress bar + console + show-results button
        div_check_panel
      ),

      ## + Results panel =======================================================
      nav_panel(
        title = i18n$t(.tr$tab_res),
        value = "res_tab",
        icon  = icon("chart-simple"),
        div_res_init,
        div_res_progress,
        div_res_show,
        div_res_cards
      ),

      ## + Sensitivity panel ===================================================
      nav_panel(
        title = i18n$t(.tr$tab_sens),
        value = "sensi_tab",
        icon  = icon("magnifying-glass"),
        div_sens_init,
        div_sens_cards
      )

    ) ## END navset_card_tab()

  ) ## END tagList

} ## END module UI function
