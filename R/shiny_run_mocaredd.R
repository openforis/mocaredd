#' Run Monte Carlo for REDD+ uncertainty's shiny app
#'
#' @description Starts a R Shiny application that guide users to fill in context and
#'              land use changes for forest related greenhouse gas emissions and removals.
#'              Inputs include distribution parameters for various distribution then the app
#'              returns error propagation and sensitivity analysis of the overall uncertainty
#'              for GHG estimates.
#'
#' @param ... arguments to pass to shinyApp
#'
#' @import shiny
#' @import bslib
#' @importFrom rlang .data
#'
#' @examples
#' if (interactive()) {
#'
#' shiny_run_mocaredd()
#'
#' }
#'
#' @export
shiny_run_mocaredd <- function(...) {

  ##
  ## GLOBAL ####################################################################
  ##

  ## + list of categories for checks ===========================================
  # app_checklist <- list(
  #   xlsx_tabs  = c("user_inputs", "time_periods", "AD_lu_transitions", "c_stocks"),
  #   cat_cunits = c("DM", "C"),
  #   cat_cpools = c("AGB", "BGB", "DW", "LI", "SOC", "ALL"),
  #   cat_racti  = c("DF", "DG", "EN", "EN_AF", "EN_RE"),
  #   cat_ptype  = c("REF", "REF[0-9]", "MON", "MON[0-9]"),
  #   cat_pdf    = c("normal", "beta"),
  #   col_usr    = c("trunc_pdf", "n_iter", "ran_seed", "c_unit", "c_fraction", "c_fraction_se",  "c_fraction_pdf", "dg_ext", "dg_pool", "ad_annual", "conf_level"),
  #   col_time   = c("period_no", "year_start", "year_end", "period_type"),
  #   col_ad     = c("trans_no",	"trans_id",	"trans_period",	"redd_activity", "lu_initial_id", "lu_initial",	"lu_final_id", "lu_final", "trans_area", "trans_se", "trans_pdf", "trans_pdf_a", "trans_pdf_b", "trans_pdf_c"),
  #   col_cs     = c("c_no",	"c_id",	"c_period", "lu_id", "lu_name",	"c_pool",	"c_value",	"c_se",	"c_pdf",	"c_pdf_a",	"c_pdf_b",	"c_pdf_c")
  # )
  #
  # app_checklist$cat_cpools_all <- c(app_checklist$cat_cpools, "RS", "DG_ratio", "C_all")

  ## + Initiate translation ====================================================

  ## !!! In a package the translation folder needs to be directed to the package location
  i18n <- shiny.i18n::Translator$new(
    translation_json_path = system.file("assets/translations.json", package = "mocaredd")
    )
  i18n$set_translation_language('en')


  ## + UI Elements =============================================================

  ## App title with logo
  app_title <- div(
    tags$a(
      href = "./",
      alt = "arena-helpers",
      tags$img(src="assets/arena-helpers3.png", height = '60px'),
      .noWS = "before-end"
    ),
    i18n$t("Monte Carlo Simulations for REDD+"),
    style = "display:inline;font-color: black !important"
  )

  ## Dropdown list for language selection
  language_selector <- shinyWidgets::pickerInput(
    inputId = "language",
    label = NULL,
    choices = c("en", "fr", "sp"),
    choicesOpt =  list(content = c('<i class="fi fi-gb"></i> EN', '<i class="fi fi-fr"></i> FR', '<i class="fi fi-es"></i> ES')),
    selected = "en",
    width = "auto",
    option = shinyWidgets::pickerOptions(style = "z-index:10000;")
  )



  ##
  ## UI ########################################################################
  ##

  ui <- shiny::tagList(

    ## + Setup =================================================================

    shiny::withMathJax(),
    shinyjs::useShinyjs(),
    shinyWidgets::useSweetAlert(),
    shiny.i18n::usei18n(i18n),
    tags$head(tags$link(rel = "stylesheet", type = "text/css", href = "assets/style.css")),
    htmltools::htmlDependency(
      name = "flag-icons",
      version = "6.6.6",
      src = c(href="https://cdn.jsdelivr.net/gh/lipis/flag-icons@6.6.6/"),
      stylesheet = "css/flag-icons.min.css"
    ),
    # tags$head(includeHTML("ga-tracker-draft-head.html")),
    # leafletjs,

    ## + Layout UI elements ====================================================

    bslib::page_navbar(
      id = "navbar",
      title = app_title,
      window_title = "{mocaredd}",
      theme = bs_theme(
        version = 5,
        bootswatch = "yeti",
        base_font = font_collection(
          "-apple-system", "BlinkMacSystemFont", "Segoe UI", "Roboto", "Helvetica Neue",
          "Arial", "Noto Sans", "sans-serif", "Apple Color Emoji", "Segoe UI Emoji",
          "Segoe UI Symbol","Noto Color Emoji"
          ),
        code_font = font_google("Fira Code"),
        heading_font = font_google("Lato"),
        primary = "#4991B0",
        secondary = "#77AB16",
      ),
      navbar_options = navbar_options(bg = "#f8f9fa"),
      fillable = FALSE, ## Not needed for now, make a tab fill the whole browser, cool for leaflets
      # inverse = FALSE, ## Not working well with yeti, overridden in assets/styles.css

      ## + Panels ####
      nav_spacer(), ## align menu to the right

      nav_panel(
        title = i18n$t("Home"),
        value = "home",
        #icon = icon("campground"),
        mod_home_UI("tab_home", i18n = i18n)
      ),

      nav_panel(
        title = i18n$t("Tool"),
        value = "tool",
        #icon = icon("mug-hot"),
        mod_tool_UI("tab_tool", i18n = i18n)
      ),

      nav_panel(
        title = i18n$t("About"),
        value = "about",
       #icon = icon("info"),
        mod_about_UI("tab_about", i18n = i18n)
      ),

      nav_item(language_selector)

    ) |> ## End page_navbar
      ## Make navbar larger before switch to menu button
      shiny::tagAppendAttributes(.cssSelector = "nav", class = "navbar-expand-md")
  ) ## End tagList


  ##
  ## Server ####################################################################
  ##

  server <- function(input, output, session) {

    ## + Initiate reactive values list to be passed between modules ####
    ## See https://rtask.thinkr.fr/communication-between-modules-and-its-whims/
    rv <- reactiveValues(
      #checklist = app_checklist,
      inputs    = reactiveValues(),
      checks    = reactiveValues(),
      sims      = reactiveValues(),
      res       = reactiveValues(),
      gt        = reactiveValues(),
      histo     = reactiveValues(),
      actions   = reactiveValues()
    )

    ## Save language value to show/hide divs with shinyjs
    r_lang <- reactive({ input$language })

    ## + Module server functions ####
    mod_home_server("tab_home", rv = rv)

    mod_tool_server("tab_tool", rv = rv)

    mod_about_server("tab_about", rv = rv)

    ## + Trans modules events ####
    observeEvent(input$language, {
      shiny.i18n::update_lang(language = input$language)
    })

    # observeEvent(rv$actions$to_home, {
    #   updateTabsetPanel(session, "navbar", "home")
    # })

    observeEvent(rv$actions$to_tool, {
      nav_select(id = "navbar", selected = "tool")
    })

    observeEvent(rv$actions$to_about, {
      nav_select(id = "navbar", selected = "about")
    })


  } ## END server

  ## App call ###############################################################
  shinyApp(ui, server, ...)

} ## END function
