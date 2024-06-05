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
#'
#'
#' @examples
#' if (interactive()) {
#'
#' shiny_run_MCredd()
#'
#' }
#'
#' @export
shiny_run_MCredd <- function(...) {

  ## GLOBAL ####################################################################
  ## Initiate translation
  ## !!! TO BE REMOVED IN PACKAGE !!!
  ## !!! In a package the translation folder needs to be directed to the package location
  #i18n <- shiny.i18n::Translator$new(translation_json_path = 'assets/translations.json')
  i18n <- shiny.i18n::Translator$new(
    translation_json_path = system.file("assets/translations.json", package = "MCredd")
    )
  # i18n <- shiny.i18n::Translator$new(translation_csvs_path = "assets/translation")
  i18n$set_translation_language('en')
  ## !!! END REMOVE

  language_selector2 <- shinyWidgets::pickerInput(
    inputId = "language",
    label = NULL,
    choices = c("en", "fr", "sp"),
    choicesOpt =  list(content = c('<i class="fi fi-gb"></i> EN', '<i class="fi fi-fr"></i> FR', '<i class="fi fi-es"></i> ES')),
    selected = "en",
    width = "auto",
    option = shinyWidgets::pickerOptions(style = "z-index:10000;")
  )

  ## UI ########################################################################
  ui <- shiny::tagList(

    ## Setup -------------------------------------------------------------------
    shiny::withMathJax(),
    shinyjs::useShinyjs(),
    shinyWidgets::useSweetAlert(),
    shiny.i18n::usei18n(i18n),
    # tags$head(tags$link(rel = "stylesheet", type = "text/css", href = "style.css")),
    htmltools::htmlDependency(
      name = "flag-icons",
      version = "6.6.6",
      src = c(href="https://cdn.jsdelivr.net/gh/lipis/flag-icons@6.6.6/"),
      stylesheet = "css/flag-icons.min.css"
    ),
    # tags$head(includeHTML("ga-tracker-draft-head.html")),
    # leafletjs,
    ## UI elements -------------------------------------------------------------
    page_navbar(
      id = "navbar",
      ## ++ Styling ++++++
      #title = div(img(src="assets/Arena-Logo.png", width = '100%'), i18n$t("Timor Leste REDD+ Geoportal"), style = "display:inline;"),
      title = div(
        tags$a(
          href = "https://openforis.org/solutions/arena/",
          #alt = "arena-helpers",
          tags$img(src="assets/arena-helpers3.png", height = '60px'),
          .noWS = "before-end"
          ),
        #img(src="assets/arena-helpers3.png", height = '60px'),
        i18n$t("Monte Carlo for REDD+"),
        style = "display:inline;font-color: black !important"
        ),
      window_title = "Monte Carlo for REDD+",
      theme = bs_theme(
        version = 5,
        bootswatch = "minty",
        # base_font = font_google("Noto Sans", wght = c(400, 700)),
        base_font = font_collection(
          "-apple-system", "BlinkMacSystemFont", "Segoe UI", "Roboto", "Helvetica Neue",
          "Arial", "Noto Sans", "sans-serif", "Apple Color Emoji", "Segoe UI Emoji",
          "Segoe UI Symbol","Noto Color Emoji"
          ),
        code_font = font_google("Fira Code"),
        heading_font = font_google("Lato", wght = 700),
        primary = "#4991B0",
        secondary = "#77AB16",
      ),
      fillable = "portal",
      bg = "#f8f9fa",
      inverse = FALSE,

      ## ++ Panels +++++
      nav_panel(
        title = i18n$t("Info"), #OR title = "I am module 1"
        value = "mod1",
        icon = icon("info"),
        mod_info_UI("tab_info_UI") ## See R/mod1_UI.R
      ),

      nav_panel(
        title = i18n$t("I am module 1"), #OR title = "I am module 1"
        value = "mod1",
        icon = icon("campground"),
        mod1_UI("tab_mod1_UI") ## See R/mod1_UI.R
      ),

      nav_panel(
        title = i18n$t("I am module 2"), #OR title = "I am module 2"
        value = "mod2",
        icon = icon("chart-line"),
        mod2_UI("tab_mod2_UI") ## See R/mod2_UI.R
      ),

      nav_spacer(),

      nav_item(language_selector2)

    ) |> ## End page_navbar
      shiny::tagAppendAttributes(.cssSelector = "nav", class = "navbar-expand-lg")
  ) ## End tagList


  ## Server #################################################################
  server <- function(input, output, session) {

    ## + Initiate reactive values list to be passed between modules =========
    ## See https://rtask.thinkr.fr/communication-between-modules-and-its-whims/
    rv <- reactiveValues(
      cv_model = reactiveValues(),
      time     = reactiveValues(),
      opti     = reactiveValues(),
      results  = reactiveValues()
    )

    r_lang <- reactive({ input$language })

    ## + Module server functions ============================================
    # mod_home_server("tab_home", rv = rv)
    #
    # mod_CV_server("tab_cv", rv = rv)
    #
    # mod_time_server("tab_time", rv = rv)
    #
    # mod_opti_server("tab_opti", rv = rv)
    #
    # mod_results_server("tab_res", rv = rv)


    ## + Observers ==========================================================
    observeEvent(input$language, {
      shiny.i18n::update_lang(language = input$language)
    })

    ## + Trans modules events ===============================================
    # observeEvent(rv$to_cv, {
    #   updateTabsetPanel(session, "navbar", "cv_model")
    # })
    #
    # observeEvent(rv$to_time, {
    #   updateTabsetPanel(session, "navbar", "time")
    # })
    #
    # observeEvent(rv$to_opti, {
    #   updateTabsetPanel(session, "navbar", "opti")
    # })
    #
    # observeEvent(rv$to_results, {
    #   updateTabsetPanel(session, "navbar", "results")
    # })

  } ## END server

  ## App call ###############################################################
  shinyApp(ui, server, ...)

} ## END function
