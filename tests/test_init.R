library(shiny)

## Initiate translation
i18n <- shiny.i18n::Translator$new(translation_csvs_path = 'translation')
i18n$set_translation_language('en')

language_selector2 <- shinyWidgets::pickerInput(
  inputId = "language",
  label = NULL,
  choices = c("en", "te"),
  choicesOpt =  list(content = c('<i class="fi fi-gb"></i> EN', '<i class="fi fi-tl"></i> TL')),
  selected = "en",
  width = "auto",
  option = shinyWidgets::pickerOptions(style = "z-index:10000;")
)

## UI #####################################################################
ui <- shiny::tagList(

  ## Setup ---------------------------------------------------------------------
  # shiny::withMathJax(),
  # shinyjs::useShinyjs(),
  # shinyWidgets::useSweetAlert(),
  shiny.i18n::usei18n(i18n),
  tags$head(tags$link(rel = "stylesheet", type = "text/css", href = "style.css")),
  htmltools::htmlDependency(
    name = "flag-icons",
    version = "6.6.6",
    src = c(href="https://cdn.jsdelivr.net/gh/lipis/flag-icons@6.6.6/"),
    stylesheet = "css/flag-icons.min.css"
  ),
  # # tags$body(includeHTML("piwik-tracker.html")),
  # # tags$head(includeHTML("piwik-tracker-draft-sync.html")),
  # # tags$body(includeHTML("piwik-tracker-draft.html")),
  # tags$head(includeHTML("ga-tracker-draft-head.html")),
  # tags$body(includeHTML("ga-tracker-draft-body.html")),
  # leafletjs,
  ## UI elements ---------------------------------------------------------------
  page_navbar(
    id = "navbar",
    ## ++ Styling ++++++
    title = div(img(src="assets/Arena-logo.png", width = '100%'), i18n$t("Timor Leste REDD+ Geoportal"), style = "display:inline;"),
    window_title = "TL REDD+ Geoportal",
    theme = bs_theme(
      version = 5,
      bootswatch = "yeti",
      base_font = font_google("Noto Sans", wght = c(400, 700)),
      code_font = font_google("Fira Code"),
      heading_font = font_google("Lato", wght = 700),
      primary = rgb(68,141,182),
      secondary =  rgba(119,171,22)
    ),
    fillable = "portal",
    bg = "#f8f9fa",

    ## ++ Panels +++++
    nav_panel(
      title = i18n$t("Home"),
      value = "home",
      icon = icon("campground"),
      #mod_home_UI("tab_home") ## See R/mod_home_UI.R
    ),

    nav_spacer(),

    nav_item(language_selector2)

  ) |> ## End page_navbar
    tagAppendAttributes(.cssSelector = "nav", class = "navbar-expand-lg")
) ## End tagList


## Server #################################################################
server <- function(input, output, session) {

  ## + Initiate reactive values list to be passed between modules =========
  ## See https://rtask.thinkr.fr/communication-between-modules-and-its-whims/
  # rv <- reactiveValues(
  #   cv_model = reactiveValues(),
  #   time     = reactiveValues(),
  #   opti     = reactiveValues(),
  #   results  = reactiveValues()
  # )



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



  ## + Trans modules events ===============================================
  observeEvent(rv$to_cv, {
    updateTabsetPanel(session, "navbar", "cv_model")
  })

  observeEvent(rv$to_time, {
    updateTabsetPanel(session, "navbar", "time")
  })

  observeEvent(rv$to_opti, {
    updateTabsetPanel(session, "navbar", "opti")
  })

  observeEvent(rv$to_results, {
    updateTabsetPanel(session, "navbar", "results")
  })

}

## App call ###############################################################
shinyApp(ui, server, ...)
