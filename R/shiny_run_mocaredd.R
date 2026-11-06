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

  ## + Initiate translation ====================================================

  ## !!! In a package the translation folder needs to be directed to the package location
  i18n <- shiny.i18n::Translator$new(
    translation_json_path = system.file("assets/translations.json", package = "mocaredd")
    )
  i18n$set_translation_language('en')

  ## Simplify text keys
  ## All text is stored in R/utils-tr.R and called via keys with .tr_keys()
  ## Simplified here to: .tr
  .tr <- .tr_keys()

  ## Version stamp for inst/assets files (latest modification time), added to their URLs
  ## so browsers reload CSS and JS after a change instead of using a cached copy
  asset_v <- list.files(system.file("assets", package = "mocaredd"), recursive = TRUE, full.names = TRUE) |>
    file.mtime() |>
    max() |>
    as.integer()


  ## + FAO header and footer elements =========================================

  ## Languages: shiny.i18n code -> ISO code used by <html lang> and fao.org links
  lang_iso <- c(en = "en", fr = "fr", sp = "es")

  ## Name of the Global Goals identity (alt text) in each language
  gg_name <- c(
    en = "FAO and the Sustainable Development Goals",
    fr = "La FAO et les objectifs de d\u00e9veloppement durable",
    es = "La FAO y los Objetivos de Desarrollo Sostenible"
  )

  ## !!! FAO BRANDING ON HOLD, pending authorisation: the FAO logos below are
  ## commented out and replaced by empty placeholders of the same size
  ## (inst/assets/branding/placeholder_logo_*.svg). To revert, uncomment the two
  ## original blocks and delete the two placeholder blocks.
  ## !!!

  # ## FAO logo, official 3-line English files for all languages (inst/assets/branding):
  # ## black on light, white on dark (CSS). Used unmodified, height only.
  # logo_imgs <- purrr::imap(c(light = "Black", dark = "White"), \(colour, mode) {
  #   tags$img(
  #     class = paste0("header-logo header-logo-", mode),
  #     src   = sprintf("assets/branding/FAO_Logo_English_%s_three_lines_25mm.svg", colour),
  #     alt   = "Food and Agriculture Organization of the United Nations"
  #   )
  # })

  # ## FAO Global Goals visual identity with the four betters (footer), official files,
  # ## same in all languages: black on light, white on dark (CSS). Used unmodified.
  # gg_imgs <- purrr::imap(c(light = "black", dark = "white"), \(colour, mode) {
  #   tags$img(
  #     class = paste0("gg-logo gg-logo-", mode),
  #     src   = sprintf("assets/branding/FAO_GG_icons_H_%s.svg", colour),
  #     alt   = gg_name[["en"]],
  #     !!!as.list(purrr::set_names(unname(gg_name[lang_iso]), paste0("data-alt-", lang_iso)))
  #   )
  # })

  ## Placeholder for the FAO logo: empty, same size, light and dark (CSS)
  logo_imgs <- purrr::map(c("light", "dark"), \(mode) {
    tags$img(
      class = paste0("header-logo header-logo-", mode),
      src   = sprintf("assets/branding/placeholder_logo_header_%s.svg", mode),
      alt   = ""
    )
  })

  ## Placeholder for the FAO Global Goals identity: empty, same size, light and dark (CSS)
  gg_imgs <- purrr::map(c("light", "dark"), \(mode) {
    tags$img(
      class = paste0("gg-logo gg-logo-", mode),
      src   = sprintf("assets/branding/placeholder_logo_footer_%s.svg", mode),
      alt   = ""
    )
  })

  ## FAO footer links, path per language (data-href-<lang>)
  footer_links <- dplyr::tribble(
    ~key,                    ~path,
    .tr$footer_contact,      "https://www.fao.org/contact-us/%s/",
    .tr$footer_terms,        "https://www.fao.org/contact-us/terms/%s/",
    .tr$footer_privacy,      "https://www.fao.org/contact-us/data-protection-and-privacy/%s/",
    .tr$footer_scam,         "https://www.fao.org/contact-us/scam-alert/%s/",
    .tr$footer_misconduct,   "https://www.fao.org/audit-and-investigations/reporting-misconduct/%s/",
    .tr$footer_transparency, "https://www.fao.org/transparency/%s"
  ) |>
    dplyr::mutate(href = purrr::map(.data$path, \(p) purrr::set_names(sprintf(p, lang_iso), paste0("data-href-", lang_iso))))


  ## + UI Elements =============================================================

  ## App title: FAO logo top left (48 px logo, 12 px clear space above and below), not a
  ## link (the FAO logo is not the app logo), then the app title on two lines ({mocaredd},
  ## then the description), a link to the home page without link styling. The description is
  ## hidden below 992 px, the name only on very small screens (assets/style.css)
  ## (as function because i18n$t() needs to be inside page_navbar())
  app_title <- function() {
    div(
      class = "app-brand",
      tags$span(class = "header-logo-block", logo_imgs),
      tags$a(
        href = "./", ## Send back to home page
        class = "app-title",
        tags$span(class = "app-name", "{mocaredd}"),
        tags$span(class = "app-subtitle d-none d-lg-inline", i18n$t(.tr$app_title)) ## hidden below 992 px
      )
    )
  }

  app_window_title <- "mocaredd"

  ## App colors, light and dark themes
  ## - primary: FAO Design System blue (links lighter in dark for contrast)
  ## - secondary, success, tint: Open Foris greens (openforis.org)
  ## - info: FAO Design System Caption blue (= Open Foris navy)
  ## - warning, danger: FAO orange and red, FAO-approved, identical in both themes
  ## - link_hover: FAO Design System greys, Gray Dark on light, Gray Medium on dark
  ## - btn_primary, btn_primary_hover: FAO Design System primary button (v3.7.2), white text
  ## - card_header: FAO Design System Primary Light (v3.7.2)
  app_colors <- list(
    light = list(
      primary = "#116bac",
      link = "#116bac",
      secondary = "#81af44",
      secondary_tint = "#edf2e7",
      info = "#1c4767",
      success = "#81af44",
      warning = "#f58320",
      danger = "#980000",
      link_hover = "#545454",
      btn_primary = "#116bac",
      btn_primary_hover = "#1688da",
      card_header = "#e5ecf4"
    ),
    dark = list(
      primary = "#116bac",
      link = "#4e9df8",
      secondary = "#81af44",
      secondary_tint = "#323e2e",
      info = "#4e9df8",
      success = "#81af44",
      warning = "#f58320",
      danger = "#980000",
      link_hover = "#999999",
      btn_primary = "#1d82f6",
      btn_primary_hover = "#4e9df8",
      card_header = "#212529"
    )
  )

  ## App theme
  ## Fonts: Open Sans for text (FAO Design System), Fira Code for code;
  ## both self-hosted in inst/assets/fonts (data-raw/vendor_fonts.R)
  app_theme <- bslib::bs_theme(
    version = 5,
    bootswatch = "yeti",
    base_font = bslib::font_collection("Open Sans", "Helvetica", "Arial", "sans-serif"),
    code_font = bslib::font_collection(
      "Fira Code", "ui-monospace", "SFMono-Regular", "Menlo", "Consolas", "monospace"
    ),
    heading_font = bslib::font_collection("Open Sans", "Helvetica", "Arial", "sans-serif"),
    primary   = app_colors$light$primary,
    secondary = app_colors$light$secondary,
    success   = app_colors$light$success,
    info      = app_colors$light$info,
    warning   = app_colors$light$warning,
    danger    = app_colors$light$danger,
    "link-color-dark" = app_colors$dark$link
  )

  ## Dropdown list for language selection
  language_selector <- div(
    shinyWidgets::pickerInput(
      inputId = "language",
      label = NULL,
      choices = c("en", "fr", "sp"),
      choicesOpt =  list(content = c('<i class="fi fi-gb"></i> EN', '<i class="fi fi-fr"></i> FR', '<i class="fi fi-es"></i> ES')),
      selected = "en",
      width = "auto"
    )
  )

  ## !!! FAO BRANDING ON HOLD, pending authorisation: the FAO footer below is
  ## commented out and replaced by a placeholder footer with the same FAO texts,
  ## invisible (.footer-placeholder in assets/style.css) and without links.
  ## To revert, uncomment the original app_footer() and delete the placeholder one.
  ## !!!

  # ## Footer: FAO Design System application footer, FAO Global Goals visual identity
  # ## bottom left, six FAO links in the active language, (c) FAO, then the app credit
  # ## (version from DESCRIPTION)
  # ## (as function because i18n$t() needs to be inside page_navbar())
  # app_footer <- function() {
  #   tags$footer(
  #     class = "footer footer-app",
  #     div(
  #       class = "container-fluid",
  #       div(
  #         class = "row align-items-center",
  #         div(class = "footer-logo col-lg-auto", gg_imgs),
  #         div(
  #           class = "footer-links col-lg",
  #           tags$ul(
  #             class = "list-inline mb-2",
  #             purrr::map2(footer_links$key, footer_links$href, \(key, href) {
  #               tags$li(
  #                 class = "list-inline-item",
  #                 tags$a(
  #                   href = href[[1]],
  #                   target = "_blank",
  #                   !!!as.list(href),
  #                   i18n$t(key)
  #                 )
  #               )
  #             })
  #           ),
  #           tags$small(
  #             paste0("{mocaredd} v", utils::packageVersion("mocaredd"), " - "),
  #             i18n$t(.tr$footer_credit),
  #             " ",
  #             tags$a(
  #               "AIM4Forests",
  #               href = "https://www.fao.org/in-action/aim4forests/en",
  #               target = "_blank"
  #             )
  #           )
  #         ),
  #         div(
  #           class = "col-lg-auto text-lg-end",
  #           paste0("\u00a9 FAO\u00a0", format(Sys.Date(), "%Y"))
  #         )
  #       )
  #     )
  #   )
  # }

  ## Placeholder footer: same layout and texts as the FAO footer, FAO texts invisible
  ## and not links; only the app name and version stay visible
  app_footer <- function() {
    tags$footer(
      class = "footer footer-app",
      div(
        class = "container-fluid",
        div(
          class = "row align-items-center",
          div(class = "footer-logo col-lg-auto", gg_imgs),
          div(
            class = "footer-links col-lg",
            tags$ul(
              class = "list-inline mb-2 footer-placeholder",
              purrr::map(footer_links$key, \(key) {
                tags$li(class = "list-inline-item", i18n$t(key))
              })
            ),
            tags$small(
              paste0("{mocaredd} v", utils::packageVersion("mocaredd")),
              tags$span(
                class = "footer-placeholder",
                " - ", i18n$t(.tr$footer_credit), " AIM4Forests"
              )
            )
          ),
          div(
            class = "col-lg-auto text-lg-end footer-placeholder",
            paste0("\u00a9 FAO\u00a0", format(Sys.Date(), "%Y"))
          )
        )
      )
    )
  }


  ##
  ## UI ########################################################################
  ##

  ui <- shiny::tagList(

    ## + Setup =================================================================

    ## HEAD scripts
    shiny::withMathJax(),
    shinyjs::useShinyjs(),
    shinyWidgets::useSweetAlert(),
    shiny.i18n::usei18n(i18n),
    tags$head(
      ## JS custom handler to updateTabsetPanel()
      tags$script(src = paste0("assets/js_activate_tab.js?v=", asset_v)),
      tags$script(src = paste0("assets/js_handlers.js?v=", asset_v)),
      ## FAO header and footer: alt texts and links follow the language
      tags$script(src = paste0("assets/js_brand.js?v=", asset_v)),
      ## CSS Style: self-hosted fonts, then app styles
      tags$link(rel = "stylesheet", type = "text/css", href = paste0("assets/fonts/fonts.css?v=", asset_v)),
      ## Language selector flags, self-hosted (data-raw/vendor_flags.R)
      tags$link(rel = "stylesheet", type = "text/css", href = paste0("assets/flags/flags.css?v=", asset_v)),
      tags$link(rel = "stylesheet", type = "text/css", href = paste0("assets/style.css?v=", asset_v)),
      ## App colors as CSS custom properties for style.css (e.g. secondary_tint -> --app-secondary-tint)
      tags$style(HTML(
        purrr::imap_chr(c(":root" = "light", "[data-bs-theme=dark]" = "dark"), \(mode, selector) {
          paste0(
            selector, "{",
            paste0("--app-", stringr::str_replace_all(names(app_colors[[mode]]), "_", "-"), ":", unlist(app_colors[[mode]]), ";", collapse = ""),
            "}"
          )
        }) |>
          paste(collapse = "")
      )),
      ## !!! FAO BRANDING ON HOLD, pending authorisation: the FAO favicon is commented
      ## out (here and in assets/js_brand.js) and replaced by an empty icon. To revert,
      ## uncomment both and delete the placeholder link.
      ## !!!
      # ## Favicon: FAO emblem (design-system.fao.org); assets/js_brand.js switches to
      # ## the white version when the browser uses a dark theme
      # tags$link(id = "favicon", rel = "icon", type = "image/x-icon", href = "assets/branding/favicon.ico"),
      ## Placeholder favicon: empty, also stops the browser requesting /favicon.ico
      tags$link(rel = "icon", href = "data:,"),
      tags$meta(name = "apple-mobile-web-app-title", content = "mocaredd")
    ),

    ## + Layout UI elements ====================================================

    bslib::page_navbar(

      ## Navbar setup ------
      id = "navbar",
      title = app_title(),
      window_title = app_window_title,
      theme = app_theme,
      navbar_options = navbar_options(
        ## Background set in assets/style.css so it follows the light/dark theme
        position = "fixed-top",
        class = "navbar-expand-xl" ## menu button below 1200 px: the FAO logo and the two-line title need the width
      ),
      fillable = FALSE, ## Not needed for now, make a tab fill the whole browser, cool for leaflets
      footer = app_footer(),

      ## Panels ------
      nav_spacer(), ## align menu to the right

      nav_panel(
        title = i18n$t(.tr$nav_home),
        value = "home",
        mod_home_UI2("tab_home", i18n = i18n, .tr = .tr)
      ),

      nav_panel(
        title = i18n$t(.tr$nav_tool),
        value = "tool",
        mod_tool_UI2("tab_tool", i18n = i18n, .tr = .tr)
      ),

      nav_panel(
        title = i18n$t(.tr$nav_about),
        value = "about",
        mod_about_UI("tab_about", i18n = i18n, .tr = .tr)
      ),

      ## Light/dark mode, then language
      nav_item(bslib::input_dark_mode(id = "dark_mode", mode = "light")),

      nav_item(language_selector)

    ) ## End page_navbar

  ) ## End tagList


  ##
  ## Server ####################################################################
  ##

  server <- function(input, output, session) {

    ## + Initiate reactive values list to be passed between modules ####
    ## See https://rtask.thinkr.fr/communication-between-modules-and-its-whims/
    rv <- reactiveValues(
      checks    = reactiveValues(),
      inputs    = reactiveValues(),
      sims      = reactiveValues(),
      res       = reactiveValues(),
      gt        = reactiveValues(),
      histo     = reactiveValues(),
      actions   = reactiveValues(),
      ui        = reactiveValues(dark_mode = "light")
    )

    ## Light or dark mode from the navbar toggle, for outputs drawn in R
    observeEvent(input$dark_mode, {
      rv$ui$dark_mode <- input$dark_mode
    })

    ## + Module server functions ####
    mod_home_server2("tab_home", rv = rv)

    mod_tool_server2("tab_tool", rv = rv, i18n = i18n, .tr = .tr)

    mod_about_server("tab_about", rv = rv)

    ## + Trans modules events ####
    observeEvent(input$language, {
      shiny.i18n::update_lang(language = input$language)
      ## Footer links, alt texts and <html lang> follow the language
      session$sendCustomMessage("app-locale", list(lang = lang_iso[[input$language]]))
    })

    observeEvent(rv$actions$to_tool, {
      nav_select(id = "navbar", selected = "tool")
      session$sendCustomMessage("scroll_top", list()) ## Go to top of the page
    })

    observeEvent(rv$actions$to_about, {
      nav_select(id = "navbar", selected = "about")
      session$sendCustomMessage("scroll_top", list())
    })


  } ## END server

  ## App call ###############################################################
  shinyApp(ui, server, ...)

} ## END function
