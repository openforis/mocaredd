



mod_tool_UI <- function(id, i18n){

  ## From https://shiny.rstudio.com/articles/modules.html
  # `NS(id)` returns a namespace function, which was save as `ns` and will
  # invoke later.
  ns <- NS(id)

  ##
  ## UI Elements ###############################################################
  ##

  card1 <- card(
    card_header(
      "Open Foris Arena ", tags$img(src="assets/Arena-Logo.png", height = '30px')
    ),
    p("More on OF Arena"),
    tags$a(
      href = "https://openforis.org/solutions/arena/",
      alt = "(logo)",
      "More on OF Arena",
      bsicons::bs_icon("box-arrow-up-right", class = "text-primary"),
      .noWS = "before-end"
    )
  )

  card2 <- card(
    card_header(tags$code("arena-helpers")),
    p("More on arena-helpers"),
    p("Add link to webpage")
  )

  card3 <- card(
    card_header("{mocaredd}"),
    p("More info on mocaredd"),
    p("link to info tab")
  )



  ##
  ## Layout UI elements with tagList() function ######
  ##

  tagList(

    h2(i18n$t("Run the uncertainty analysis")),

    br(),

    navset_card_tab(

      sidebar = sidebar(
        h4(i18n$t("Upload your data")),
        br(),
        shiny::fileInput(
          inputId = "load_xlsx",
          accept = ".xlsx",
          label = i18n$t("Upload your data here")
        ),
        hr(),
        "Download a template file",
        "Downlaod button",
        hr(),
        "Data check 1",
        "Data check 2",
        "..."
      ),

      nav_panel(
        title = i18n$t("Check your data"),
        value = "about",
        icon = icon("circle-check"),
        submod_upload_UI("tab_upload_UI", i18n = i18n)
      ),

      nav_panel(
        title = i18n$t("Results"),
        value = "res",
        icon = icon("chart-simple"),
        submod_res_UI("tab_res_UI", i18n = i18n)
      ),

      nav_panel(
        title = i18n$t("Sensitivity"),
        value = "sensitivity",
        icon = icon("magnifying-glass"),
        mod_about_UI("tab_sensitivity_UI", i18n = i18n)
      )
    )

  ) ## END tagList

} ## END module UI function
