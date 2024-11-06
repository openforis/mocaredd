
mod_about_UI <- function(id, i18n){

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

    h2(i18n$t("About {mocaredd}")),

    br(),

    "Workflow, package list, Link to Arena"

  ) ## END tagList

} ## END module UI function
