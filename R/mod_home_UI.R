#' Home module UI function
#'
#' @noRd
mod_home_UI <- function(id, i18n){

  ## From https://shiny.rstudio.com/articles/modules.html
  # `NS(id)` returns a namespace function, which was save as `ns` and will
  # invoke later.
  ns <- NS(id)


  ##
  ## UI Elements ######
  ##

  card1 <- card(
    card_header("Open Foris Arena"),
    p("text"),
    tags$a(
      href = "https://openforis.org/solutions/arena/",
      alt = "",
      "More on OF Arena",
      bsicons::bs_icon("box-arrow-up-right", class = "text-primary"),
      .noWS = "before-end"
    )
  )

  card2 <- card(
    card_header(tags$code("arena-helpers")),
    p("text"),
    tags$a(
      href = "https://openforis.github.io/arena-helpers/",
      alt = "",
      "More on OF Arena",
      bsicons::bs_icon("box-arrow-up-right", class = "text-primary"),
      .noWS = "before-end"
    )
  )

  card3 <- card(
    card_header("{mocaredd}"),
    p("text"),
    p(
      "More on {mocaredd} in:", HTML("&nbsp;"),
      actionButton(inputId = ns("to_about"), label = "About")
      )
  )

  cards <- list(card1, card2, card3)



  ##
  ## UI elements wrapped in a tagList() function ######
  ##

  tagList(

    h2(i18n$t("Welcome to {mocaredd}")),

    br(),

    p(
      "
      {mocaredd} is a R package and a Shiny application designed to help you with
      running Monte Carlo Simulations for REDD+ uncertainty analysis.
      "
      ),
    p(
      "
      {mocaredd} is part of arena-helpers: a collection of tools to assist
      "
      ),
    p(
    "
    Openforis Arena is a ...
    "
    ),

    layout_columns(
      col_widths = c(4, 4, 4),
      !!!cards
    ),

    br(),

    h4(
      icon("arrow-right"), "Continue to the:", HTML("&nbsp;"),
      actionButton(inputId = ns("to_tool"), label = "Tool")
    )

  ) ## END tagList

} ## END module UI function
