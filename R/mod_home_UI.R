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

  # card1 <- card(
  #   card_header("Open Foris Arena"),
  #   p("text"),
  #   tags$a(
  #     href = "https://openforis.org/solutions/arena/",
  #     alt = "",
  #     "More on OF Arena",
  #     bsicons::bs_icon("box-arrow-up-right", class = "text-primary"),
  #     .noWS = "before-end"
  #   )
  # )
  #
  # card2 <- card(
  #   card_header(tags$code("arena-helpers")),
  #   p("text"),
  #   tags$a(
  #     href = "https://openforis.github.io/arena-helpers/",
  #     alt = "",
  #     "More on OF Arena",
  #     bsicons::bs_icon("box-arrow-up-right", class = "text-primary"),
  #     .noWS = "before-end"
  #   )
  # )
  #
  # card3 <- card(
  #   card_header("{mocaredd}"),
  #   p("text"),
  #   p(
  #     "More on {mocaredd} in:", HTML("&nbsp;"),
  #     actionButton(inputId = ns("to_about"), label = "About")
  #     )
  # )
  #
  # cards <- list(card1, card2, card3)



  ##
  ## UI elements wrapped in a tagList() function ######
  ##

  tagList(

    h2(i18n$t("Welcome to {mocaredd}!")),

    br(), br(),

    div(
      "
      {mocaredd} is a R package and a Shiny application designed to help you with
      running Monte Carlo Simulations for REDD+ uncertainty analysis.
      ", style = "font-size: x-large; text-align: center; font-style: italic;font-family: serif;"
      ),
    br(), br(),
    p(
      "
      This app is developed in the context of the REDD+ mechanism (Reducing emission from Deforestation
      and forest Degradation 'plus') and carbon accounting calculations to estimate emission reductions
      and removal increases of greenhouse gas (GHG) in the forestry sector.
      "
      ),
    # p(
    #   "
    #   Emission reductions are estimated by quantifying the amount of greenhouse gas emissions between a
    #   reference level (i.e baseline) and a monitorig period. These estimates come with a level of uncertainty
    #   due to the sampling methods and models used to generate the estimations.
    #   "
    # ),
    # p(
    #   "
    #   High integrity REDD+ standards, that set rules for the quantification of emissions and removals, and their uncertainties,
    #   increasingly require the use of Monte Carlo simulations for REDD+ uncertainties. These simulations are often run in spreadsheet
    #   tools, but the lack of structure and inherent disadvantage of spreadsheets for this task (error prone, difficulty to handle large
    #   amount of data) is becoming a barrier for the quality, reproducibility and verification of uncertainties based on simulations.
    #   "
    # ),
    # br(),
    p(
      "
      {mocaredd} provides a template for organizing data, and a tool that (1) takes the data, (2) runs Monte Carlo Simulations and (3) produces improved
      estimates of and confidence intervals around greenhouse gas emissions and emission reductions for REDD+.
      "
    ),

    # layout_columns(
    #   col_widths = c(4, 4, 4),
    #   !!!cards
    # ),

    br(),

    h4(
      icon("arrow-right"), "To start exploring, continue to the:", HTML("&nbsp;"),
      actionButton(inputId = ns("to_tool"), label = "Tool")
    ),

    br(),

    div(
    "{mocaredd}  v1.0. Development supported by FAO and the Aim4Forest Programme",
    style = "font-size: large; text-align: right; font-style: italic;font-family: serif;"
    )

  ) ## END tagList

} ## END module UI function
