


mod_home_UI <- function(id){

  ## From https://shiny.rstudio.com/articles/modules.html
  # `NS(id)` returns a namespace function, which was save as `ns` and will
  # invoke later.
  ns <- NS(id)

  ##
  ## UI Elements ###############################################################
  ##

  # card_left <- card(
  #   h1("Welcome"),
  #
  #   h4("What is ", tags$code("arena-helpers")),
  #   p(
  #     "This app is part of of a collection of tools designed to support Forest inventory related
  #     activities and grouped under ", tags$code("arena-helpers")
  #     ),
  #   br(),
  #   h4("Open Foris Arena ", tags$img(src="assets/Arena-Logo.png", height = '30px')),
  #   p(
  #     "They aim to provide additional functionality to ",
  #     tags$a(
  #       href = "https://openforis.org/solutions/arena/",
  #       #alt = "arena-helpers",
  #       "Open Foris Arena ",
  #       bsicons::bs_icon("box-arrow-up-right", class = "text-primary"),
  #       .noWS = "before-end"
  #     ),
  #     " in particular support data analysis parts that cannot be embedded directly
  #     to OF Arena "
  #   )
  # )

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
    card_header(tag$code("arena-helpers")),
    p("More on arena-helpers"),
    p("Add link to webpage")
  )

  card3 <- card(
    card_header("{mocaredd}"),
    p("More info on mocaredd"),
    p("link to info tab")
  )

  card_right_top <- card(
    card_header(bsicons::bs_icon("1-circle-fill", size = "1.5rem", class = "text-primary"), "Card right top"),
    layout_column_wrap(
      width = 1/2,
      card_right_sub1,
      card_right_sub2
    )
  )

  card_right_bot <- card(
    p("placeholder for more content")
  )

  card_right <- card(
    card_right_top,
    card_right_bot
  )


  ## UI elements wrapped in a tagList() function
  tagList(

    h1(i18n$t("Welcome to {mocaredd}")),

    card(
      "
      {mocaredd} is a R package and a Shiny application designed to help you with
      running Monte Carlo Simulations for REDD+ uncertainty analysis.
      ",
      "
      {mocaredd} is part of arena-helpers: a collection of tools to assist
      ",
      "Openforis Arena is a ..."
    ),

    layout_columns(
      col_widths = c(4, 4, 4,),
      card1,
      card2,
      card3
    )

  ) ## END tagList

} ## END module UI function
