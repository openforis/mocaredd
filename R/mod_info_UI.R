


mod_info_UI <- function(id){

  ## From https://shiny.rstudio.com/articles/modules.html
  # `NS(id)` returns a namespace function, which was save as `ns` and will
  # invoke later.
  ns <- NS(id)

  ##
  ## UI Elements ###############################################################
  ##

  card_left <- card(
    h1("Welcome"),

    h4("What is ", tags$code("arena-helpers")),
    p(
      "This app is part of of a collection of tools designed to support Forest inventory related
      activities and grouped under ", tags$code("arena-helpers")
      ),
    br(),
    h4("Open Foris Arena ", tags$img(src="assets/Arena-Logo.png", height = '30px')),
    p(
      "They aim to provide additional functionality to ",
      tags$a(
        href = "https://openforis.org/solutions/arena/",
        #alt = "arena-helpers",
        "Open Foris Arena ",
        bsicons::bs_icon("box-arrow-up-right", class = "text-primary"),
        .noWS = "before-end"
      ),
      " in particular support data analysis parts that cannot be embedded directly
      to OF Arena "
    )
  )


  ## Emission factors ----------------------------------------------------------
  card_right_sub1 <- card(
    p("Placeholder for content"),
  )

  card_right_sub2 <- card(
    p("placeholder for content")
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

    layout_columns(
      col_widths = c(4, 8),
      card_left,
      card_right
    )

  ) ## END tagList

} ## END module UI function
