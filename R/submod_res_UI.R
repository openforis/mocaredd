


submod_res_UI <- function(id, i18n){

  ## From https://shiny.rstudio.com/articles/modules.html
  # `NS(id)` returns a namespace function, which was save as `ns` and will
  # invoke later.
  ns <- NS(id)

  ##
  ## UI Elements ###############################################################
  ##

  card_left <- card(
    h4("WELCOME TO MODULE 2"),
    p(
      bsicons::bs_icon("1-circle", class = "text-primary"),
      "More text."),
    br(),
    p(
      bsicons::bs_icon("2-circle", class = "text-primary"),
      "More text again."
    ),
  )


  ## Emission factors ----------------------------------------------------------
  card_right_sub1 <- card(
    p("Placeholder for module 2 content"),
  )

  card_right_sub2 <- card(
    p("placeholder for module 2 content")
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
