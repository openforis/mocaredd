#' Home module UI function (v2)
#'
#' @noRd
mod_home_UI2 <- function(id, i18n, .tr){

  ns <- NS(id)


  ##
  ## UI Elements ###############################################################
  ##

  ## + Hero section ============================================================
  ## Full-width banner as on openforis.org, with the title and button in a box
  ## over the image (box under the image on small screens)
  hero_banner <- div(
    class = "hero-banner",
    tags$img(src = "assets/hero-olive-amber.svg", alt = "Illustration of a forest landscape"),
    div(
      class = "hero-overlay",
      div(
        class = "container",
        div(
          class = "hero-box",
          h1(class = "hero-title", i18n$t(.tr$hero_title)),
          actionButton(
            inputId = ns("to_tool"),
            label = tagList(i18n$t(.tr$hero_btn), bsicons::bs_icon("chevron-right")),
            class = "btn-primary btn-icon" ## FAO Design System primary button with chevron
          )
        )
      )
    )
  )


  ## + Features section ========================================================
  card1 <- card(
    class = "feature-card shadow-sm",
    card_body(
      h3("{mocaredd}", style = "font-weight:700;"),
      p(i18n$t(.tr$hero_txt)),
      p(
        i18n$t(.tr$feat1_p2),
        tags$a(
          href = "https://github.com/openforis/mocaredd",
          target = "_blank",
          tags$span("Github", bsicons::bs_icon("github"))
        )
      ),
      p(i18n$t(.tr$feat1_p3))
    )
  )

  card2 <- card(
    class = "feature-card shadow-sm",
    card_body(
      h3(i18n$t(.tr$feat2_title), style = "font-weight:700;"),
      p(i18n$t(.tr$feat2_p1)),
      p(i18n$t(.tr$feat2_p2)),
      p(i18n$t(.tr$feat2_p3))
    )
  )

  card3 <- card(
    class = "feature-card shadow-sm",
    card_body(
      h3(i18n$t(.tr$feat3_title), style = "font-weight:700;"),
      p(i18n$t(.tr$feat3_p1)),
      p(i18n$t(.tr$feat3_p2)),
      p(
        i18n$t(.tr$feat3_p3), HTML("&nbsp;"),
        actionButton(
          inputId = ns("to_about"),
          label = i18n$t(.tr$feat3_btn),
          class = "btn-primary"
        )
      )
    )
  )


  ##
  ## Layout UI elements with tagList() function ################################
  ##

  tagList(

    ## Hero section
    hero_banner,

    ## Features section
    div(
      class = "container section-padding",
      div(
        class = "row g-4",
        div(class = "col-md-4", card1),
        div(class = "col-md-4", card2),
        div(class = "col-md-4", card3)
      )
    ),

    br()

  ) ## END tagList

} ## END module UI function
