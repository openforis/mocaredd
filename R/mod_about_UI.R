#' About module UI function
#'
#' @noRd
mod_about_UI <- function(id, i18n, .tr){

  ns <- NS(id)



  ##
  ## UI Elements ###############################################################
  ##

  ## + Header ==================================================================

  about_header <- div(
    class = "about-header",
    h2(class = "about-title", i18n$t(.tr$about_title)),
    p(class = "about-lead", i18n$t(.tr$about_lead))
  )

  ## + How it works ============================================================

  ## Four steps joined by chevrons, the two {mocaredd} steps in Open Foris green
  about_steps <- div(
    class = "about-steps",
    purrr::pmap(
      list(
        num   = 1:4,
        title = list(.tr$about_step1_title, .tr$about_step2_title, .tr$about_step3_title, .tr$about_step4_title),
        txt   = list(.tr$about_step1_txt, .tr$about_step2_txt, .tr$about_step3_txt, .tr$about_step4_txt)
      ),
      \(num, title, txt) tagList(
        if (num > 1) div(class = "about-chevron", bsicons::bs_icon("chevron-right")),
        div(
          class = paste("about-step", if (num >= 3) "about-step-app"),
          div(class = "about-step-num", num),
          h4(i18n$t(title)),
          p(i18n$t(txt))
        )
      )
    )
  )

  ## Workflow in detail (pop-up): data preparation band, then {mocaredd} band.
  ## Bootstrap modal markup, so the text is translated on the client like the rest
  about_flow <- div(
    class = "about-flow",
    purrr::pmap(
      list(
        band  = c("about-flow-prep", "about-flow-app"),
        label = list(i18n$t(.tr$about_flow_prep), "{mocaredd}"),
        items = list(
          list(.tr$about_step1_title, .tr$about_step2_title, .tr$about_flow_file),
          list(.tr$about_flow_load, .tr$about_flow_ari, .tr$about_flow_mcs, .tr$about_flow_sens, .tr$about_flow_review)
        )
      ),
      \(band, label, items) div(
        class = paste("about-flow-band", band),
        div(class = "about-flow-label", label),
        div(
          class = "about-flow-items",
          purrr::imap(items, \(item, i) tagList(
            if (i > 1) bsicons::bs_icon("chevron-right", class = "about-flow-chevron"),
            span(
              class = paste("about-flow-item", if (identical(item, .tr$about_flow_file)) "about-flow-file"),
              i18n$t(item)
            )
          ))
        )
      )
    )
  )

  about_flow_modal <- div(
    id = ns("flow_modal"),
    class = "modal fade",
    tabindex = "-1",
    `aria-labelledby` = ns("flow_modal_title"),
    `aria-hidden` = "true",
    div(
      class = "modal-dialog modal-xl modal-dialog-centered",
      div(
        class = "modal-content",
        div(
          class = "modal-header",
          h5(id = ns("flow_modal_title"), class = "modal-title", i18n$t(.tr$about_flow_title)),
          tags$button(type = "button", class = "btn-close", `data-bs-dismiss` = "modal", `aria-label` = "Close")
        ),
        div(class = "modal-body", about_flow)
      )
    )
  )

  ## Buttons: workflow pop-up and the package documentation
  about_actions <- div(
    class = "about-actions",
    tags$button(
      type = "button",
      class = "btn btn-primary btn-icon",
      `data-bs-toggle` = "modal",
      `data-bs-target` = paste0("#", ns("flow_modal")),
      i18n$t(.tr$about_flow_btn),
      bsicons::bs_icon("diagram-3")
    ),
    tags$a(
      class = "btn btn-primary btn-icon",
      href = "https://openforis.github.io/mocaredd/",
      target = "_blank",
      i18n$t(.tr$about_doc_btn),
      bsicons::bs_icon("box-arrow-up-right")
    )
  )

  about_how <- tags$section(
    class = "about-section",
    h3(i18n$t(.tr$about_how_title)),
    p(class = "about-intro", i18n$t(.tr$about_how_intro)),
    about_steps,
    about_actions,
    about_flow_modal
  )

  ## + Demo dataset ============================================================

  ## The data overview describes inst/extdata/mocaredd-templatev2-simple.xlsx: update it
  ## if the demo dataset changes
  demo_kb <- system.file("extdata/mocaredd-templatev2-simple.xlsx", package = "mocaredd") |>
    file.size() |>
    (\(x) round(x / 1024))()

  demo_use <- card(
    card_body(
      h4(i18n$t(.tr$about_demo_howto)),
      tags$ol(
        tags$li(i18n$t(.tr$about_demo_s1)),
        tags$li(i18n$t(.tr$about_demo_s2)),
        tags$li(i18n$t(.tr$about_demo_s3))
      ),
      div(
        downloadButton(
          outputId = ns("dl_example"),
          label = tagList(i18n$t(.tr$about_demo_btn), sprintf(" (XLSX, %s KB)", demo_kb)),
          class = "btn-primary"
        )
      )
    )
  )

  demo_overview <- card(
    card_body(
      h4(i18n$t(.tr$about_overview_title)),
      tags$dl(
        class = "about-facts",
        purrr::map2(
          list(.tr$about_fact_template, .tr$about_fact_time, .tr$about_fact_ad),
          list(.tr$about_fact_template_val, .tr$about_fact_time_val, .tr$about_fact_ad_val),
          \(term, value) tagList(tags$dt(i18n$t(term)), tags$dd(i18n$t(value)))
        )
      ),
      hr(class = "my-1"),
      tags$dl(
        class = "about-facts",
        purrr::map2(
          list(.tr$about_ex_lu, .tr$about_ex_redd, .tr$about_ex_carbon, .tr$about_ex_sims),
          list(.tr$about_ex_lu_val, .tr$about_ex_redd_val, .tr$about_ex_carbon_val, .tr$about_ex_sims_val),
          \(term, value) tagList(tags$dt(i18n$t(term)), tags$dd(i18n$t(value)))
        )
      )
    )
  )

  about_demo <- tags$section(
    class = "about-section",
    h3(i18n$t(.tr$about_demo_title)),
    p(class = "about-intro", i18n$t(.tr$about_demo_intro)),
    layout_columns(
      col_widths = breakpoints(sm = 12, lg = c(5, 7)),
      fill = FALSE,
      demo_use,
      demo_overview
    )
  )

  ## + Context =================================================================

  about_context <- tags$section(
    class = "about-section",
    h3(i18n$t(.tr$about_context_title)),
    layout_column_wrap(
      width = "300px",
      fill = FALSE,
      !!!purrr::pmap(
        list(
          title = list("REDD+", i18n$t(.tr$about_unc_title), "{mocaredd}"),
          txt   = list(.tr$about_redd_txt, .tr$about_unc_txt, .tr$about_moc_txt),
          href  = list(
            "https://unfccc.int/topics/land-use/workstreams/redd/what-is-redd",
            "https://www.ipcc-nggip.iges.or.jp/public/2006gl/vol1.html",
            "https://github.com/openforis/mocaredd"
          ),
          link  = list("unfccc.int", "2006 IPCC Guidelines, Volume 1", i18n$t(.tr$about_moc_link))
        ),
        \(title, txt, href, link) card(
          card_header(title),
          card_body(
            p(i18n$t(txt)),
            tags$a(href = href, target = "_blank", class = "mt-auto", link)
          )
        )
      )
    )
  )

  ## + Under the hood ==========================================================

  struct_package <- card(
    card_header(i18n$t(.tr$about_pkg_title)),
    card_body(
      tags$dl(
        class = "about-facts",
        tags$dt(i18n$t(.tr$about_pkg_version)),
        tags$dd(as.character(utils::packageVersion("mocaredd"))),
        tags$dt(i18n$t(.tr$about_pkg_licence)),
        tags$dd("MIT, \u00a9 2024 mocaredd authors, Open Foris"),
        tags$dt(i18n$t(.tr$about_pkg_authors)),
        tags$dd(
          "Gael Sola (", span(i18n$t(.tr$about_pkg_maintainer), .noWS = c("outside", "inside")), "), ",
          "Javier Garcia Perez, Jimena Saucedo Miranda, Lauri Vesa"
        ),
        tags$dt(i18n$t(.tr$about_pkg_source)),
        tags$dd(tags$a(href = "https://github.com/openforis/mocaredd", target = "_blank", "github.com/openforis/mocaredd")),
        tags$dt(i18n$t(.tr$about_pkg_doc)),
        tags$dd(tags$a(href = "https://openforis.github.io/mocaredd/", target = "_blank", "openforis.github.io/mocaredd"))
      ),
      p(i18n$t(.tr$about_pkg_install)),
      tags$pre(
        class = "about-code",
        paste(
          'if (!require(remotes)) install.packages("remotes")',
          'remotes::install_github("openforis/mocaredd", upgrade = "never")',
          "mocaredd::shiny_run_mocaredd()",
          sep = "\n"
        )
      ),
      p(tags$a(href = "https://openforis-shiny.shinyapps.io/mocaredd/", target = "_blank", i18n$t(.tr$about_pkg_online)))
    )
  )

  struct_deps <- card(
    card_body(
      h4(i18n$t(.tr$about_dep_title)),
      p(i18n$t(.tr$about_dep_intro)),
      purrr::map2(
        list(.tr$about_dep_ui, .tr$about_dep_data, .tr$about_dep_stats, .tr$about_dep_report),
        list(
          c("shiny", "bslib", "bsicons", "shinyWidgets", "shinyjs", "shiny.i18n", "htmltools"),
          c("readxl", "dplyr", "tidyr", "purrr", "stringr", "rlang"),
          c("stats"),
          c("ggplot2", "gt", "scales")
        ),
        \(group, pkgs) div(
          class = "about-dep-group",
          div(class = "about-dep-label", i18n$t(group)),
          div(class = "about-dep-chips", purrr::map(pkgs, \(x) span(class = "about-dep-chip", x)))
        )
      )
    )
  )

  ## Latest developments: periods stacked, newest first, until the last month only
  struct_dev <- card(
    card_body(
      h4(i18n$t(.tr$about_dev_title)),
      div(
        purrr::map2(
          list(.tr$about_dev_next, .tr$about_dev_oct),
          list(
            list(.tr$about_dev_next1, .tr$about_dev_next2),
            list(.tr$about_dev_oct1, .tr$about_dev_oct2, .tr$about_dev_oct3)
          ),
          \(period, items) div(
            class = "about-dev-month",
            strong(i18n$t(period)),
            tags$ul(purrr::map(items, \(x) tags$li(i18n$t(x))))
          )
        )
      )
    )
  )

  ## Use cases and stories: links to info notes and stories (to come)
  struct_cases <- card(
    card_body(
      h4(i18n$t(.tr$about_cases_title)),
      p(class = "about-small", i18n$t(.tr$about_cases_soon))
    )
  )

  ## Staggered widths: 7/5 then 5/7
  about_structure <- tags$section(
    class = "about-section",
    h3(i18n$t(.tr$about_struct_title)),
    layout_columns(
      col_widths = breakpoints(sm = 12, lg = c(7, 5)),
      fill = FALSE,
      struct_package,
      struct_deps
    ),
    layout_columns(
      col_widths = breakpoints(sm = 12, lg = c(5, 7)),
      fill = FALSE,
      struct_cases,
      struct_dev
    )
  )



  ##
  ## Layout UI elements with tagList() function ################################
  ##

  tagList(
    div(
      class = "about-page",
      about_header,
      about_how,
      about_demo,
      about_context,
      about_structure
    )
  ) ## END tagList

} ## END module UI function
