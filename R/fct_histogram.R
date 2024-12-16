#' Generate histogram to show the simulations at any stage.
#'
#' @description
#' Takes simulation data from fct_combine_mcs_E() or fct_combine_mcs_P() and
#' aggregated results from fct_calc_res() and generate an histogram showing the
#' PDF density, modeled normal distribution, simulations' median and confidence
#' interval.
#'
#' @param .data data frame of simulated data. Output of fct_combine_mcs_E() or fct_combine_mcs_P().
#' @param .res aggregated results of a simulation. Output of fct_calc_res()
#' @param .id ID column of .data and .res inputs.
#' @param .value simulated values column in .data.
#' @param .value_type "E" or "ER" if .data represents emissions or emission reductions respectively.
#' @param ... filter query, passed to [dplyr::filter()].
#'
#' @return a ggplot or a named list of ggplots
#'
#' @importFrom rlang .data
#' @importFrom ggplot2 ggplot aes geom_histogram after_stat geom_density geom_vline labs
#'             scale_x_continuous guide_axis theme theme_classic coord_cartesian element_text
#'             element_blank
#'
#' @export
fct_histogram <- function(.data, .res, .id, .value, .value_type, ...){

  id    <- rlang::enquo(.id)
  value <- rlang::enquo(.value)

  dat <- .data |> dplyr::filter(...)
  res <- .res |> dplyr::filter(...)

  ## Extend plot range to 5% of the simulated values' range
  sim_min <- min(.data[[rlang::as_name(value)]], 0)
  sim_max <- max(.data[[rlang::as_name(value)]], 0)
  plot_min <- sim_min - ((sim_max - sim_min) * 0.05)
  plot_max <- sim_max + ((sim_max - sim_min) * 0.05)
  plot_range <- c(plot_min, plot_max)

  ## Get iterations for map()
  iter <- res |> dplyr::pull(!!id) |> unique()

  gg_ER <-purrr::map(iter, function(x){

    dat2 <- dat |> dplyr::filter(!!id == x)
    res2 <- res |> dplyr::filter(!!id == x)
    brk <- sort(c(0, res2$E_cilower, res2$E, res2$E_ciupper))

    if (.value_type == "ER") {
      cols <- c("darkseagreen1", "darkseagreen", "forestgreen")
      subt <- paste0("Emission reductions for: ", x, " (tCO2e/year)")
    } else if (.value_type == "E") {
      cols <- c("pink", "pink3", "hotpink")
      subt <- paste0("Emissions for: ", x, " (tCO2e/year)")
    }


    ggplot(dat2, aes(x = !!value)) +
      geom_histogram(aes(y = after_stat(.data$density)), fill = cols[1], color = cols[2]) +
      ## approximated normal dist. replaced by density.
      # stat_function(
      #   fun = stats::dnorm,
      #   colour = cols[3],
      #   args = list(
      #     mean = mean(dat2[[rlang::as_name(value)]], na.rm = TRUE),
      #     sd = stats::sd(dat2[[rlang::as_name(value)]], na.rm = TRUE)
      #   )
      # ) +
      geom_density(col = cols[3]) +
      geom_vline(xintercept = res2$E, color = "darkred", linewidth = 0.8) +
      geom_vline(xintercept = res2$E_cilower, color = "darkblue", linewidth = 0.6) +
      geom_vline(xintercept = res2$E_ciupper, color = "darkblue", linewidth = 0.6) +
      geom_vline(xintercept = 0, linetype = "dotted", linewidth = 0.6) +
      labs(
        x = "Simulated values",
        y = "Density",
        caption = "Red: median \nBlue: confidence interval \nDots: x=0",
        subtitle = subt
      ) +
      scale_x_continuous(labels = scales::comma, breaks = brk, guide = guide_axis(n.dodge = 2)) +
      coord_cartesian(xlim = plot_range) +
      theme_classic(base_size = 20) +
      theme(
        axis.text.y = element_blank(), axis.ticks.y = element_blank(),
        plot.caption.position = "plot",
        plot.caption = element_text(hjust = 0)
      )

  })

  names(gg_ER) <- iter

  gg_ER

}

