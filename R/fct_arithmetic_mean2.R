#' Calculate emissions and IPCC Tier 1 uncertainty from arithmetic means
#'
#' @description
#' Analytical counterpart of \code{fct_arithmetic_mean()}. Instead of Monte Carlo
#' simulations, activity data and emission factors are aggregated deterministically
#' and their uncertainty is propagated with the IPCC Tier 1 (first-order error
#' propagation) rules: relative uncertainties combine in quadrature for products
#' and absolute standard errors combine in quadrature for sums / differences. The
#' propagation is done numerically over the independent input variables (carbon
#' pool values, carbon fraction, degradation ratios and activity data), so
#' correlations introduced by shared inputs (e.g. a degraded land use that reuses
#' the intact carbon stock, or a global carbon fraction) are handled consistently.
#'
#' The result reports, for each reference / monitoring emission level and for each
#' emission reduction, the mean, its standard error, and the half-width uncertainty
#' \eqn{U\% = z \cdot se / |mean| \cdot 100}, where \eqn{z} is the two-sided normal
#' quantile for \code{setup$conf_level}.
#'
#' @param .checked_data The list returned by \code{fct_checkinput()}. Its
#'             \code{template_version} element drives version-specific behaviour and its
#'             \code{data} element supplies the \code{setup}, \code{time}, \code{area}
#'             and \code{carbon} tables.
#'
#' @return A list with elements \code{ER} (reference / monitoring levels and emission
#'         reductions with mean, se, U\% and confidence bounds), \code{emissions}
#'         (per time period, with uncertainty), \code{gg_emissions} (figure with
#'         emissions as lines with confidence interval areas, and emission
#'         reductions as points with error bars) and
#'         \code{emissions_table} (a tidy tibble ready to
#'         be turned into a gt table by the app).
#'
#' @importFrom rlang .data
#'
#' @examples
#' path <- system.file("extdata/mocaredd-templatev2-simple.xlsx", package = "mocaredd")
#'
#' checked <- fct_checkinput(.path = path)
#'
#' res <- fct_arithmetic_mean2(.checked_data = checked)
#' res$ER
#'
#' @export
fct_arithmetic_mean2 <- function(.checked_data){

  ## !!! FOR TESTING ONLY
  # .checked_data <- fct_checkinput(.path = path)
  ## !!!

  setup     <- .checked_data$data$setup
  time      <- .checked_data$data$time
  area      <- .checked_data$data$area
  carbon    <- .checked_data$data$carbon
  is_v2     <- isTRUE(.checked_data$template_version == 2)
  is_annual <- isTRUE(setup$ad_annual)

  ## z value for the requested confidence level (two-sided)
  z_score <- .checked_data$data$setup$z_score

  ##
  ## 1. Carbon formula and degradation handling (mirror fct_combine_mcs_E) ######
  ##
  carbon_pools <- if (is_v2) dplyr::filter(carbon, .data$c_element != "CF") else carbon

  to_intact <- function(lu_id) {
    out <- stringr::str_remove(lu_id, setup$dg_ext)
    if (is_v2) out <- stringr::str_replace(out, "[^A-Za-z]$", "")
    out
  }

  if (is_v2) {
    c_formula <- carbon_pools |>
      dplyr::summarise(c_el = list(c(.data$c_element)), c_un = list(c(.data$c_unit)),
                       .by = c("c_period", "c_lu_id")) |>
      dplyr::rowwise() |>
      dplyr::mutate(c_form = fct_make_formula2(.c_el = .data$c_el, .c_unit = .data$c_un, .version = 2)) |>
      dplyr::ungroup()
  } else {
    c_formula <- carbon_pools |>
      dplyr::summarise(c_el = list(c(.data$c_element)), .by = c("c_period", "c_lu_id")) |>
      dplyr::rowwise() |>
      dplyr::mutate(c_form = fct_make_formula2(.c_el = .data$c_el, .c_unit = setup$c_unit, .version = 1)) |>
      dplyr::ungroup()
  }
  form_of <- stats::setNames(c_formula$c_form, c_formula$c_lu_id)

  degrat_lu <- carbon_pools |> dplyr::filter(.data$c_element == "DG_ratio") |> dplyr::pull("c_lu_id")

  ## Pools affected by degradation. "ALL" degrades the whole stock; otherwise the
  ## excluded pools stay at their intact level (as in fct_combine_mcs_E).
  dg_pool_vec <- stringr::str_trim(stringr::str_split(stringr::str_trim(setup$dg_pool), ",")[[1]])
  dg_is_all   <- length(dg_pool_vec) == 1 && dg_pool_vec == "ALL"

  ## Carbon fraction (global). v2: dedicated 'carbon' row; v1: from setup.
  if (is_v2) {
    cf_row <- carbon |> dplyr::filter(.data$c_element == "CF")
    has_cf <- nrow(cf_row) > 0
    cf_mean <- if (has_cf) cf_row$c_value[1] else NA_real_
    cf_se   <- if (has_cf) cf_row$c_se[1]    else NA_real_
  } else {
    has_cf <- is.numeric(setup[["c_fraction"]]) && isTRUE(setup[["c_unit"]] == "DM")
    cf_mean <- if (has_cf) setup$c_fraction    else NA_real_
    cf_se   <- if (has_cf) setup$c_fraction_se else NA_real_
  }

  ##
  ## 2. Independent input variables (means and standard errors) #################
  ##
  ## Carbon pools, namespaced by land use so each is an independent input.
  pool_key  <- paste0(carbon_pools$c_lu_id, "..", carbon_pools$c_element)
  prim_mean <- stats::setNames(carbon_pools$c_value, pool_key)
  prim_se   <- stats::setNames(carbon_pools$c_se,    pool_key)
  ## Activity data, namespaced by transition.
  ad_key    <- paste0("AD..", area$trans_id)
  prim_mean <- c(prim_mean, stats::setNames(area$trans_area, ad_key))
  prim_se   <- c(prim_se,   stats::setNames(area$trans_se,   ad_key))
  ## Global carbon fraction.
  if (has_cf) { prim_mean["CF"] <- cf_mean; prim_se["CF"] <- cf_se }

  prim_keys <- names(prim_mean)

  ## Carbon stock of a land use, from a (possibly perturbed) input vector.
  C_of <- function(lu, pm) {
    if (lu %in% degrat_lu) {
      dg     <- pm[[paste0(lu, "..DG_ratio")]]
      intact <- to_intact(lu)
      if (intact == lu) stop("dg_ext '", setup$dg_ext, "' not found in degraded land use '", lu, "'")
      c_int  <- C_of(intact, pm)
      if (dg_is_all) return(dg * c_int)
      ## Pools not affected by degradation keep their intact value.
      int_els <- carbon_pools$c_element[carbon_pools$c_lu_id == intact]
      excl    <- setdiff(int_els, dg_pool_vec)
      c_noDG  <- if (length(excl) > 0) sum(pm[paste0(intact, "..", excl)]) else 0
      return(dg * c_int + (1 - dg) * c_noDG)
    }
    els <- carbon_pools$c_element[carbon_pools$c_lu_id == lu]
    env <- as.list(stats::setNames(pm[paste0(lu, "..", els)], els))
    if (has_cf) env$CF <- pm[["CF"]]
    eval(parse(text = form_of[[lu]]), env)
  }

  ## Lookups used by the aggregation
  nb_years   <- stats::setNames(time$nb_years, time$period_no)
  ptype      <- stats::setNames(time$period_type, time$period_no)
  ref_types  <- unique(stats::na.omit(time$period_type[stringr::str_detect(time$period_type, "REF")]))
  mon_types  <- unique(stats::na.omit(time$period_type[stringr::str_detect(time$period_type, "MON")]))
  a_period   <- area$trans_period
  a_activity <- area$redd_activity
  a_li       <- area$lu_initial_id
  a_lf       <- area$lu_final_id
  a_id       <- area$trans_id

  ##
  ## 3. All scalar outputs as a function of the inputs #########################
  ##
  ## PN.<period>        annual emissions of a time period (all activities)
  ## PA.<period>.<act>  annual emissions of a time period for one REDD+ activity
  ## PT.<ptype>         annual emissions of a reference / monitoring period type
  ## ER.<montype>       emission reductions = REF - MON
  metrics <- function(pm) {
    Et <- purrr::map_dbl(seq_along(a_id), function(i) {
      pm[[paste0("AD..", a_id[i])]] * (C_of(a_li[i], pm) - C_of(a_lf[i], pm)) * 44 / 12
    })
    Ey <- if (is_annual) Et else Et / nb_years[a_period]

    out <- c()
    for (p in unique(a_period)) {
      sel <- a_period == p
      out[paste0("PN.", p)] <- sum(Ey[sel])
      for (act in unique(a_activity[sel])) {
        out[paste0("PA.", p, ".", act)] <- sum(Ey[sel & a_activity == act])
      }
    }
    for (pt in c(ref_types, mon_types)) {
      per <- names(ptype)[ptype %in% pt & !is.na(ptype)]
      Ty  <- sum(nb_years[per])
      sel <- a_period %in% per
      out[paste0("PT.", pt)] <- if (is_annual) sum(Et[sel] * nb_years[a_period[sel]]) / Ty else sum(Et[sel]) / Ty
    }
    if (length(ref_types) >= 1) {
      refv <- out[[paste0("PT.", ref_types[1])]]
      for (m in mon_types) out[paste0("ER.", m)] <- refv - out[[paste0("PT.", m)]]
    }
    out
  }

  ##
  ## 4. IPCC Tier 1 propagation: numeric jacobian over the inputs ##############
  ##
  m0  <- metrics(prim_mean)
  keys_out <- names(m0)
  var <- stats::setNames(rep(0, length(m0)), keys_out)

  for (k in prim_keys) {
    sk <- prim_se[[k]]
    if (is.na(sk) || sk == 0) next
    h  <- max(abs(prim_mean[[k]]) * 1e-4, 1e-6)
    pp <- prim_mean; pp[k] <- prim_mean[[k]] + h
    pm <- prim_mean; pm[k] <- prim_mean[[k]] - h
    d  <- (metrics(pp)[keys_out] - metrics(pm)[keys_out]) / (2 * h)
    var <- var + (d * sk)^2
  }
  se_out <- sqrt(var)

  ## helper to fetch (mean, se, U%) for one output key
  stat_of <- function(key) {
    m <- unname(m0[key]); s <- unname(se_out[key])
    u <- if (is.na(m) || m == 0) NA_real_ else z_score * s / abs(m) * 100
    list(mean = m, se = s, U = u, lower = m - z_score * s, upper = m + z_score * s)
  }

  ##
  ## 5. Reference / monitoring levels and emission reductions ##################
  ##
  combi_rows <- c(
    stats::setNames(paste0("PT.", ref_types), ref_types),
    stats::setNames(paste0("PT.", mon_types), paste0("E-", mon_types)),
    stats::setNames(paste0("ER.", mon_types), paste0("ER-", mon_types))
  )
  ari_combi <- purrr::imap_dfr(combi_rows, function(key, label){
    s <- stat_of(key)
    dplyr::tibble(period_type = label, E = s$mean, E_se = s$se, E_U = s$U,
                  E_lower = s$lower, E_upper = s$upper)
  })

  ##
  ## 6. Per time period emissions (with uncertainty) ##########################
  ##
  period_meta <- time |>
    dplyr::select("period_no", "year_start", "year_end", "period_type") |>
    dplyr::arrange(.data$year_start)

  out_combi <- period_meta |>
    dplyr::rowwise() |>
    dplyr::mutate(
      E    = stat_of(paste0("PN.", .data$period_no))$mean,
      E_se = stat_of(paste0("PN.", .data$period_no))$se,
      E_U  = stat_of(paste0("PN.", .data$period_no))$U,
      years = ifelse(.data$year_start == .data$year_end, as.character(.data$year_start),
                     paste0(.data$year_start, "-", .data$year_end))
    ) |>
    dplyr::ungroup()

  ##
  ## 7. Figure: emissions (top) and emission reductions (bottom) ###############
  ##
  ## Top panel: each period as its mean emissions (line) and confidence interval
  ## (area), with the reference level (FREL) extended over the whole time line.
  ## Bottom panel: emission reductions per monitoring type as a point with its
  ## confidence interval (error bar), on their own axis.
  FREL <- stat_of(paste0("PT.", ref_types[1]))$mean / 1e6

  panel_lab <- c("Emissions\n(MtCO2e/y)", "Reductions\n(MtCO2e/y)")

  out_mark <- period_meta |>
    dplyr::mutate(
      s       = purrr::map(.data$period_no, \(p) stat_of(paste0("PN.", p))),
      E       = purrr::map_dbl(.data$s, "mean")  / 1e6,
      E_lower = purrr::map_dbl(.data$s, "lower") / 1e6,
      E_upper = purrr::map_dbl(.data$s, "upper") / 1e6,
      xmin    = .data$year_start - 0.5,
      xmax    = .data$year_end + 0.5,
      panel   = factor(panel_lab[1], levels = panel_lab),
      ## Periods outside REF and MON (e.g. a gap year) are drawn in grey
      period_type = dplyr::coalesce(.data$period_type, "Not used")
    ) |>
    dplyr::select(-"s")

  ## ER per monitoring type, centred on the span of its periods
  out_er <- ari_combi |>
    dplyr::filter(stringr::str_detect(.data$period_type, "^ER-")) |>
    dplyr::mutate(period_type = stringr::str_remove(.data$period_type, "^ER-")) |>
    dplyr::inner_join(
      dplyr::summarise(period_meta, x = (min(.data$year_start) + max(.data$year_end)) / 2, .by = "period_type"),
      by = "period_type"
    ) |>
    dplyr::mutate(
      ER       = .data$E / 1e6,
      ER_lower = .data$E_lower / 1e6,
      ER_upper = .data$E_upper / 1e6,
      label    = paste0(round(.data$ER, 2), "\n\u00b1", round(.data$E_U), "%"),
      panel    = factor(panel_lab[2], levels = panel_lab)
    )

  ## Zero in both panels, zero line in the ER panel
  zero <- dplyr::tibble(panel = factor(panel_lab, levels = panel_lab), y = 0)

  ## Nearest xterm-256 colours (xterm id); green is kept for ER
  col_ref <- "#005FAF"                                                    # 25
  col_mon <- c("#D7AF00", "#D75F00", "#D787AF", "#5FAFD7", "#FFD75F")     # 178, 166, 175, 74, 221
  col_er  <- "#00875F"                                                    # 29

  pal <- c(
    stats::setNames(rep(col_ref, length(ref_types)), ref_types),
    stats::setNames(rep_len(col_mon, length(mon_types)), mon_types),
    "Not used" = "gray60"
  )

  out_gg <- ggplot2::ggplot() +
    ggplot2::geom_blank(data = zero, ggplot2::aes(x = min(out_mark$xmin), y = .data$y)) +
    ggplot2::geom_hline(data = zero[2, ], ggplot2::aes(yintercept = .data$y), colour = "gray50", linetype = "dashed") +
    ## Emissions: confidence interval as area, mean as line
    ggplot2::geom_rect(
      data = out_mark,
      ggplot2::aes(xmin = .data$xmin, xmax = .data$xmax, ymin = .data$E_lower, ymax = .data$E_upper,
                   fill = .data$period_type),
      alpha = 0.2
    ) +
    ggplot2::geom_segment(
      data = dplyr::mutate(out_mark, FREL = FREL),
      ggplot2::aes(x = min(out_mark$xmin), xend = max(out_mark$xmax), y = .data$FREL, yend = .data$FREL),
      colour = col_ref, linewidth = 0.8, linetype = "dashed"
    ) +
    ggplot2::geom_segment(
      data = out_mark,
      ggplot2::aes(x = .data$xmin, xend = .data$xmax, y = .data$E, yend = .data$E, colour = .data$period_type),
      linewidth = 1.2
    ) +
    ## Emission reductions: point and confidence interval
    ggplot2::geom_errorbar(
      data = out_er,
      ggplot2::aes(x = .data$x, ymin = .data$ER_lower, ymax = .data$ER_upper),
      colour = col_er, width = 0.2, linewidth = 0.8
    ) +
    ggplot2::geom_point(data = out_er, ggplot2::aes(x = .data$x, y = .data$ER), colour = col_er, size = 4) +
    ggplot2::geom_text(
      data = out_er,
      ggplot2::aes(x = .data$x, y = .data$ER_upper, label = .data$label),
      colour = col_er, fontface = "bold", size = 4, vjust = -0.3, lineheight = 0.9
    ) +
    ggplot2::facet_grid(rows = ggplot2::vars(.data$panel), scales = "free_y", switch = "y") +
    ggplot2::scale_colour_manual(values = pal, na.value = "gray50", aesthetics = c("colour", "fill")) +
    ggplot2::scale_x_continuous(breaks = min(period_meta$year_start):max(period_meta$year_end), minor_breaks = NULL) +
    ggplot2::scale_y_continuous(expand = ggplot2::expansion(mult = c(0.05, 0.3))) +
    ggplot2::theme_bw(base_size = 16) +
    ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 45, hjust = 1),
                   legend.position = "bottom",
                   strip.placement = "outside",
                   strip.background = ggplot2::element_blank(),
                   strip.text.y.left = ggplot2::element_text(angle = 90),
                   plot.caption = ggplot2::element_text(size = ggplot2::rel(0.56))) +
    ggplot2::labs(
      x = "Years", y = NULL, colour = "", fill = "",
      caption = paste0(
        "Lines: mean annual emissions, dashed: reference level. Areas and error bars: ",
        setup$conf_level * 100, "% confidence intervals."
      )
    )

  ##
  ## 8. Result table (tidy data; gt is built in the server) ####################
  ##
  act_present <- unique(a_activity)
  tbl_period <- period_meta |>
    dplyr::rowwise() |>
    dplyr::transmute(
      grp   = "Emissions by time period",
      item  = dplyr::coalesce(.data$period_type, "No period"),
      years = ifelse(.data$year_start == .data$year_end, as.character(.data$year_start),
                     paste0(.data$year_start, "-", .data$year_end)),
      DF    = { k <- paste0("PA.", .data$period_no, ".DF"); if (k %in% keys_out) unname(m0[k]) else 0 },
      DG    = { k <- paste0("PA.", .data$period_no, ".DG"); if (k %in% keys_out) unname(m0[k]) else 0 },
      total = stat_of(paste0("PN.", .data$period_no))$mean,
      U_pct = stat_of(paste0("PN.", .data$period_no))$U
    ) |>
    dplyr::ungroup()

  tbl_summary <- ari_combi |>
    dplyr::transmute(
      grp   = "Reference, monitoring & emission reductions",
      item  = dplyr::case_when(
        stringr::str_detect(.data$period_type, "^ER-") ~ paste0("Reduction ",  stringr::str_remove(.data$period_type, "^ER-")),
        stringr::str_detect(.data$period_type, "^E-")  ~ paste0("Emissions ",  stringr::str_remove(.data$period_type, "^E-")),
        TRUE                                           ~ paste0("Reference level (", .data$period_type, ")")
      ),
      years = NA_character_, DF = NA_real_, DG = NA_real_,
      total = .data$E, U_pct = .data$E_U
    )

  emissions_table <- dplyr::bind_rows(tbl_period, tbl_summary)

  list(
    ER = ari_combi,
    emissions = out_combi,
    gg_emissions = out_gg,
    emissions_table = emissions_table
  )
}
