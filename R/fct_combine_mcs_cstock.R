#' Generate Monte Carlo Simulations and calculate carbon stock for all pools
#'
#' @description TBD
#'
#' @param .cs Subset with one land use from the carbon stock input table for the shiny app (c_stock)
#'
#' @return A data frame with N simulations of each carbon pool and associated factors and the resulting
#'         total carbon stock in tCO2/ha
#'
#' @examples
#' library(mocaredd)
#' library(readxl)
#' library(dplyr)
#' library(ggplot2)
#'
#' cs <- read_xlsx(system.file("extdata/example1.xlsx", package = "mocaredd"), sheet = "c_stock", na = "NA")
#' c_sub <- cs |> filter(lu_id == "ev_wet_closed")
#' res <- fct_combine_mcs_cstock(.n_iter = 10000, .c_sub = c_sub, .c_unit = "C")
#' res
#'
#' @export
fct_combine_mcs_cstock <- function(.n_iter, .c_sub, .c_unit, .c_fraction = NA){

  ## !! FOR TESTING ONLY
  # .c_sub      <- .cs |> filter(lu_id == "dg_ev_wet_closed")
  # .c_unit     <- usr$c_unit
  # .c_fraction <- usr$c_fraction
  # .dg_ratio   <- usr$dg_ratio
  ## !!

  c_pools <- unique(.c_sub$c_pool)
  c_check <- fct_check_pool(.c_lu = .c_sub, .c_unit = .c_unit, .c_fraction = .c_fraction)
  c_form  <- fct_make_formula(.c_check = c_check, .c_unit = .c_unit)

  message("For land use: ", unique(.c_sub$lu_name), ", the carbon stock formula is: ", c_form)

  ## Create named list with simulations for all pools
  sims_pool <- map(c_pools, function(x){

    ## !! FOR TESTING ONLY
    ## x = 'AGB'
    ## !!

    params <- .c_sub |> filter(c_pool == x)

    sims <- fct_make_mcs(
      .n_iter = .n_iter,
      .pdf    = params$c_pdf,
      .mean   = params$c_value,
      .se     = params$c_se,
      .params = c(params$c_pdf_a, params$c_pdf_b, params$c_pdf_c),
      .trunc  = usr$trunc_pdf
    )

    as.data.frame(sims)

  })
  ## End map()

  # names(sims_pool) <- c_pools
  # str(sims_pool)

  sims <- sims_pool |> list_cbind()
  names(sims) <- c_pools
  as_tibble(sims)

  ## ADD CF if needed
  if (c_check$has_CF) {
    params <- .c_sub |> filter(c_pool == "CF")
    sims_pool$CF <- fct_make_mcs(
      .n_iter = .n_iter,
      .pdf    = params$c_pdf,
      .mean   = params$c_value,
      .se     = params$c_se,
      .params = c(params$c_pdf_a, params$c_pdf_b, params$c_pdf_c),
      .trunc  = usr$trunc_pdf
    )
  }

  ## Check
  str(sims_pool)

  # ## AGB initial
  # if (c_check$has_AG) {
  #   c_pool <- .c_sub |> filter(c_pool == "AGB")
  #   AGB <- fct_make_mcs(
  #     .n_iter = .n_iter,
  #     .pdf    = c_pool$c_pdf,
  #     .mean   = c_pool$c_value,
  #     .se     = c_pool$c_se,
  #     .params = c(c_pool$c_pdf_a, c_pool$c_pdf_b, c_pool$c_pdf_c),
  #     .trunc  = usr$trunc_pdf
  #     )
  # } else {
  #   AGB <- NULL
  # }
  # # median(AGB_i)
  # # c_pool$c_value
  #
  # ## BGB initial
  # if (c_check$has_BG) {
  #   c_pool <- .c_sub |> filter(c_pool == "BGB")
  #   BGB <- fct_make_mcs(
  #     .n_iter = .n_iter,
  #     .pdf    = c_pool$c_pdf,
  #     .mean   = c_pool$c_value,
  #     .se     = c_pool$c_se,
  #     .params = c(c_pool$c_pdf_a, c_pool$c_pdf_b, c_pool$c_pdf_c),
  #     .trunc  = usr$trunc_pdf
  #   )
  # } else {
  #   BGB <- NULL
  # }
  #
  # ## RS initial
  # if (c_check$has_RS) {
  #   c_pool <- .c_sub |> filter(c_pool == "RS")
  #   RS <- fct_make_mcs(
  #     .n_iter = .n_iter,
  #     .pdf    = c_pool$c_pdf,
  #     .mean   = c_pool$c_value,
  #     .se     = c_pool$c_se,
  #     .params = c(c_pool$c_pdf_a, c_pool$c_pdf_b, c_pool$c_pdf_c),
  #     .trunc  = usr$trunc_pdf
  #   )
  # } else {
  #   RS <- NULL
  # }
  #
  # ## CF initial
  # if (c_check$has_CF) {
  #   c_pool <- .c_sub |> filter(c_pool == "CF")
  #   CF <- fct_make_mcs(
  #     .n_iter = .n_iter,
  #     .pdf    = c_pool$c_pdf,
  #     .mean   = c_pool$c_value,
  #     .se     = c_pool$c_se,
  #     .params = c(c_pool$c_pdf_a, c_pool$c_pdf_b, c_pool$c_pdf_c),
  #     .trunc  = usr$trunc_pdf
  #   )
  # } else {
  #   CF <- NULL
  # }
  #
  # ## DW initial
  # if (c_check$has_DW) {
  #   c_pool <- .c_sub |> filter(c_pool == "DW")
  #   DW <- fct_make_mcs(.n_iter = .n_iter, .pdf = c_pool$c_pdf, .mean = c_pool$c_value, .se = c_pool$c_se, .trunc = usr$trunc_pdf)
  # } else {
  #   DW <- NULL
  # }
  #
  # ## LI initial
  # if (c_check$has_LI) {
  #   c_pool <- .c_sub |> filter(c_pool == "LI")
  #   LI <- fct_make_mcs(.n_iter = .n_iter, .pdf = c_pool$c_pdf, .mean = c_pool$c_value, .se = c_pool$c_se, .trunc = usr$trunc_pdf)
  # } else {
  #   LI <- NULL
  # }
  #
  # ## SOC initial
  # if (c_check$has_SO) {
  #   c_pool <- .c_sub |> filter(c_pool == "SOC")
  #   SOC <- fct_make_mcs(.n_iter = .n_iter, .pdf = c_pool$c_pdf, .mean = c_pool$c_value, .se = c_pool$c_se, .trunc = usr$trunc_pdf)
  # } else {
  #   SOC <- NULL
  # }
  #
  # ## ALL
  # if (c_check$has_AL) {
  #   c_pool <- .c_sub |> filter(c_pool == "ALL")
  #   C_all <- fct_make_mcs(.n_iter = .n_iter, .pdf = c_pool$c_pdf, .mean = c_pool$c_value, .se = c_pool$c_se, .trunc = usr$trunc_pdf)
  # } else {
  #   C_all <- NULL
  # }

  C_all <- cbind(AGB, BGB, RS, CF, DW, LI, SOC, C_all) |> as_tibble()

  C_all |> mutate(
    C_all = eval(parse(text=c_form), c_i),
    trans_id = ad_x$trans_id,
    redd_activity = ad_x$redd_activity,
    sim_no = 1:.n_iter
    ) |>
    select(sim_no, redd_activity, trans_id, everything())

}

