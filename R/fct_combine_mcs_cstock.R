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
fct_combine_mcs_cstock <- function(.c_sub, .c_unit, .c_fraction = NA, .n_iter){

  ## !! FOR TESTING ONLY
  # .c_sub      <- cs |> filter(lu_id == "postdef_open") ## "dg_ev_wet_closed"
  # .c_unit     <- usr$c_unit
  # .c_fraction <- usr$c_fraction
  # .n_iter     <- 10
  ## !!

  c_pools <- unique(.c_sub$c_pool)
  c_check <- fct_check_pool(.c_lu = .c_sub, .c_unit = .c_unit, .c_fraction = .c_fraction)
  c_form  <- fct_make_formula(.c_check = c_check, .c_unit = .c_unit)

  ## Create named list with simulations for all pools
  SIMS <- map(c_pools, function(x){

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

    out <- as.data.frame(sims)
    names(out) <- x

    out

  }) |> list_cbind() |> as_tibble()
  ## End map()

  ## ADD CF if needed
  if (c_check$has_CF) {
    params <- .c_sub |> filter(c_pool == "CF")
    SIMS$CF <- fct_make_mcs(
      .n_iter = .n_iter,
      .pdf    = params$c_pdf,
      .mean   = params$c_value,
      .se     = params$c_se,
      .params = c(params$c_pdf_a, params$c_pdf_b, params$c_pdf_c),
      .trunc  = usr$trunc_pdf
    )
  }

  SIMS |>
    mutate(
      C_form = c_form,
      C_all = eval(parse(text=c_form), SIMS),
      sim_no = 1:.n_iter
    ) |>
    select(sim_no, everything())

}

