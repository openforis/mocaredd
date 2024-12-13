#' Generate Monte Carlo Simulations and calculate carbon stock for all pools
#'
#' @description TBD
#'
#' @param .c_sub Subset with one land use from the carbon stock input table for the shiny app (c_stocks).
#' @param .usr User inputs' table for the shiny app (user_inputs). Contains the number
#'             of iterations of the MCS, carbon fraction if needed and if truncated PDFs
#'             should be used when necessary.
#'
#' @return
#' A data frame with N simulations of each carbon pool and associated factors and the resulting
#' total carbon stock in tCO2/ha.
#'
#' @importFrom rlang .data
#'
#' @examples
#' library(dplyr)
#' library(readxl)
#' library(mocaredd)
#'
#' path <- system.file("extdata/example1-4pools.xlsx", package = "mocaredd")
#'
#' usr <- read_xlsx(path = path, sheet = "user_inputs", na = "NA")
#' cs <- read_xlsx(path = path, sheet = "c_stocks", na = "NA")
#'
#' cs_clean <- cs |> filter(!(is.na(c_value) & is.na(c_pdf_a)))
#' c_sub <- cs_clean |> filter(lu_id == "ev_wet_closed")
#'
#' res <- fct_combine_mcs_cpools(.c_sub = c_sub, .usr = usr)
#'
#' hist(res$C_all)
#' round(median(res$C_all))
#'
#' @export
fct_combine_mcs_cpools <- function(.c_sub, .usr){

  ## !! FOR TESTING ONLY
  # .c_sub  <- cs |> dplyr::filter(lu_id == "P_deg") ## "dg_ev_wet_closed" ## "ALL_P_AGB"
  # .usr    <- usr
  ## !!

  .c_sub <- .c_sub |> dplyr::filter(!(is.na(.data$c_value) & is.na(.data$c_pdf_a)))

  c_pools <- unique(.c_sub$c_pool)
  c_check <- fct_check_pool(.c_lu = .c_sub, .c_unit = .usr$c_unit, .c_fraction = .usr$c_fraction)
  c_form  <- fct_make_formula(.c_check = c_check, .c_unit = .usr$c_unit)

  ## Create named list with simulations for all pools
  SIMS <- purrr::map(c_pools, function(x){

    ## !! FOR TESTING ONLY
    ## x = 'AGB'
    ## !!

    params <- .c_sub |> dplyr::filter(.data$c_pool == x)

    sims <- fct_make_mcs(
      .n_iter = .usr$n_iter,
      .pdf    = params$c_pdf,
      .mean   = params$c_value,
      .se     = params$c_se,
      .params = c(params$c_pdf_a, params$c_pdf_b, params$c_pdf_c),
      .trunc  = .usr$trunc_pdf
    )

    out <- as.data.frame(sims)
    names(out) <- x

    out

  }) |> purrr::list_cbind() |> dplyr::as_tibble()
  ## End map()

  ## ADD CF if needed
  if (c_check$has_CF & !("DG_ratio" %in% names(SIMS))) {
    SIMS$CF <- fct_make_mcs(
      .n_iter = .usr$n_iter,
      .pdf    = .usr$c_fraction_pdf,
      .mean   = .usr$c_fraction,
      .se     = .usr$c_fraction_se,
      #.params = c(params$c_pdf_a, params$c_pdf_b, params$c_pdf_c),
      .trunc  = .usr$trunc_pdf
    )
  }

  SIMS |>
    dplyr::mutate(
      C_form = c_form,
      C_all = eval(parse(text=c_form), SIMS),
      sim_no = 1:.usr$n_iter
    ) |>
    dplyr::select("sim_no", dplyr::everything())

}

