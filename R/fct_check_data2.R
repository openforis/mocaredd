#' Check that the app's input data meet the template criteria.
#'
#' @description
#' Tests 6 types of conformity with the app template to avoid code break during
#' analysis: (1) column names, (2) tables' size, (3) data types,
#' (4) categories names, (5) unique IDs and (6) matching key variables between tables.
#'
#' @param .ad Activity Data input table for the shiny app (AD_lu_transitions)
#' @param .cs Carbon Stock input table for the shiny app (c_stocks)
#' @param .usr User inputs' table for the shiny app (user_inputs)
#' @param .time the 'time' table from the tool input file (see template)
#' @param .checklist list of coded category variables used in the template.
#'
#' @return
#' A dataframe with TRUE or FALSE (TRUE if each check passes), and broad error locations
#' if FALSE.
#'
#' @examples
#' library(mocaredd)
#' library(readxl)
#'
#' cs <- read_xlsx(
#'   path = system.file("extdata/example1.xlsx", package = "mocaredd"),
#'   sheet = "c_stocks",
#'   na = "NA"
#'   )
#' ad <- read_xlsx(
#'   path = system.file("extdata/example1.xlsx", package = "mocaredd"),
#'   sheet = "AD_lu_transitions",
#'   na = "NA"
#'   )
#' usr <- read_xlsx(
#'   path = system.file("extdata/example1.xlsx", package = "mocaredd"),
#'   sheet = "user_inputs",
#'   na = "NA"
#'   )
#' time <- read_xlsx(
#'   path = system.file("extdata/example1.xlsx", package = "mocaredd"),
#'   sheet = "time_periods",
#'   na = "NA"
#'   )
#'
#' app_checklist <- list(
#'   xlsx_tabs  = c("user_inputs", "time_periods", "AD_lu_transitions", "c_stock"),
#'   cat_cunits = c("DM", "C"),
#'   cat_cpools = c("AGB", "BGB", "DW", "LI", "SOC", "ALL"),
#'   cat_racti  = c("DF", "DG", "EN", "EN_AF", "EN_RE"),
#'   cat_ptype  = c("REF", "M"),
#'   col_usr    = c("trunc_pdf", "n_iter", "ran_seed", "c_unit", "c_fraction", "dg_pool", "dg_expool", "ad_annual", "conf_level"),
#'   col_time   = c("period_no", "year_start", "year_end", "period_type"),
#'   col_ad     = c("trans_no",	"trans_id",	"trans_period",	"redd_activity", "lu_initial_id", "lu_initial",	"lu_final_id", "lu_final", "trans_area", "trans_se", "trans_pdf", "trans_pdf_a", "trans_pdf_b", "trans_pdf_c"),
#'   col_cs     = c("c_no",	"c_id",	"lu_id", "lu_name",	"c_pool",	"c_value",	"c_se",	"c_pdf",	"c_pdf_a",	"c_pdf_b",	"c_pdf_c")
#'   )
#'
#' fct_check_data(.ad = ad, .cs = cs, .usr = usr, .time = time, .checklist = app_checklist)
#'
#' @export
fct_check_data2 <- function(.ad, .cs, .usr, .time, .checklist){

  ## Check 1. tables have at least the correct columns
  out$cols_usr_ok  <- .checklist$col_usr %in% names(.usr)
  out$cols_time_ok <- .checklist$col_time %in% names(.time)
  out$cols_ad_ok   <- .checklist$col_ad %in% names(.ad)
  out$cols_cs_ok   <- .checklist$col_cs %in% names(.cs)

  out$cols_ok <- all(
    out$cols_usr_ok, out$cols_time_ok, out$cols_ad_ok, out$cols_cs_ok
    )

  if (out$cols_ok) {
    out$cols_pb <- NULL
  } else {
    out$cols_usr_pb  <- ifelse(out$cols_usr_ok, NULL, "user_inputs")
    out$cols_time_pb <- ifelse(out$cols_time_ok, NULL, "time_periods")
    out$cols_ad_pb   <- ifelse(out$cols_ad_ok, NULL, "AD_lu_transitions")
    out$cols_cs_pb   <- ifelse(out$cols_cs_ok, NULL, "c_stocks")

    out$cols_pb <- c(out$cols_usr_pb, out$cols_time_pb, out$cols_ad_pb, out$cols_cs_pb)
  }

  ## Check 2. tables dimensions
  out$size_usr_ok <- nrow(.usr) == 1 ## usr has only one row
  out$size_time_ok <- nrow(.time) >= 2 ## at least one ref and one monitoring
  out$size_ad_ok <- nrow(.ad) >= 2 ## at least one lu transition for ref and monitoring
  out$size_time_ok <- nrow(.cs) >= 2 ## at least one initial and one final cstock of one pool

  out$size_ok <- all(
    out$size_usr_ok, out$size_time_ok, out$size_ad_ok, out$size_time_ok
  )

  if (out$size_ok) {
    out$size_pb <- NULL
  } else {
    out$size_usr_pb  <- ifelse(out$size_usr_ok, NULL, "user_inputs")
    out$size_time_pb <- ifelse(out$size_time_ok, NULL, "time_periods")
    out$size_ad_pb   <- ifelse(out$size_ad_ok, NULL, "AD_lu_transitions")
    out$size_cs_pb   <- ifelse(out$size_cs_ok, NULL, "c_stocks")

    out$size_pb <- c(out$size_usr_pb, out$size_time_pb, out$size_ad_pb, out$size_cs_pb)
  }


  ## Check 3. data types are correct
  ## - usr tab
  out$usr_datatypes_ok <- all(
    is.logical(.usr$trunc_pdf),
    is.integer(.usr$n_iter),
    is.integer(.usr$ran_seed) | is.na(.usr$ran_seed),
    is.character(.usr$c_unit),
    #is.numeric(.usr$c_fraction), ## !!! C_fraction needs mean, sd if not NA, maybe better to have it in Cstock table
    is.character(.usr$dg_pool) | is.na(.usr$dg_pool),
    is.character(.usr$dg_expool) | is.na(.usr$dg_expool),
    is.logical(.usr$ad_annual),
    is.numeric(.usr$conf_level)
  )

  ## - time tab
  out$time_datatypes_ok <- all(
    is.character(period_no),
    is.integer(year_start),
    is.integer(year_end),
    is.character(period_type)
  )

  ## - ad tab
  out$ad_datatypes_ok <- all(
    is.integer(trans_no),
    is.character(trans_id),
    is.character(trans_period),
    is.character(redd_activity),
    is.character(lu_initial_id),
    is.character(lu_initial),
    is.character(lu_final_id),
    is.character(lu_final),
    is.numeric(trans_area), ## Should be is.integer() but might be too restrictive
    is.numeric(trans_se),
    is.character(trans_pdf),
    is.numeric(trans_pdf_a),
    is.numeric(trans_pdf_b),
    is.numeric(trans_pdf_c)
  )

  ## - cs tab
  out$cs_datatypes_ok <- all(
    is.integer(c_no),
    is.character(c_id),
    is.character(lu_id),
    is.character(lu_name),
    is.character(c_pool),
    is.numeric(c_value),
    is.numeric(c_se),
    is.character(c_pdf),
    is.numeric(c_pdf_a),
    is.numeric(c_pdf_b),
    is.numeric(c_pdf_c)
  )

  ## - all
  out$datatypes_ok <- all(
    out$usr_datatypes_ok,
    out$time_datatypes_ok,
    out$ad_datatypes_ok,
    out$cs_datatypes_ok
  )

  if (out$datatypes_ok) {
    out$datatypes_pb <- NULL
  } else {
    out$datatypes_usr_pb  <- ifelse(out$datatypes_usr_ok, NULL, "user_inputs")
    out$datatypes_time_pb <- ifelse(out$datatypes_time_ok, NULL, "time_periods")
    out$datatypes_ad_pb   <- ifelse(out$datatypes_ad_ok, NULL, "AD_lu_transitions")
    out$datatypes_cs_pb   <- ifelse(out$datatypes_cs_ok, NULL, "c_stocks")

    out$datatypes_pb <- c(out$datatypes_usr_pb, out$datatypes_time_pb, out$datatypes_ad_pb, out$datatypes_cs_pb)
  }

  ## Check 4. category variables are correct
  ## Get usr$dg_pool as vector
  ## ex. c("AGB", "BGB", "DW") %in% c("AGB", "BGB", "DW", "LI", "SOC", "ALL")
  dg_pool <- stringr::str_split(.usr$dg_pool, pattern = ",") |>
    purrr::map(stringr::str_trim) |>
    unlist()

  ### Check usr categorie variables
  out$usr_cats_ok <- all(
    .usr$c_unit %in% .checklist$cat_cunits,
    all(dg_pool %in% .checklist$cat_cpools)
  )

  ## Check time categorie variables
  out$time_cats_ok <- all()

  ## Check ad categorie variables
  out$ad_cats_ok <- all()

  ## Check cs categorie variables
  out$cs_cats_ok <- all()

  ## Combine
  out$cats_ok <- all(
    out$usr_cats_ok,
    out$time_cats_ok,
    out$ad_cats_ok,
    out$cs_cats_ok
  )

  ## +++ Check Unique IDs ----
  out$ids_ok <- all()

  ## +++ Check matching variables ----
  out$matches_ok <- all()

  out$all_ok <- all(
    out$cols_ok,
    out$size_ok,
    out$datatypes_ok,
    out$cats_ok,
    out$ids_ok,
    out$matches_ok
  )


}
