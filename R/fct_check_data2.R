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

  ## +++ Check tables have at least the correct columns ----
  out$cols_usr_ok  <- .checklist$col_usr %in% names(.usr)
  out$cols_time_ok <- .checklist$col_time %in% names(.time)
  out$cols_ad_ok   <- .checklist$col_ad %in% names(.ad)
  out$cols_cs_ok   <- .checklist$col_cs %in% names(.cs)

  out$cols_ok <- all(out$cols_usr_ok, out$cols_time_ok, out$cols_ad_ok, out$cols_cs_ok)

  if (!out$cols_ok) {
    out$cols_usr_pb <- ifelse(out$cols_usr_ok, NULL, "user_inputs")
    out$cols_time_pb <- ifelse(out$cols_time_ok, NULL, "time_periods")
    out$cols_ad_pb <- ifelse(out$cols_ad_ok, NULL, "AD_lu_transitions")
    out$cols_cs_pb <- ifelse(out$cols_cs_ok, NULL, "c_stocks")
  }

  out$cols_pb <-

  ## +++ Check tables dimensions ----
  if (rv$checks$cols_ok) {
    rv$checks$size_ok <- all(
      nrow(.usr) == 1, ## usr has only one row
      nrow(.time) >= 2 ## at least one ref and one monitoring
    )
  }

  ## +++ Check data types are correct ----
  if (rv$checks$size_ok) {
    ## usr tab
    rv$checks$usr_datatypes_ok <- all(
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

    ## time tab
    rv$checks$time_datatypes_ok <- all()

    ## ad tab
    rv$checks$ad_datatypes_ok <- all()

    ## cs tab
    rv$checks$cs_datatypes_ok <- all()

    ## all
    rv$checks$datatypes_ok <- all(
      rv$checks$usr_datatypes_ok,
      rv$checks$time_datatypes_ok,
      rv$checks$ad_datatypes_ok,
      rv$checks$cs_datatypes_ok
    )

  }

  ## +++ Check category variables are correct ----
  if (rv$checks$datatypes_ok) {

    ## Get usr$dg_pool as vector
    ## ex. c("AGB", "BGB", "DW") %in% c("AGB", "BGB", "DW", "LI", "SOC", "ALL")
    dg_pool <- stringr::str_split(.usr$dg_pool, pattern = ",") |>
      purrr::map(stringr::str_trim) |>
      unlist()

    ### Check usr categorie variables
    rv$checks$usr_cats_ok <- all(
      .usr$c_unit %in% .checklist$cat_cunits,
      all(dg_pool %in% .checklist$cat_cpools)
    )

    ## Check time categorie variables
    rv$checks$time_cats_ok <- all()

    ## Check ad categorie variables
    rv$checks$ad_cats_ok <- all()

    ## Check cs categorie variables
    rv$checks$cs_cats_ok <- all()

    ## Combine
    rv$checks$cats_ok <- all(
      rv$checks$usr_cats_ok,
      rv$checks$time_cats_ok,
      rv$checks$ad_cats_ok,
      rv$checks$cs_cats_ok
    )

  }

  ## +++ Check Unique IDs ----
  rv$checks$ids_ok <- all()

  ## +++ Check matching variables ----
  rv$checks$matches_ok <- all()

  rv$checks$all_ok <- all(
    rv$checks$cols_ok,
    rv$checks$size_ok,
    rv$checks$datatypes_ok,
    rv$checks$cats_ok,
    rv$checks$ids_ok,
    rv$checks$matches_ok
  )
}
