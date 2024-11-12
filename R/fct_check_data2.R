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
#'   xlsx_tabs  = c("user_inputs", "time_periods", "AD_lu_transitions", "c_stocks"),
#'   cat_cunits = c("DM", "C"),
#'   cat_cpools = c("AGB", "BGB", "DW", "LI", "SOC", "ALL"),
#'   cat_racti  = c("DF", "DG", "EN", "EN_AF", "EN_RE"),
#'   cat_ptype  = c("REF", "REF[0-9]", "MON", "MON[0-9]"),
#'   cat_pdf    = c("normal", "beta"),
#'   col_usr    = c("trunc_pdf", "n_iter", "ran_seed", "c_unit", "c_fraction", "dg_pool", "dg_expool", "ad_annual", "conf_level"),
#'   col_time   = c("period_no", "year_start", "year_end", "period_type"),
#'   col_ad     = c("trans_no",	"trans_id",	"trans_period",	"redd_activity", "lu_initial_id", "lu_initial",	"lu_final_id", "lu_final", "trans_area", "trans_se", "trans_pdf", "trans_pdf_a", "trans_pdf_b", "trans_pdf_c"),
#'   col_cs     = c("c_no",	"c_id",	"lu_id", "lu_name",	"c_pool",	"c_value",	"c_se",	"c_pdf",	"c_pdf_a",	"c_pdf_b",	"c_pdf_c")
#' )
#'
#' app_checklist$cat_cpools_all <- c(app_checklist$c_pools, "RS", "DG_ratio")
#'
#' fct_check_data2(.ad = ad, .cs = cs, .usr = usr, .time = time, .checklist = app_checklist)
#'
#' @export
fct_check_data2 <- function(.ad, .cs, .usr, .time, .checklist){

  tmp <- list()
  out <- list()

  ## Check 1. tables have at least the correct columns
  tmp$cols_usr_ok  <- all(.checklist$col_usr %in% names(.usr))
  tmp$cols_time_ok <- all(.checklist$col_time %in% names(.time))
  tmp$cols_ad_ok   <- all(.checklist$col_ad %in% names(.ad))
  tmp$cols_cs_ok   <- all(.checklist$col_cs %in% names(.cs))

  out$cols_ok <- all(
    tmp$cols_usr_ok, tmp$cols_time_ok, tmp$cols_ad_ok, tmp$cols_cs_ok
    )

  if (out$cols_ok) {
    out$cols_pb <- ""
  } else {
    tmp$cols_usr_pb  <- ifelse(tmp$cols_usr_ok, NULL, "user_inputs")
    tmp$cols_time_pb <- ifelse(tmp$cols_time_ok, NULL, "time_periods")
    tmp$cols_ad_pb   <- ifelse(tmp$cols_ad_ok, NULL, "AD_lu_transitions")
    tmp$cols_cs_pb   <- ifelse(tmp$cols_cs_ok, NULL, "c_stocks")

    out$cols_pb <- c(tmp$cols_usr_pb, tmp$cols_time_pb, tmp$cols_ad_pb, tmp$cols_cs_pb)
  }

  ## Check 2. tables dimensions
  tmp$size_usr_ok <- nrow(.usr) == 1 ## usr has only one row
  tmp$size_time_ok <- nrow(.time) >= 2 ## at least one ref and one monitoring
  tmp$size_ad_ok <- nrow(.ad) >= 2 ## at least one lu transition for ref and monitoring
  tmp$size_cs_ok <- nrow(.cs) >= 2 ## at least one initial and one final cstock of one pool

  out$size_ok <- all(
    out$size_usr_ok, out$size_time_ok, out$size_ad_ok, out$size_cs_ok
  )

  if (out$size_ok) {
    out$size_pb <- ""
  } else {
    tmp$size_usr_pb  <- ifelse(tmp$size_usr_ok, NULL, "user_inputs")
    tmp$size_time_pb <- ifelse(tmp$size_time_ok, NULL, "time_periods")
    tmp$size_ad_pb   <- ifelse(tmp$size_ad_ok, NULL, "AD_lu_transitions")
    tmp$size_cs_pb   <- ifelse(tmp$size_cs_ok, NULL, "c_stocks")

    tmp$size_pb <- c(tmp$size_usr_pb, tmp$size_time_pb, tmp$size_ad_pb, tmp$size_cs_pb)
  }


  ## Check 3. data types are correct
  ## - usr tab
  tmp$datatypes_usr_ok <- all(
    is.logical(.usr$trunc_pdf),
    is.numeric(.usr$n_iter),
    is.numeric(.usr$ran_seed) | is.logical(.usr$ran_seed),
    is.character(.usr$c_unit),
    #is.numeric(.usr$c_fraction), ## !!! C_fraction needs mean, sd if not NA, maybe better to have it in Cstock table
    is.character(.usr$dg_pool) | is.logical(.usr$dg_pool),
    is.logical(.usr$dg_expool),
    is.logical(.usr$ad_annual),
    is.numeric(.usr$conf_level)
  )

  ## - time tab
  tmp$datatypes_time_ok <- all(
    is.character(.time$period_no),
    is.numeric(.time$year_start),
    is.numeric(.time$year_end),
    is.character(.time$period_type)
  )

  ## - ad tab
  tmp$datatypes_ad_ok <- all(
    is.numeric(.ad$trans_no),
    is.character(.ad$trans_id),
    is.character(.ad$trans_period),
    is.character(.ad$redd_activity),
    is.character(.ad$lu_initial_id),
    is.character(.ad$lu_initial),
    is.character(.ad$lu_final_id),
    is.character(.ad$lu_final),
    is.numeric(.ad$trans_area) | is.logical(.ad$trans_area),
    is.numeric(.ad$trans_se) | is.logical(.ad$trans_se),
    is.character(.ad$trans_pdf),
    is.numeric(.ad$trans_pdf_a) | is.logical(.ad$trans_pdf_a),
    is.numeric(.ad$trans_pdf_b) | is.logical(.ad$trans_pdf_b),
    is.numeric(.ad$trans_pdf_c) | is.logical(.ad$trans_pdf_c)
  )

  ## - cs tab
  tmp$datatypes_cs_ok <- all(
    is.numeric(.cs$c_no),
    is.character(.cs$c_id),
    is.character(.cs$lu_id),
    is.character(.cs$lu_name),
    is.character(.cs$c_pool),
    is.numeric(.cs$c_value) | is.logical(.cs$c_value),
    is.numeric(.cs$c_se) | is.logical(.cs$c_se),
    is.character(.cs$c_pdf),
    is.numeric(.cs$c_pdf_a) | is.logical(.cs$c_pdf_a),
    is.numeric(.cs$c_pdf_b) | is.logical(.cs$c_pdf_b),
    is.numeric(.cs$c_pdf_c) | is.logical(.cs$c_pdf_c)
  )

  ## - all
  out$datatypes_ok <- all(
    tmp$datatypes_usr_ok,
    tmp$datatypes_time_ok,
    tmp$datatypes_ad_ok,
    tmp$datatypes_cs_ok
  )

  if (out$datatypes_ok) {
    out$datatypes_pb <- ""
  } else {
    tmp$datatypes_usr_pb  <- ifelse(tmp$datatypes_usr_ok, NULL, "user_inputs")
    tmp$datatypes_time_pb <- ifelse(tmp$datatypes_time_ok, NULL, "time_periods")
    tmp$datatypes_ad_pb   <- ifelse(tmp$datatypes_ad_ok, NULL, "AD_lu_transitions")
    tmp$datatypes_cs_pb   <- ifelse(tmp$datatypes_cs_ok, NULL, "c_stocks")

    out$datatypes_pb <- c(tmp$datatypes_usr_pb, tmp$datatypes_time_pb, tmp$datatypes_ad_pb, tmp$datatypes_cs_pb)
  }

  ## Check 4. category variables are correct
  ## - usr
  ## Get usr$dg_pool as vector
  ## ex. c("AGB", "BGB", "DW") %in% c("AGB", "BGB", "DW", "LI", "SOC", "ALL")
  dg_pool <- stringr::str_split(.usr$dg_pool, pattern = ",") |>
    purrr::map(stringr::str_trim) |>
    unlist()

  tmp$cats_usr_ok <- all(
    .usr$c_unit %in% .checklist$cat_cunits,
    all(dg_pool %in% .checklist$cat_cpools)
  )

  ## - time
  ptype_pattern <- paste0(.checklist$cat_ptype, collapse = "|")

  tmp$cats_time_ok <- all(
     stringr::str_detect(unique(.time$period_type), pattern = ptype_pattern)
  )

  ## - ad
  tmp$cats_ad_ok <- all(
    unique(.ad$redd_activity) %in% .checklist$cat_racti,
    unique(.ad$trans_pdf) %in% .checklist$cat_pdf
  )

  ## - cs
  tmp$cats_cs_ok <- all(
    unique(.cs$c_pool) %in% .checklist$cat_cpools_all,
    unique(.cs$c_pdf) %in% .checklist$cat_pdf
  )

  ## - Combine
  out$cats_ok <- all(
    tmp$cats_usr_ok,
    tmp$cats_time_ok,
    tmp$cats_ad_ok,
    tmp$cats_cs_ok
  )

  if (out$cats_ok) {
    out$cats_pb <- ""
  } else {
    tmp$cats_usr_pb  <- ifelse(tmp$cats_usr_ok, NULL, "user_inputs")
    tmp$cats_time_pb <- ifelse(tmp$cats_time_ok, NULL, "time_periods")
    tmp$cats_ad_pb   <- ifelse(tmp$cats_ad_ok, NULL, "AD_lu_transitions")
    tmp$cats_cs_pb   <- ifelse(tmp$cats_cs_ok, NULL, "c_stocks")

    out$cats_pb <- c(tmp$cats_usr_pb, tmp$cats_time_pb, tmp$cats_ad_pb, tmp$cats_cs_pb)
  }


  ## 5. Check Unique IDs
  out$ids_ok <- all()

  ## 6. Check matching and logical interactions
  # nb_ref <- .time |> dplyr::filter(stringr::str_detect(period_type, pattern = "REF|REF[0-9]"))
  # nb_mon <- .time |> dplyr::filter(stringr::str_detect(period_type, pattern = "MON|MON[0-9]"))


  out$matches_ok <- all()

  ## - combine all
  out$all_ok <- all(
    out$cols_ok,
    out$size_ok,
    out$datatypes_ok,
    out$cats_ok,
    out$ids_ok,
    out$matches_ok
  )


}
