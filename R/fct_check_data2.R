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
#'
#' @return
#' A dataframe with TRUE or FALSE (TRUE if each check passes), and broad error locations
#' if FALSE.
#'
#' @importFrom rlang .data
#'
#' @examples
#' library(mocaredd)
#' library(readxl)
#'
#' path <- system.file("extdata/example1-4pools.xlsx", package = "mocaredd")
#'
#' cs <- read_xlsx(path = path, sheet = "c_stocks", na = "NA")
#' ad <- read_xlsx(path = path, sheet = "AD_lu_transitions", na = "NA")
#' usr <- read_xlsx(path = path, sheet = "user_inputs", na = "NA")
#' time <- read_xlsx(path = path, sheet = "time_periods", na = "NA")
#'
#' fct_check_data2(.ad = ad, .cs = cs, .usr = usr, .time = time)
#'
#' @export
fct_check_data2 <- function(.usr, .time, .ad, .cs){

  ## !! FOR TESTING ONLY
  # .usr <- usr
  # .time <- time
  # .ad <- ad
  # .cs <- cs
  ## !!

  ## Build checklist
  app_checklist <- list(
    xlsx_tabs  = c("user_inputs", "time_periods", "AD_lu_transitions", "c_stocks"),
    cat_cunits = c("DM", "C"),
    cat_cpools = c("AGB", "BGB", "DW", "LI", "SOC", "ALL"),
    cat_racti  = c("DF", "DG", "EN", "EN_AF", "EN_RE"),
    cat_ptype  = c("REF", "REF[0-9]", "MON", "MON[0-9]"),
    cat_pdf    = c("normal", "beta"),
    col_usr    = c("trunc_pdf", "n_iter", "ran_seed", "c_unit", "c_fraction", "c_fraction_se",
                   "c_fraction_pdf", "dg_ext", "dg_pool", "ad_annual", "conf_level"),
    col_time   = c("period_no", "year_start", "year_end", "period_type"),
    col_ad     = c("trans_no",	"trans_id",	"trans_period",	"trans_placeholder",
                   "lu_initial_id", "lu_final_id", "trans_area", "trans_se",
                   "trans_pdf", "trans_pdf_a", "trans_pdf_b", "trans_pdf_c",
                   "lu_initial",	"lu_final", "redd_activity"),
    col_cs     = c("c_no", "c_id", "c_period", "c_element", "c_lu_id", "c_placeholder",
                   "c_value", "c_se", "c_pdf", "c_pdf_a",	"c_pdf_b", "c_pdf_c", "c_lu_name")
  )

  app_checklist$cat_c_elements <- c(app_checklist$cat_cpools, "RS", "DG_ratio", "C_all")


  tmp <- list()
  out <- list()

  ## Check 1. tables have at least the correct columns #########################
  tmp$cols_usr_ok  <- all(app_checklist$col_usr %in% names(.usr))
  tmp$cols_time_ok <- all(app_checklist$col_time %in% names(.time))
  tmp$cols_ad_ok   <- all(app_checklist$col_ad %in% names(.ad))
  tmp$cols_cs_ok   <- all(app_checklist$col_cs %in% names(.cs))

  out$cols_ok <- all(
    tmp$cols_usr_ok, tmp$cols_time_ok, tmp$cols_ad_ok, tmp$cols_cs_ok
    )

  if (out$cols_ok) {
    out$cols_pb <- NULL
  } else {
    if (tmp$cols_usr_ok)  tmp$cols_usr_pb  <- NULL else tmp$cols_usr_pb  <- "user_inputs"
    if (tmp$cols_time_ok) tmp$cols_time_pb <- NULL else tmp$cols_time_pb <- "time_periods"
    if (tmp$cols_ad_ok)   tmp$cols_ad_pb   <- NULL else tmp$cols_ad_pb   <- "AD_lu_transitions"
    if (tmp$cols_cs_ok)   tmp$cols_cs_pb   <- NULL else tmp$cols_cs_pb   <- "c_stocks"

    out$cols_pb <- c(tmp$cols_usr_pb, tmp$cols_time_pb, tmp$cols_ad_pb, tmp$cols_cs_pb)
  }

  ## Check 2. tables dimensions ################################################
  tmp$size_usr_ok  <- nrow(.usr) == 1 ## usr has only one row
  tmp$size_time_ok <- nrow(.time) >= 2 ## at least one ref and one monitoring
  tmp$size_ad_ok   <- nrow(.ad) >= 2 ## at least one lu transition for ref and monitoring
  tmp$size_cs_ok   <- nrow(.cs) >= 2 ## at least one initial and one final cstock of one pool

  out$size_ok <- all(
    out$size_usr_ok, out$size_time_ok, out$size_ad_ok, out$size_cs_ok
  )

  if (out$size_ok) {
    out$size_pb <- NULL
  } else {
    if (tmp$size_usr_ok)  tmp$size_usr_pb  <- NULL else tmp$size_usr_pb  <- "user_inputs"
    if (tmp$size_time_ok) tmp$size_time_pb <- NULL else tmp$size_time_pb <- "time_periods"
    if (tmp$size_ad_ok)   tmp$size_ad_pb   <- NULL else tmp$size_ad_pb   <- "AD_lu_transitions"
    if (tmp$size_cs_ok)   tmp$size_cs_pb   <- NULL else tmp$size_cs_pb   <- "c_stocks"

    tmp$size_pb <- c(tmp$size_usr_pb, tmp$size_time_pb, tmp$size_ad_pb, tmp$size_cs_pb)
  }


  ## Check 3. data types are correct ###########################################
  ## - usr tab
  tmp$datatypes_usr_ok <- all(
    is.logical(.usr$trunc_pdf),
    is.numeric(.usr$n_iter),
    is.numeric(.usr$ran_seed) | is.logical(.usr$ran_seed),
    is.character(.usr$c_unit),
    #is.numeric(.usr$c_fraction), ## !!! C_fraction needs mean, sd if not NA, maybe better to have it in Cstock table
    is.character(.usr$dg_pool) | is.logical(.usr$dg_pool),
    #is.logical(.usr$dg_expool),
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
    is.character(.cs$c_period),
    is.character(.cs$c_lu_id),
    is.character(.cs$c_lu_name),
    is.character(.cs$c_element),
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
    out$datatypes_pb <- NULL
  } else {
    if (tmp$datatypes_usr_ok)  tmp$datatypes_usr_pb  <- NULL else tmp$datatypes_usr_pb  <- "user_inputs"
    if (tmp$datatypes_time_ok) tmp$datatypes_time_pb <- NULL else tmp$datatypes_time_pb <- "time_periods"
    if (tmp$datatypes_ad_ok)   tmp$datatypes_ad_pb   <- NULL else tmp$datatypes_ad_pb   <- "AD_lu_transitions"
    if (tmp$datatypes_cs_ok)   tmp$datatypes_cs_pb   <- NULL else tmp$datatypes_cs_pb   <- "c_stocks"

    out$datatypes_pb <- c(tmp$datatypes_usr_pb, tmp$datatypes_time_pb, tmp$datatypes_ad_pb, tmp$datatypes_cs_pb)
  }

  ## Check 4. category variables are correct ###################################
  ## - usr
  ## Get usr$dg_pool as vector
  ## ex. c("AGB", "BGB", "DW") %in% c("AGB", "BGB", "DW", "LI", "SOC", "ALL")
  dg_pool <- stringr::str_split(.usr$dg_pool, pattern = ",") |>
    purrr::map(stringr::str_trim) |>
    unlist()

  tmp$cats_usr_ok <- all(
    .usr$c_unit %in% app_checklist$cat_cunits,
    all(dg_pool %in% app_checklist$cat_c_elements)
  )

  ## - time
  ptype_pattern <- paste0(app_checklist$cat_ptype, collapse = "|")

  tmp$cats_time_ok <- all(
     stringr::str_detect(unique(.time$period_type), pattern = ptype_pattern)
  )

  ## - ad
  tmp$cats_ad_ok <- all(
    unique(.ad$redd_activity) %in% app_checklist$cat_racti,
    unique(.ad$trans_pdf) %in% app_checklist$cat_pdf
  )

  ## - cs
  tmp$cats_cs_ok <- all(
    unique(.cs$c_element) %in% app_checklist$cat_c_elements,
    unique(.cs$c_pdf) %in% app_checklist$cat_pdf
  )

  ## - Combine
  out$cats_ok <- all(
    tmp$cats_usr_ok,
    tmp$cats_time_ok,
    tmp$cats_ad_ok,
    tmp$cats_cs_ok
  )

  if (out$cats_ok) {
    out$cats_pb <- NULL
  } else {
    if (tmp$cats_usr_ok)  tmp$cats_usr_pb  <- NULL else tmp$cats_usr_pb  <- "user_inputs"
    if (tmp$cats_time_ok) tmp$cats_time_pb <- NULL else tmp$cats_time_pb <- "time_periods"
    if (tmp$cats_ad_ok)   tmp$cats_ad_pb   <- NULL else tmp$cats_ad_pb   <- "AD_lu_transitions"
    if (tmp$cats_cs_ok)   tmp$cats_cs_pb   <- NULL else tmp$cats_cs_pb   <- "c_stocks"

    out$cats_pb <- c(tmp$cats_usr_pb, tmp$cats_time_pb, tmp$cats_ad_pb, tmp$cats_cs_pb)
  }


  ## Check 5. Check Unique IDs #################################################

  tmp$ids_time_ok <- nrow(.time) == length(unique(.time$period_no))
  tmp$ids_ad_ok   <- nrow(.ad) == length(unique(.ad$trans_id))
  tmp$ids_cs_ok   <- nrow(.cs) == length(unique(.cs$c_id))

  out$ids_ok <- all(tmp$ids_time_ok, tmp$ids_ad_ok, tmp$ids_ad_ok)

  if (out$cats_ok) {
    out$ids_pb <- NULL
  } else {
    if (tmp$ids_time_ok) tmp$ids_time_pb <- NULL else tmp$ids_time_pb <- "time_periods"
    if (tmp$ids_ad_ok)   tmp$ids_ad_pb   <- NULL else tmp$ids_ad_pb   <- "AD_lu_transitions"
    if (tmp$ids_cs_ok)   tmp$ids_cs_pb   <- NULL else tmp$ids_cs_pb   <- "c_stocks"

    out$ids_pb <- c(tmp$ids_time_pb, tmp$ids_ad_pb, tmp$ids_cs_pb)
  }


  ## Check 6. Check matching and logical interactions ##########################

  ## - Period matching exactly between tables
  tmp$match_period_ad_ok <- all(sort(unique(.ad$trans_period)) == sort(unique(.time$period_no)))

  cs_period <- .cs |> dplyr::filter(.data$c_period != "ALL")
  if (nrow(cs_period) > 0) {
    tmp$match_period_cs_ok <- all(sort(unique(.cs$c_period)) == sort(unique(.time$period_no)))
  } else {
    tmp$match_period_cs_ok <- TRUE
  }

  ## - LU matching between tables
  lu_ad <- sort(c(unique(.ad$lu_initial_id), unique(.ad$lu_final_id)))
  lu_cs <- sort(unique(.cs$c_lu_id))

  tmp$match_lu_ok <- all(lu_ad %in% lu_cs)

  ## - At least one ref and one monitoring period
  nb_ref <- .time |> dplyr::filter(stringr::str_detect(.data$period_type, pattern = "REF|REF[0-9]"))
  nb_mon <- .time |> dplyr::filter(stringr::str_detect(.data$period_type, pattern = "MON|MON[0-9]"))

  tmp$match_ref_ok <- nrow(nb_ref) > 0
  tmp$match_mon_ok <- nrow(nb_mon) > 0

  ## - CF if DM
  tmp$match_dm_ok <- (.usr$c_unit == "DM" & is.numeric(.usr$c_fraction)) | .usr$c_unit == "C"

  ## - DEG ext working
  if (!is.na(.usr$dg_ext) & "DG_ratio" %in% unique(.cs$c_element)) {
    dg_lu <- .cs |>
      dplyr::filter(.data$c_element == "DG_ratio") |>
      dplyr::pull("c_lu_id") |>
      stringr::str_remove(pattern = .usr$dg_ext)

    tmp$match_dg_ok <- all(dg_lu %in% unique(.cs$c_lu_id))

  } else if (is.na(.usr$dg_ext) & "DG_ratio" %in% unique(.cs$c_element)) {
    tmp$match_dg_ok <- FALSE
  } else {
    tmp$match_dg_ok <- FALSE
  }

  ## - DG method: either (1) dg_ratio applied to all pools, (2) dg_ratio applied to some pools other kept intact (dg_expool == T), (3) diff in Cstocks.
  ## NOT IMPLEMENTED YET

  ## - Combine
  out$matches_ok <- all(
    tmp$match_period_ad_ok,
    tmp$match_period_cs_ok,
    tmp$match_lu_ok,
    tmp$match_ref_ok,
    tmp$match_mon_ok,
    tmp$match_dm_ok,
    tmp$match_dg_ok
  )

  if (out$matches_ok) {
    out$ids_pb <- NULL
  } else {
    if (tmp$match_period_ad_ok) tmp$match_period_ad_pb <- NULL else tmp$match_period_ad_pb <- "period_no != trans_period"
    if (tmp$match_period_cs_ok) tmp$match_period_cs_pb <- NULL else tmp$match_period_cs_pb <- "period_no != c_period"
    if (tmp$match_lu_ok)        tmp$match_lu_pb        <- NULL else tmp$match_lu_pb        <- "land use mismatch between AD and CS"
    if (tmp$match_ref_ok)       tmp$match_ref_pb       <- NULL else tmp$match_ref_pb       <- "missing REF in time_periods"
    if (tmp$match_mon_ok)       tmp$match_mon_pb       <- NULL else tmp$match_mon_pb       <- "missing MON in time_periods"
    if (tmp$match_dm_ok)        tmp$match_dm_pb        <- NULL else tmp$match_dm_pb        <- "user_inputs c_units is 'DM' but c_fraction missing"
    if (tmp$match_dg_ok)        tmp$match_dg_pb        <- NULL else tmp$match_dg_pb        <- "missing dg_ext for DG_ratio or mismatch with dg_ext and AD"
    out$matches_pb <- c(tmp$match_period_ad_pb, tmp$match_period_cs_pb, tmp$match_lu_pb, tmp$match_ref_pb, tmp$match_mon_pb, tmp$match_dm_pb, tmp$match_dg_pb)

  }

  ## 7. combine all
  out$all_ok <- all(
    out$cols_ok,
    out$size_ok,
    out$datatypes_ok,
    out$cats_ok,
    out$ids_ok,
    out$matches_ok
  )

  ## Return results as 1st level elements for checks and 2nd level for problems
  checks <- out[stringr::str_detect(names(out), pattern = "_ok")]
  checks$pbs  <- out[stringr::str_detect(names(out), pattern = "_pb")]
  checks

}
