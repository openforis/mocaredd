#' Check that the app's the input data has matching land uses and correct carbon pools and associated factors.
#'
#' @description TBD
#'
#'
#' @param .ad Activity Data input table for the shiny app (AD_lu_transitions)
#' @param .cs Carbon Stock input table for the shiny app (c_stocks)
#' @param .init initial list of coded variables for carbon pools and REDD+ activity
#'              to check the imported data against.
#'
#' @return A dataframe with TRUE or FALSE for each flag, (TRUE meaning the flag is raised and an issue was detected).
#'
#'
#' @examples
#' library(mocaredd)
#' library(readxl)
#'
#' cs <- read_xlsx(
#'   path = system.file("extdata/example1.xlsx", package = "mocaredd"),
#'    sheet = "c_stocks",
#'    na = "NA"
#'    )
#' ad <- read_xlsx(
#'   path = system.file("extdata/example1.xlsx", package = "mocaredd"),
#'   sheet = "AD_lu_transitions",
#'   na = "NA"
#'   )
#'
#'.init <- init <- list(
#'   c_pools = c("AGB", "BGB", "RS", "DW", "LI", "SOC", "ALL", "DG_ratio"),
#'   redd_acti = c("DF", "DG", "EN", "EN_AF", "EN_RE")
#' )
#'
#' fct_check_data(.ad = ad, .cs = cs, .init = init)
#'
#' @export
fct_check_data <- function(.ad, .cs, .init){

  ## Unique IDs
  pass_trans_id <- length(unique(.ad$trans_id)) == nrow(.ad)
  pass_c_id     <- length(unique(.cs$c_id)) == nrow(.cs)

  ## .cs conformity
  pass_pool <- all(unique(.cs$c_pool) %in% .init$c_pools)

  ## V1.0, can only handle one type of unit
  # pass_unit <- unique(.cs$c_unit) %in% c("DM", "C", "CO2") |> all()

  ## AD conformity
  pass_acti <- all(unique(.ad$redd_activity) %in% .init$redd_acti)

  ## Matching landuse
  pass_lu_no <- length(unique(c(.ad$lu_initial_id, .ad$lu_final_id))) == length(unique(.cs$lu_id))

  data.frame(
    pass_trans_id = pass_trans_id,
    pass_c_id = pass_c_id,
    pass_pool = pass_pool,
    pass_acti = pass_acti,
    pass_lu_no = pass_lu_no
    )

}
