#' Check that the app's the input data has matching land uses and correct carbon pools and associated factors.
#'
#' @description TBD
#'
#'
#' @param .ad Activity Data input table for the shiny app (AD_lu_transitions)
#' @param .cs Carbon Stock input table for the shiny app (c_stock)
#'
#' @return A dataframe with TRUE or FALSE for each flag, (TRUE meaning the flag is raised and an issue was detected).
#'
#'
#' @examples
#' library(mocaredd)
#' library(readxl)
#'
#' cs <- read_xlsx(system.file("extdata/example1.xlsx", package = "mocaredd"), sheet = "c_stock", na = "NA")
#' ad <- read_xlsx(system.file("extdata/example1.xlsx", package = "mocaredd"), sheet = "AD_lu_transitions", na = "NA")
#'
#' fct_check_data(.ad = ad, .cs = cs)
#'
#' @export
fct_check_data <- function(.ad, .cs){

  ## Unique IDs
  flag_trans_id <- !(length(unique(.ad$trans_id)) == nrow(.ad))
  flag_c_id     <- !(length(unique(.cs$c_id)) == nrow(.cs))

  ## .cs conformity
  flag_pool <- !all(unique(.cs$c_pool) %in% c("AGB", "BGB", "DW", "LI", "SOC", "ALL", "CF", "RS"))

  ## V1.0, can only handle one type of unit
  # flag_unit <- unique(.cs$c_unit) %in% c("DM", "C", "CO2") |> all()

  ## AD conformity
  flag_acti <- !all(unique(.ad$redd_activity) %in% c("DF", "DG", "E", "E_AF", "E_RE"))

  ## Matching landuse
  flag_lu_no <- !(length(unique(c(.ad$lu_initial_id, .ad$lu_final_id))) == length(unique(.cs$lu_id)))

  data.frame(
    flag_trans_id = flag_trans_id,
    flag_c_id = flag_c_id,
    flag_pool = flag_pool,
    # flag_unit = flag_unit,
    flag_acti = flag_acti,
    flag_lu_no = flag_lu_no
    )

}
