#' Check that the app's the input data has matching land uses and correct carbon pools and associated factors.
#'
#' @description The expected input carbon stock table for the tool contains carbon values of
#'              different carbon pools coded:
#'              - AGB for aboveground biomass,
#'              - BGB or RS (Root-to-Shoot ratio) for the belowground biomass,
#'              - DW for deadwood,
#'              - LI for litter and
#'              - SOC for soil organic carbon.
#'              All pools can be expressed in tons of Carbon (C) or CO2.
#'              AGB and BGB can also be expressed in ton of dry matter (DM), then a carbon fraction (CF) is also needed.
#'
#'
#' @param .c_check output of fct_check_pool()
#' @param .c_unit "DM" or "C",
#'
#' @return A character value with the formula for calculating total carbon stock for a specific land use.
#'
#' @examples
#' library(mocaredd)
#' library(readxl)
#' library(dplyr)
#'
#' cs    <- read_xlsx(system.file("extdata/example1.xlsx", package = "mocaredd"), sheet = "c_stock", na = "NA")
#' c_lu  <- cs |> filter(lu_id == LU_init)
#'
#' c_check <- fct_check_pool(.c_lu = c_lu, .c_unit = "C")
#'
#' fct_make_formula(.c_check = c_check, .c_unit = "C")
#'
#' @export
fct_make_formula <- function(.c_check, .c_unit){

  c_eq <- c("(", "(",  "AGB", " + ", "BGB", ")", " * ", "CF", " + ", "DW", " + ", "LI", " + ", "SOC", ")", " * ", "44/12")
  names(c_eq) <- c("mol_(", "cf_(", "AGB", "plus_bgb", "BGB", "cf_)", "times_cf", "CF", "plus_dw", "DW", "plus_li", "LI", "plus_soc", "SOC", "mol_)", "times_mol", "mol")

  ## Handle C unit
  if (.c_unit == "DM"){
    c_eq_out <- c_eq
  } else if (.c_unit == "C") {
    c_eq_out <- c_eq[!(names(c_eq) %in% c("cf_(", "cf_)", "times_cf", "CF"))]
    # V1.0 CO2 not allowed, just a constant value and nobody reporting directly CO2
    # } else if (.c_unit == "CO2") {
    #   c_eq_out <- c_eq[!(names(c_eq) %in% c("cf_(", "cf_)", "times_cf", "CF", "mol_(", "mol_)", "times_mol", "mol"))]
  }

  ## Handle pools
  if (.c_check$has_RS) {
    c_eq_out["BGB"] <- "AGB*RS"
  } else if (!.c_check$has_BG){
    c_eq_out <- c_eq_out[!(names(c_eq_out) %in% c("plus_bgb", "BGB"))]
  }

  if (!.c_check$has_DW){
    c_eq_out <- c_eq_out[!(names(c_eq_out) %in% c("plus_dw", "DW"))]
  }

  if (!.c_check$has_LI){
    c_eq_out <- c_eq_out[!(names(c_eq_out) %in% c("plus_li", "LI"))]
  }

  if (!.c_check$has_SO){
    c_eq_out <- c_eq_out[!(names(c_eq_out) %in% c("plus_soc", "SOC"))]
  }

  ## Output
  paste(c_eq_out, collapse = "")

}
