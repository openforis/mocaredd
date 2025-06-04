#' Check that the app's the input data has matching land uses and correct carbon pools and associated factors.
#'
#' @description The expected input carbon stock table for the tool contains carbon values of
#'              different carbon pools coded:
#'              - AGB for aboveground biomass,
#'              - BGB or RS (Root-to-Shoot ratio) for the belowground biomass,
#'              - DW for deadwood,
#'              - LI for litter and
#'              - SOC for soil organic carbon.
#'              All pools can be expressed in tons of carbon (C), except AGB and BGB
#'              which can also be expressed in ton of dry matter (DM) if a carbon fraction
#'              'CF' is provided.
#'
#'
#' @param .c_el Vector of carbon elements, inc. "A::
#' @param .c_unit "DM" or "C",
#'
#' @return A character value with the formula for calculating total carbon stock.
#'
#' @examples
#' library(mocaredd)
#'
#' c_el <- c("AGB", "RS", "DW")
#'
#' fct_make_formula(.c_el = c_el, .c_unit = "DM")
#'
#' @export
fct_make_formula <- function(.c_el, .c_unit){

  ## + Initiate formula
  c_eq <- c("(",  "AGB", " + ", "BGB", ")", " * ", "CF", " + ", "DW", " + ", "LI", " + ", "SOC")
  names(c_eq) <- c("cf_(", "AGB", "plus_bgb", "BGB", "cf_)", "times_cf", "CF", "plus_dw", "DW", "plus_li", "LI", "plus_soc", "SOC")

  ## + ALL pools combined - !!! ALL should only be possible for non-forest, which is usually expressed as ton C
  if (.c_el[1] == "ALL") return("ALL")
  # if (.c_el[1] == "ALL") {
  #   if(.c_unit == "C") c_eq <- "ALL" else c_eq <- "ALL * CF"
  #   return(c_eq)
  # }

  ## + Taylor formula
  if (.c_unit == "C"){
    c_eq <- c_eq[!(names(c_eq) %in% c("cf_(", "cf_)", "times_cf", "CF"))]
  }

  if (!"BGB" %in% .c_el & !"RS" %in% .c_el){
    c_eq <- c_eq[!(names(c_eq) %in% c("plus_bgb", "BGB", "cf_(", "cf_)"))]
  } else if (!"BGB" %in% .c_el & "RS" %in% .c_el) {
    c_eq["BGB"] <- "AGB * RS"
  }

  if (!'DW' %in% .c_el){
    c_eq <- c_eq[!(names(c_eq) %in% c("plus_dw", "DW"))]
  }

  if (!'LI' %in% .c_el){
    c_eq <- c_eq[!(names(c_eq) %in% c("plus_li", "LI"))]
  }

  if (!'SOC' %in% .c_el){
    c_eq <- c_eq[!(names(c_eq) %in% c("plus_soc", "SOC"))]
  }

  ## Output
  paste(c_eq, collapse = "")

}

## OLD VERSION

# fct_make_formula <- function(.c_check, .c_unit){
#
#   c_eq <- c("(",  "AGB", " + ", "BGB", ")", " * ", "CF", " + ", "DW", " + ", "LI", " + ", "SOC")
#   names(c_eq) <- c("cf_(", "AGB", "plus_bgb", "BGB", "cf_)", "times_cf", "CF", "plus_dw", "DW", "plus_li", "LI", "plus_soc", "SOC")
#
#   ## Handle dg_ratio
#   if (.c_check$has_DG) return("DG_ratio")
#
#   ## Handle C unit
#   if (.c_unit == "DM"){
#     c_eq1 <- c_eq
#   } else if (.c_unit == "C") {
#     c_eq1 <- c_eq[!(names(c_eq) %in% c("cf_(", "cf_)", "times_cf", "CF"))]
#     # V1.0 CO2 not allowed, just a constant value and nobody reporting directly CO2
#     # } else if (.c_unit == "CO2") {
#     #   c_eq_out <- c_eq[!(names(c_eq) %in% c("cf_(", "cf_)", "times_cf", "CF", "mol_(", "mol_)", "times_mol", "mol"))]
#   } else {
#     return("Wrong unit for carbon, should be either 'DM' or 'C'.")
#   }
#
#   ## Handle pools
#   c_eq_out <- c_eq1
#
#   if (.c_check$has_RS) {
#     c_eq_out["BGB"] <- "AGB * RS"
#   } else if (!.c_check$has_BG){
#     c_eq_out <- c_eq_out[!(names(c_eq_out) %in% c("plus_bgb", "BGB"))]
#   }
#
#   if (!.c_check$has_DW){
#     c_eq_out <- c_eq_out[!(names(c_eq_out) %in% c("plus_dw", "DW"))]
#   }
#
#   if (!.c_check$has_LI){
#     c_eq_out <- c_eq_out[!(names(c_eq_out) %in% c("plus_li", "LI"))]
#   }
#
#   if (!.c_check$has_SO){
#     c_eq_out <- c_eq_out[!(names(c_eq_out) %in% c("plus_soc", "SOC"))]
#   }
#
#   if (.c_check$has_AL){
#     c_eq_out <- "ALL"
#   }
#
#   ## Output
#   paste(c_eq_out, collapse = "")
#
# }
