#' Make the carbon stock formula of a land use from its carbon elements
#'
#' @description The biomass part is written so each input appears once:
#'              - AGB + RS:  \code{AGB * (1 + RS)} (BGB derived from AGB),
#'              - AGB + BGB: \code{(AGB + BGB)},
#'              multiplied by \code{CF} when biomass is in dry matter (DM).
#'              DW, LI and SOC are added as separate terms.
#'
#'              The factored form makes the AGB/BGB dependence explicit, which
#'              matters when uncertainty is propagated term by term with IPCC
#'              Approach 1 rules.
#'
#' @param .c_el Vector of carbon elements, among "AGB", "BGB", "RS", "DW", "LI",
#'              "SOC", "ALL". Other elements (e.g. "DG_ratio") are ignored.
#' @param .c_unit Carbon unit. \code{.version = 1}: a single value ("DM" or "C")
#'              for the land use; CF applies to biomass (AGB, BGB) only.
#'              \code{.version = 2}: a vector aligned with \code{.c_el}, one unit per
#'              element; CF applies to each element in DM. RS takes the unit of AGB.
#' @param .version Template version, 1 (default) or 2.
#'
#' @return A character value with the carbon stock formula.
#'
#' @examples
#' fct_make_formula2(.c_el = c("AGB", "RS", "DW"), .c_unit = "DM")
#' #> "AGB * (1 + RS) * CF + DW"
#'
#' fct_make_formula2(.c_el = c("AGB", "RS", "DW"), .c_unit = c("DM", NA, "C"), .version = 2)
#' #> "AGB * (1 + RS) * CF + DW"
#'
#' @export
fct_make_formula2 <- function(.c_el, .c_unit, .version = 1){

  ## Is an element in dry matter (needs CF)?
  if (.version == 2) {
    unit  <- stats::setNames(.c_unit, .c_el)
    is_dm <- function(el) el %in% .c_el && !is.na(unit[[el]]) && unit[[el]] == "DM"
  } else {
    is_dm <- function(el) el %in% c("AGB", "BGB", "ALL") && isTRUE(.c_unit == "DM")
  }
  with_cf <- function(term, dm) if (dm) paste0(term, " * CF") else term

  ## ALL pools combined (usually non-forest, in C). v1 never applies CF to ALL.
  if ("ALL" %in% .c_el) return(with_cf("ALL", .version == 2 && is_dm("ALL")))

  has <- function(el) el %in% .c_el
  terms <- character(0)

  ## Biomass
  if (has("RS") && !has("AGB")) stop("RS requires AGB.")

  if (has("AGB") && has("BGB")) {
    if (is_dm("AGB") == is_dm("BGB")) {
      terms <- c(terms, with_cf("(AGB + BGB)", is_dm("AGB")))
    } else {
      terms <- c(terms, with_cf("AGB", is_dm("AGB")), with_cf("BGB", is_dm("BGB")))
    }
  } else if (has("AGB") && has("RS")) {
    terms <- c(terms, with_cf("AGB * (1 + RS)", is_dm("AGB")))
  } else if (has("AGB")) {
    terms <- c(terms, with_cf("AGB", is_dm("AGB")))
  } else if (has("BGB")) {
    terms <- c(terms, with_cf("BGB", is_dm("BGB")))
  }

  ## Other pools
  for (pool in c("DW", "LI", "SOC")) {
    if (has(pool)) terms <- c(terms, with_cf(pool, .version == 2 && is_dm(pool)))
  }

  ## Empty only for element-less land uses (e.g. DG_ratio only), overwritten later
  if (length(terms) == 0) return("0")

  paste(terms, collapse = " + ")
}
