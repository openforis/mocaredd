#' Make the carbon stock uncertainty formula of a land use (IPCC Approach 1)
#'
#' @description Companion of \code{fct_make_formula2()}: same inputs, returns the
#'              formula of the standard error of the carbon stock, built from the
#'              terms of \code{fct_make_formula2()} with the IPCC Approach 1 rules
#'              (IPCC 2006, Vol. 1, Ch. 3, Eq. 3.1 and 3.2):
#'              - product: \code{x * y * sqrt((x_se / x)^2 + (y_se / y)^2)},
#'              - sum: \code{sqrt(x_se^2 + y_se^2)}.
#'
#'              Composite factors are resolved first:
#'              - \code{(1 + RS)}: standard error \code{RS_se},
#'              - \code{(AGB + BGB)}: standard error \code{sqrt(AGB_se^2 + BGB_se^2)}.
#'
#'              Evaluate it with each element and its standard error as
#'              \code{<element>} and \code{<element>_se} (e.g. \code{AGB}, \code{AGB_se},
#'              \code{CF}, \code{CF_se}). Terms are assumed independent. A factor
#'              equal to 0 in a product returns NaN.
#'
#' @inheritParams fct_make_formula2
#'
#' @return A character value with the standard error formula of the carbon stock.
#'
#' @examples
#' fct_make_formula_U(.c_el = c("AGB", "RS"), .c_unit = "DM")
#' #> "AGB * (1 + RS) * CF * sqrt((AGB_se / AGB)^2 + (RS_se / (1 + RS))^2 + (CF_se / CF)^2)"
#'
#' fct_make_formula_U(.c_el = c("AGB", "RS", "DW"), .c_unit = c("DM", NA, "C"), .version = 2)
#'
#' ## Evaluate
#' env <- list(AGB = 250, AGB_se = 25, RS = 0.24, RS_se = 0.05, CF = 0.47, CF_se = 0.02)
#' eval(parse(text = fct_make_formula_U(c("AGB", "RS"), "DM")), env)
#'
#' @export
fct_make_formula_U <- function(.c_el, .c_unit, .version = 1){

  ## Carbon stock formula, composite factors as single placeholders
  c_form <- fct_make_formula2(.c_el = .c_el, .c_unit = .c_unit, .version = .version)

  if (c_form == "0") return("0")

  ## Value and standard error of each factor
  fac_value <- c(RS1 = "(1 + RS)", AGB_BGB = "(AGB + BGB)")
  fac_se    <- c(RS1 = "RS_se",    AGB_BGB = "sqrt(AGB_se^2 + BGB_se^2)")

  terms <- c_form |>
    stringr::str_replace_all(stringr::fixed("(1 + RS)"), "RS1") |>
    stringr::str_replace_all(stringr::fixed("(AGB + BGB)"), "AGB_BGB") |>
    stringr::str_split(stringr::fixed(" + ")) |>
    unlist()

  ## Product rule within each term
  terms_se <- terms |>
    stringr::str_split(stringr::fixed(" * ")) |>
    purrr::map_chr(function(fac) {
      value <- ifelse(fac %in% names(fac_value), fac_value[fac], fac)
      se    <- ifelse(fac %in% names(fac_se),    fac_se[fac],    paste0(fac, "_se"))
      if (length(fac) == 1) return(se)
      paste0(
        paste(value, collapse = " * "), " * sqrt(",
        paste0("(", se, " / ", value, ")^2", collapse = " + "), ")"
      )
    })

  ## Sum rule across terms
  if (length(terms_se) == 1) return(terms_se)

  paste0("sqrt(", paste0("(", terms_se, ")^2", collapse = " + "), ")")
}
