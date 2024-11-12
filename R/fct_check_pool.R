#' Check which Carbon pool and what units are in the carbon stock data.
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
#' @param .c_lu Subset of the Carbon Stock input table (c_stock) for a unique land use
#' @param .c_unit Unit used to report AGB (and BGB if included), from the shiny app or template user inputs.
#' @param .c_fraction Value of carbon fraction, only if dry matter reported
#'
#' @return A dataframe with TRUE or FALSE for each pool and factor in the app's carbon stock input data for a given land use.
#'
#' @importFrom dplyr pull
#'
#' @examples
#' library(mocaredd)
#' library(readxl)
#' library(dplyr)
#'
#' cs <- read_xlsx(
#'   path = system.file("extdata/example1.xlsx", package = "mocaredd"),
#'   sheet = "c_stocks",
#'   na = "NA"
#'   )
#' c_lu <- cs |> filter(lu_id == "ev_wet_closed")
#'
#' fct_check_pool(.c_lu = c_lu, .c_unit = "C", .c_fraction = NA)
#'
#' @export
fct_check_pool <- function(.c_lu, .c_unit, .c_fraction){

  ## !! FOR TESTING ONLY
  # .c_lu    <- .cs |> filter(lu_id == "dg_ev_wet_closed")
  # .c_unit <- "C"
  # .c_fraction <- NA
  ## !!

  ## Get pools for the input data subset
  c_pool  <- .c_lu |> pull(c_pool) |> sort()
  lu_id   <- .c_lu |> pull(lu_id) |> unique()
  lu_name <- .c_lu |> pull(lu_name) |> unique()

  ## V1.0, DG ratio and pools used for degradation need to be added to C_stock table,
  ##       means duplicating the intact forest information but no way around for now since
  ##       the tool is based on C stock difference between land uses.
  ##       Hence commenting code below as not needed for now.
  # c_status <- .c_lu$c_status |> unique()
  # if (length(c_status) != 1) flag_cstatus <- T else flag_cstatus <- F
  # if (!(c_status %in% c("Intact", "Degraded", "Non-forest"))) flag_cstatus <- T else flag_cstatus <- F
  # if (flag_cstatus) return("c_status in carbon tab contains error, not unique for each land use or not in limited choice list: 'Intact','Degraded', 'Non-forest'")

  has_AG <- if ("AGB" %in% c_pool) T else F
  has_BG <- if ("BGB" %in% c_pool) T else F
  has_RS <- if ("RS"  %in% c_pool) T else F
  has_DW <- if ("DW"  %in% c_pool) T else F
  has_LI <- if ("LI"  %in% c_pool) T else F
  has_SO <- if ("SOC" %in% c_pool) T else F
  has_AL <- if ("ALL" %in% c_pool) T else F
  has_CF <- if (is.numeric(.c_fraction)) T else F
  has_DG <- if ("DG_ratio" %in% c_pool) T else F

  ## Checks
  if(has_BG & has_RS) {
    has_RS <- F
    message("trans_id: ", lu_name, ", has both BGB and RS in the data, using BGB only.")
  }

  if(.c_unit == "DM" & !has_CF) return("Cstock unit is dry matter (DM) but no carbon fraction provided")

  data.frame(
    lu_id   = lu_id,
    lu_name = lu_name,
    has_AG  = has_AG,
    has_BG  = has_BG,
    has_RS  = has_RS,
    has_DW  = has_DW,
    has_LI  = has_LI,
    has_SO  = has_SO,
    has_AL  = has_AL,
    has_CF  = has_CF,
    has_DG  = has_DG
  )

}
