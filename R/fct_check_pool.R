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
#' cs    <- read_xlsx(system.file("extdata/example1.xlsx", package = "mocaredd"), sheet = "c_stock", na = "NA")
#' c_lu  <- cs |> filter(lu_id == LU_init)
#'
#' fct_check_pool(.c_lu = c_lu, .c_unit = "C")
#'
#' @export
fct_check_pool <- function(.c_lu, .c_unit){

  ## Get pools for the input data subset
  #.c_lu   <- .cs |> filter(lu_id == LU_init)
  c_pool  <- .c_lu |> pull(c_pool) |> sort()
  lu_id   <- .c_lu |> pull(lu_id) |> unique()
  lu_name <- .c_lu |> pull(lu_name) |> unique()

  has_AG <- if ("AGB" %in% c_pool) T else F
  has_BG <- if ("BGB" %in% c_pool) T else F
  has_RS <- if ("RS"  %in% c_pool) T else F
  has_DW <- if ("DW"  %in% c_pool) T else F
  has_LI <- if ("LI"  %in% c_pool) T else F
  has_SO <- if ("SOC" %in% c_pool) T else F
  has_AL <- if ("ALL" %in% c_pool) T else F
  has_CF <- if ("CF"  %in% c_pool) T else F

  ## Checks
  if(has_BG & has_RS) {
    has_RS <- F
    message("trans_id: ", lu_name, ", has both BGB and RS in the data, using BGB only.")
  }

  if(.c_unit == "DM" & !has_CF) {
    flag_miss_CF <- T
    message("Cstock unit is dry matter (DM) but no carbon fraction provided")
  } else {
    flag_miss_CF <- F
  }

  out <- data.frame(
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
    flag_miss_CF = flag_miss_CF
  )

}
