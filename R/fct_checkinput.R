#' Check and load the mocaredd input XLSX file
#'
#' @description
#' Loads an XLSX input file (e.g. from a Shiny \code{fileInput} widget), verifies
#' that all required sheets are present, reads the four data tables, and runs six
#' conformity checks:
#' \enumerate{
#'   \item Column names
#'   \item Table sizes
#'   \item Column data types
#'   \item Category variable values
#'   \item Unique IDs
#'   \item Cross-table matching and logical consistency
#' }
#'
#' Each check emits a \code{message()}, so results can be captured outside the function with
#' \code{withCallingHandlers(message = ...)} and displayed in a console-style UI
#' element. A \code{shinyWidgets} progress bar is advanced after each of the
#' \code{n_steps} steps (1 load + 6 checks).
#'
#' @section Server-side usage:
#' \preformatted{
#' log_lines <- character()
#' result <- withCallingHandlers(
#'   tryCatch(
#'     fct_checkinput(.path = path, .pb_session = session, .pb_id = "prog_allchecks"),
#'     error = function(e) {
#'       log_lines <<- c(log_lines, paste0("\u2717 ERROR: ", conditionMessage(e)))
#'       NULL
#'     }
#'   ),
#'   message = function(m) {
#'     log_lines <<- c(log_lines, trimws(conditionMessage(m)))
#'     invokeRestart("muffleMessage")
#'   }
#' )
#' rv$check_log <- log_lines
#' if (!is.null(result)) {
#'   rv$inputs$setup  <- result$data$setup
#'   rv$inputs$time   <- result$data$time
#'   rv$inputs$area   <- result$data$area
#'   rv$inputs$carbon <- result$data$carbon
#'   rv$checks$all_ok <- result$all_ok
#' }
#' }
#'
#' @param .path       Character. Path to the XLSX file (e.g. \code{input$load_xlsx$datapath}).
#' @param .pb_session Shiny session object used to update the progress bar. Pass \code{NULL}
#'                    (default) to skip progress bar updates (e.g. when calling outside Shiny).
#' @param .pb_id      Character. The \code{id} of the \code{shinyWidgets::progressBar()} to
#'                    update. Ignored when \code{.pb_session} is \code{NULL}.
#' @param .pb_max     Value between 0 and 100, default to 100. What should progress bar max be
#'                    after all checks are passed.
#' @param .minislow   NA or numeric. If numeric, add that value in seconds to system sleep to
#'                    make checks more visible in the console.
#'
#' @return A list with three elements:
#' \describe{
#'   \item{\code{all_ok}}{Logical. \code{TRUE} only if every check passed.}
#'   \item{\code{template_version}}{Integer, 1 or 2, the detected template version.
#'         Downstream calculation functions take this whole list as \code{.checked_data}
#'         and read \code{.checked_data$template_version} to know where to find the carbon
#'         unit and carbon fraction.}
#'   \item{\code{data}}{Named list with elements \code{setup}, \code{time}, \code{area},
#'         \code{carbon} (the four loaded tables as tibbles).}
#' }
#' The function throws an error (via \code{stop()}) only for unrecoverable failures
#' (unreadable file, missing sheets). All check outcomes are reported via \code{message()}.
#'
#' @importFrom rlang .data
#'
#' @examples
#' path <- system.file("extdata/example1-4pools.xlsx", package = "mocaredd")
#'
#' log_lines <- character()
#' result <- withCallingHandlers(
#'   fct_checkinput(.path = path),
#'   message = function(m) {
#'     log_lines <<- c(log_lines, trimws(conditionMessage(m)))
#'     invokeRestart("muffleMessage")
#'   }
#' )
#' cat(log_lines, sep = "\n")
#'
#' @export
fct_checkinput <- function(.path, .pb_session = NULL, .pb_id = NULL, .pb_max = 100, .minislow = NA) {


  ## !!! FOR TESTING ONLY
  # .path = system.file("extdata/example1-4pools.xlsx", package = "mocaredd")
  # .path = system.file("extdata/mocaredd-templatev2-4pools.xlsx", package = "mocaredd")
  # .path = system.file("extdata/mocaredd-templatev2-simple.xlsx", package = "mocaredd")
  # .path = system.file("extdata/test-simple-noCF.xlsx", package = "mocaredd")
  # .pb_id = NULL ; .pb_session = NULL ; .pb_max = 80 ; .minislow = NA
  ## !!!

  ##
  ## Constants #################################################################
  ##

  checklist <- list(
    tabs   = c("user_inputs", "time_periods", "AD_lu_transitions", "c_stocks"),
    tabsv2 = c("setup", "time", "area", "carbon"),
    cats   = list(
      c_unit = c("DM", "C", "ratio"),
      c_pool = c("AGB", "BGB", "DW", "LI", "SOC", "ALL"),
      r_acti = c("DF", "DG", "EN", "EN_AF", "EN_RE"),
      p_type = c("REF", "REF[0-9]", "MON", "MON[0-9]"),
      pdf    = c("normal", "beta"),
      c_var  = c("CF", "RS", "DG_ratio")
    ),
    cols = list(
      setup  = c("n_iter", "c_unit", "dg_ext", "dg_pool", "ad_annual",
                 "conf_level", "digits", "ran_seed", "trunc_pdf"),
      setupv2  = c("n_iter", "dg_ext", "dg_pool", "ad_annual",
                 "conf_level", "digits", "ran_seed", "trunc_pdf"),
      time   = c("period_no", "year_start", "year_end", "period_type"),
      area   = c("trans_no", "trans_id", "trans_period", "lu_initial_id", "lu_final_id",
                 "trans_area", "trans_se", "trans_pdf", "trans_pdf_a",
                 "trans_pdf_b", "trans_pdf_c", "redd_activity"),
      carbon = c("c_no", "c_id", "c_period", "c_element", "c_lu_id", "c_value",
                 "c_se", "c_pdf", "c_pdf_a", "c_pdf_b", "c_pdf_c"),
      areav2 = c("trans_no", "trans_period", "lu_initial", "lu_final",
                 "trans_area", "trans_se", "trans_pdf", "trans_pdf_a",
                 "trans_pdf_b", "trans_pdf_c", "redd_activity"),
      carbonv2 = c("c_no", "c_period", "c_element", "c_unit", "c_lu", "c_value",
                   "c_se", "c_pdf", "c_pdf_a", "c_pdf_b", "c_pdf_c")
    )
  )


  ##
  ## Helpers ###################################################################
  ##

  ## Advance the progress bar by one step
  show_progress <- function(.title, .value) {
    if (!is.null(.pb_session) && !is.null(.pb_id)) {
      shinyWidgets::updateProgressBar(
        session = .pb_session,
        id      = .pb_id,
        value   = round(.value),
        title   = .title,
        status  = "primary"
      )
    } else {
      message(paste0(.title, " - progress: ", round(.value), "%."))
    }
  }

  ## Emit a pass or fail message; mark all_ok FALSE on failure
   show_msg <- function(check, pass_label, fail_label, problems = NULL, stop_on_fail = TRUE) {
    if (check) {
      message("\u2713 ", pass_label)
    } else {
      detail <- if (length(problems) > 0L) {
        paste0(" [", paste(problems, collapse = ", "), "]")
      } else {
        ""
      }
      message("\u2717 ", fail_label, detail)
      if (stop_on_fail) {
        if (!is.null(.pb_session) && !is.null(.pb_id)) {
          shinyWidgets::updateProgressBar(
            session = .pb_session,
            id      = .pb_id,
            value   = 100,
            title   = "Checks halted \u2014 fix the issue above and re-run.",
            status  = "danger"
            )
          }
        message("-- Subsequent checks skipped: fix the issue above first.")
        return(list(all_ok = FALSE, data = NULL))
        #stop(fail_label, detail, call. = FALSE)
        }
    }
  }

  ## Return names of tables whose check flag is FALSE (replaces the repeated
  ## if/else pattern from fct_check_data2)
  failed_tables <- function(check_flags, table_names) {
    table_names[!check_flags]
  }

  ## Make ID from labels
  label2id <- function(label) {
    label |>
      stringr::str_to_lower() |>
      stringr::str_replace_all(" ", "_") |>
      stringr::str_replace("non.forest", "NF") |>
      stringr::str_remove("_forest") |>
      stringr::str_remove("forest")
  }

  ## system sleep to make checkks more visible in console
  minislow <- function(.num) {

    if (is.null(.num) || length(.num) != 1) return(invisible(NULL))

    num <- suppressWarnings(as.numeric(.num))

    if (!is.na(num) && is.finite(num) && num >= 0) {
      Sys.sleep(num)
    }

    invisible(NULL)

  }

  ##
  ## State #####################################################################
  ##

  ## 1 load step + 6 checks - update this if steps are added or removed
  n_steps <- 7L
  pb_factor <- .pb_max / n_steps

  ## Initiate
  step   <- 0L

  ## If check fails return out_fail
  out_fail <- list(all_ok = FALSE, data = NULL)

  ##
  ## Step 1: Load data #########################################################
  ##

  show_progress("Loading data...", step * pb_factor)

  sheets_found <- tryCatch(
    readxl::excel_sheets(.path),
    error = function(e) NULL
  )
  if (is.null(sheets_found)) {
    message("-- Cannot read the file. Is it a valid .xlsx file?")
    return(out_fail)
  }

  missing_tabsv1 <- setdiff(checklist$tabs, sheets_found)
  missing_tabsv2 <- setdiff(checklist$tabsv2, sheets_found)

  if (length(missing_tabsv1) == 0L) {
    check_version <- 1

    setup  <- readxl::read_xlsx(.path, sheet = "user_inputs",       na = "NA")
    time   <- readxl::read_xlsx(.path, sheet = "time_periods",      na = "NA")
    area   <- readxl::read_xlsx(.path, sheet = "AD_lu_transitions", na = "NA")
    carbon <- readxl::read_xlsx(.path, sheet = "c_stocks",          na = "NA")

    show_msg(check = T, pass_label = paste0("Tables loaded successfully from template v", check_version))

  } else if (length(missing_tabsv2) == 0L) {
    check_version <- 2

    setup  <- readxl::read_xlsx(.path, sheet = "setup" , na = "NA")
    time   <- readxl::read_xlsx(.path, sheet = "time"  , na = "NA")
    area   <- readxl::read_xlsx(.path, sheet = "area"  , na = "NA")
    carbon <- readxl::read_xlsx(.path, sheet = "carbon", na = "NA")

    show_msg(check = T, pass_label = paste0("Tables loaded successfully from template v", check_version))

  } else {
    show_msg(check = F, fail_label = paste0("Missing required sheet(s): ", paste(missing_tabsv2, collapse = ", ")))
    return(out_fail)
  }

  minislow(.minislow)

  step <- step + 1L

  ##
  ## Step 2: Column names #####################################################
  ##

  show_progress("Checking column names...", step * pb_factor)

   if (check_version == 1) {
     check_cols_setup  <- all(checklist$cols$setup  %in% names(setup))
     check_cols_time   <- all(checklist$cols$time   %in% names(time))
     check_cols_area   <- all(checklist$cols$area   %in% names(area))
     check_cols_carbon <- all(checklist$cols$carbon %in% names(carbon))
  } else {
    check_cols_setup  <- all(checklist$cols$setupv2  %in% names(setup))
    check_cols_time   <- all(checklist$cols$time     %in% names(time))
    check_cols_area   <- all(checklist$cols$areav2   %in% names(area))
    check_cols_carbon <- all(checklist$cols$carbonv2 %in% names(carbon))
  }

  check_cols_all <- all(check_cols_setup, check_cols_time, check_cols_area, check_cols_carbon)

  show_msg(
    check         = check_cols_all,
    pass_label = "Column names: all required columns present",
    fail_label = "Column names: missing columns in",
    problems   = failed_tables(
      c(check_cols_setup, check_cols_time, check_cols_area, check_cols_carbon),
      c("setup/user_inputs", "time/time_periods", "area/AD_lu_transitions", "carbon/c_stocks")
    )
  )

  if (!check_cols_all) return(out_fail)

  minislow(.minislow)

  step <- step + 1L

  ##
  ## Step 3: Table sizes ######################################################
  ##

  show_progress("Checking table dimensions...", step * pb_factor)

  check_size_setup  <- nrow(setup)  == 1L  # exactly one settings row
  #check_size_time   <- nrow(time)   >= 2L  # at least one REF + one MON period
  check_size_area   <- nrow(area)   >= 2L  # at least two land use transitions
  check_size_carbon <- nrow(carbon) >= 2L  # at least one initial + one final C stock


  check_size_time_ref <- any(stringr::str_detect(time$period_type, "REF"))
  check_size_time_mon <- any(stringr::str_detect(time$period_type, "MON"))

  check_size_time <- all(nrow(time) >= 2L, check_size_time_ref, check_size_time_mon)

  check_size_all <- all(check_size_setup, check_size_time, check_size_area, check_size_carbon)

  show_msg(
    check         = check_size_all,
    pass_label = "Table sizes: all tables have sufficient rows",
    fail_label = "Table sizes: unexpected row count in",
    problems   = failed_tables(
      c(check_size_setup, check_size_time, check_size_area, check_size_carbon),
      c("setup/user_inputs", "time/time_periods", "area/AD_lu_transitions", "carbon/c_stocks")
    )
  )

  if (!check_size_all) return(out_fail)

  minislow(.minislow)

  step <- step + 1L

  ##
  ## Step 4: Data types #######################################################
  ##

  show_progress("Checking column data types...", step * pb_factor)

  check_dt_setup <- all(
    is.logical(setup$trunc_pdf),
    is.numeric(setup$n_iter),
    is.numeric(setup$ran_seed)  | is.logical(setup$ran_seed),
    if (check_version == 1) is.character(setup$c_unit) else TRUE,
    is.character(setup$dg_pool) | is.logical(setup$dg_pool),
    is.logical(setup$ad_annual),
    is.numeric(setup$conf_level),
    is.numeric(setup$digits)
  )

  check_dt_time <- all(
    is.character(time$period_no),
    is.numeric(time$year_start),
    is.numeric(time$year_end),
    is.character(time$period_type)
  )

  check_dt_area <- all(
    is.numeric(area$trans_no),
    if (check_version == 1) is.character(area$trans_id) else TRUE,
    is.character(area$trans_period),
    if (check_version == 1) is.character(area$lu_initial_id) else is.character(area$lu_initial),
    if (check_version == 1) is.character(area$lu_final_id) else is.character(area$lu_final),
    # is.numeric(area$trans_area)  | is.logical(area$trans_area),
    # is.numeric(area$trans_se)    | is.logical(area$trans_se),
    is.numeric(area$trans_area),
    is.numeric(area$trans_se),
    is.character(area$trans_pdf),
    is.numeric(area$trans_pdf_a) | is.logical(area$trans_pdf_a),
    is.numeric(area$trans_pdf_b) | is.logical(area$trans_pdf_b),
    is.numeric(area$trans_pdf_c) | is.logical(area$trans_pdf_c),
    is.character(area$redd_activity)
  )

  check_dt_carbon <- all(
    is.numeric(carbon$c_no),
    if (check_version == 1) is.character(carbon$c_id) else TRUE,
    is.character(carbon$c_period),
    is.character(carbon$c_element),
    if (check_version == 1) is.character(carbon$c_lu_id) else is.character(carbon$c_lu),
    if (check_version == 2) is.character(carbon$c_unit) else TRUE,
    # is.numeric(carbon$c_value)  | is.logical(carbon$c_value),
    # is.numeric(carbon$c_se)     | is.logical(carbon$c_se),
    is.numeric(carbon$c_value),
    is.numeric(carbon$c_se),
    is.character(carbon$c_pdf),
    is.numeric(carbon$c_pdf_a)  | is.logical(carbon$c_pdf_a),
    is.numeric(carbon$c_pdf_b)  | is.logical(carbon$c_pdf_b),
    is.numeric(carbon$c_pdf_c)  | is.logical(carbon$c_pdf_c)
  )

  check_dt_all <- all(check_dt_setup, check_dt_time, check_dt_area, check_dt_carbon)

  show_msg(
    check      = check_dt_all,
    pass_label = "Data types: all columns have correct data types",
    fail_label = "Data types: unexpected column types in",
    problems   = failed_tables(
      c(check_dt_setup, check_dt_time, check_dt_area, check_dt_carbon),
      c("setup/user_inputs", "time/time_periods", "area/AD_lu_transitions", "carbon/c_stocks")
    )
  )

  if (!check_dt_all) return(out_fail)

  minislow(.minislow)

  step <- step + 1L


  ##
  ## Step 5: Category variables ###############################################
  ##

  show_progress("Checking category values...", step * pb_factor)

  ## Parse dg_pool, which can be a comma-separated string e.g. "AGB, BGB"
  dg_pool_vec <- stringr::str_split(setup$dg_pool, pattern = ",") |>
    purrr::map(stringr::str_trim) |>
    unlist()

  check_cats_setup <- all(
    if (check_version == 1) setup$c_unit %in% checklist$cats$c_unit else TRUE,
    dg_pool_vec  %in% checklist$cats$c_pool
  )

  ptype_pattern  <- paste0(checklist$cats$p_type, collapse = "|")
  check_cats_time <- all(stringr::str_detect(unique(time$period_type[!is.na(time$period_type)]), pattern = ptype_pattern))

  check_cats_area <- all(
    unique(area$redd_activity)                    %in% checklist$cats$r_acti,
    unique(stringr::str_to_lower(area$trans_pdf)) %in% checklist$cats$pdf
  )

  check_cats_carbon <- all(
    unique(carbon$c_element) %in% c(checklist$cats$c_pool, checklist$cats$c_var),
    if (check_version == 2) all(unique(carbon$c_unit[!is.na(carbon$c_unit)]) %in% checklist$cats$c_unit) else TRUE,
    if (check_version == 2) all(carbon$c_unit[carbon$c_element %in% checklist$cats$c_pool] %in% c("DM", "C")) else TRUE,
    unique(stringr::str_to_lower(carbon$c_pdf)) %in% checklist$cats$pdf
  )

  check_cats_all <- all(check_cats_setup, check_cats_time, check_cats_area, check_cats_carbon)

  show_msg(
    check      = check_cats_all,
    pass_label = "Category values: all categories are valid",
    fail_label = "Category values: invalid categories in",
    problems   = failed_tables(
      c(check_cats_setup, check_cats_time, check_cats_area, check_cats_carbon),
      c("setup/user_inputs", "time/time_periods", "area/AD_lu_transitions", "carbon/c_stocks")
    )
  )

  if (!check_cats_all) return(out_fail)

  minislow(.minislow)

  step <- step + 1L


  ##
  ## Step 6: Unique IDs #######################################################
  ##

  show_progress("Checking unique IDs...", step * pb_factor)

  ## Create unique IDs for 'area' and 'carbon'
  if (check_version == 2) {
    area <- area |>
      dplyr::mutate(
        lu_initial_id = label2id(.data$lu_initial),
        lu_final_id = label2id(.data$lu_final),
        #trans_id = paste(trans_period, lu_initial_id, "2", lu_final_id, sep = "*")
        trans_id = paste(.data$trans_period, .data$lu_initial_id, .data$lu_final_id, sep = "*")
      )

    unique_c_period <- length(unique(carbon$c_period)) == 1 & unique(carbon$c_period) == "ALL"

    carbon <- carbon |>
      dplyr::mutate(
        c_lu_id = label2id(.data$c_lu),
        c_plu_id = if (unique_c_period) .data$c_lu_id else paste(.data$c_period, .data$c_lu_id, sep = "*"),
        c_id = if (unique_c_period) paste(.data$c_lu_id, .data$c_element, sep = "*") else paste(.data$c_period, .data$c_lu_id, .data$c_element, sep = "*")
      )
  }


  ## Get dg_ext to IDs
  setup$dg_ext <- label2id(setup$dg_ext) |> stringr::str_remove_all("\"")

  ## Check unique IDs
  check_ids_time   <- nrow(time)   == length(unique(time$period_no))
  check_ids_area   <- nrow(area)   == length(unique(area$trans_id))
  check_ids_carbon <- nrow(carbon) == length(unique(carbon$c_id))

  check_ids_all <- all(check_ids_time, check_ids_area, check_ids_carbon)

  show_msg(
    check      = check_ids_all,
    pass_label = "Unique IDs: no duplicates or missing IDs found",
    fail_label = "Unique IDs: duplicate or missing IDs found in",
    problems   = failed_tables(
      c(check_ids_time, check_ids_area, check_ids_carbon),
      c("setup/user_inputs", "time/time_periods", "area/AD_lu_transitions", "carbon/c_stocks")
    )
  )

  if (!check_ids_all) return(out_fail)

  minislow(.minislow)

  step <- step + 1L


  ##
  ## Step 7: Cross- and intra-table matching and logical consistency ##########
  ##

  show_progress("Checking cross-table and intra_table consistency...", step * pb_factor)

  ## + Match periods ------
  ## Periods match between AD and time tables
  check_match_period_area <- all(
    sort(unique(area$trans_period)) == sort(unique(time$period_no))
  )

  ## Periods match between CS and time tables (skip when all rows use "ALL")
  carbon_with_period <- carbon |> dplyr::filter(.data$c_period != "ALL")
  check_match_period_carbon <- if (nrow(carbon_with_period) > 0L) {
    all(sort(unique(carbon$c_period)) == sort(unique(time$period_no)))
  } else {
    TRUE
  }

  ## + Match land uses ------
  ## All land uses in 'area' must be present in 'carbon'
  # lu_in_area <- sort(unique(c(area$lu_initial, area$lu_final)))
  # lu_in_carbon <- sort(unique(carbon$c_lu))
  # check_match_lu <- all(lu_in_area %in% lu_in_carbon)

  lu_in_area <- sort(unique(c(area$lu_initial_id, area$lu_final_id)))
  lu_in_carbon <- sort(unique(carbon$c_lu_id))
  check_match_lu_id <- all(lu_in_area %in% lu_in_carbon)

  ## + Carbon fraction x dry matter ------
  ## Carbon fraction must be provided when unit is dry matter
  check_match_dm <- if (check_version == 1) {
    if (setup$c_unit == "DM") {
      all(setup$c_fraction > 0, setup$c_fraction_se >= 0)
    } else if (setup$c_unit == "C") {
      TRUE
    } else {
      FALSE
    }
  } else if (check_version == 2) {
    if ("DM" %in% unique(carbon$c_unit)) {
      all("CF" %in% carbon$c_element, carbon$c_value[carbon$c_element == "CF"] > 0, carbon$c_se[carbon$c_element == "CF"] >= 0)
    } else if ("C" %in% unique(carbon$c_unit)) {
      TRUE
    } else {
      FALSE
    }
  } else {
    FALSE
  }

  ## + Match RS with AGB ------
  ## if RS is use, there should be AGB for the same LU
  check_match_rsagb1 <- if ("RS" %in% unique(carbon$c_element)) {
    lu_rs <- carbon |> dplyr::filter(.data$c_element == "RS") |> dplyr::pull("c_plu_id") |> sort()
    lu_agb <- carbon |> dplyr::filter(.data$c_element == "AGB") |> dplyr::pull("c_plu_id") |> sort()
    length(setdiff(lu_rs, lu_agb)) == 0
  } else {
    TRUE
  }

  # ## C unit for AGB and RS should be same -- NOT USED now RS dimensionless ratio
  # check_match_rsagb2 <- if ("RS" %in% unique(carbon$c_element) & check_match_rsagb1) {
  #   lu_rsagb <- carbon |>
  #     dplyr::filter(c_element %in% c("AGB", "RS")) |>
  #     tidyr::pivot_wider(id_cols = c(c_plu_id), values_from = c_unit, names_from = c_element)
  #   all(lu_rsagb$AGB == lu_rsagb$RS)
  # } else if (!check_match_rsagb1) {
  #   FALSE
  # } else {
  #   TRUE
  # }

  # check_match_rsagb <- all(check_match_rsagb1, check_match_rsagb2)
  check_match_rsagb <- all(check_match_rsagb1)

  ## + Match degraded and intact LU ------
  ## If DG_ratio is used, dg_ext must strip degraded LU names back to intact LUs
  if (!is.na(setup$dg_ext) && "DG_ratio" %in% unique(carbon$c_element)) {
    dg_lu_intact <- carbon |>
      dplyr::filter(.data$c_element == "DG_ratio") |>
      dplyr::pull("c_lu_id") |>
      stringr::str_remove(pattern = setup$dg_ext) |>
      stringr::str_replace("[^A-Za-z]$", "") ## Remove trailing special character if any

    check_match_dg <- all(dg_lu_intact %in% unique(carbon$c_lu_id))
  } else if (is.na(setup$dg_ext) && "DG_ratio" %in% unique(carbon$c_element)) {
    check_match_dg <- FALSE   # DG_ratio used but no extension provided
  } else {
    check_match_dg <- TRUE
  }




  ## + Recap ------
  match_flags  <- c(check_match_period_area, check_match_period_carbon, check_match_lu_id,
                    check_match_dm, check_match_rsagb, check_match_dg)
  match_labels <- c(
    "trans_period does not match period_no",
    "c_period does not match period_no",
    "land use IDs present in AD/area but missing from c_stocks/carbon",
    "c_unit is 'DM' but CF is missing in usr(v1)/carbon(v2)",
    "RS defined for land uses with no AGB values or mismatch C unit",
    "DG_ratio used but dg_ext does not resolve to existing LU IDs"
  )

  check_match_all <- all(match_flags)

  show_msg(
    check      = check_match_all ,
    pass_label = "Cross-table consistency: all references match",
    fail_label = "Cross-table consistency: mismatches found",
    problems   = match_labels[!match_flags]
  )

  if (!check_match_all) return(out_fail)

  minislow(.minislow)


  ##
  ## Final progress bar update #################################################
  ##

  all_ok <- all(
    check_cols_all, check_size_all, check_dt_all, check_cats_all, check_ids_all,
    check_match_all
    )

  if (!is.null(.pb_session) && !is.null(.pb_id)) {
    shinyWidgets::updateProgressBar(
      session = .pb_session,
      id      = .pb_id,
      value   = 100,
      title   = if (all_ok) "All checks passed!" else "Some checks failed - see details.",
      status  = if (all_ok) "success" else "danger"
    )
  }

  if (all_ok) {
    message("-- All checks passed.")
  } else {
    message("-- One or more checks failed. Fix the issues above before running simulations.")
  }

  ## ADD calculated elements to data
  time$nb_years  <- time$year_end - time$year_start + 1
  #setup$ci_alpha <- 1 - setup$conf_level
  setup$z_score        <- stats::qnorm(1 - (1 - setup$conf_level) / 2)
  setup$conf_level_txt <- paste0(setup$conf_level * 100, "%")


  ##
  ## Return ####################################################################
  ##

  list(
    all_ok           = all_ok,
    template_version = check_version,
    data             = list(setup = setup, time = time, area = area, carbon = carbon)
  )

}
