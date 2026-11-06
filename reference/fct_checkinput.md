# Check and load the mocaredd input XLSX file

Loads an XLSX input file (e.g. from a Shiny `fileInput` widget),
verifies that all required sheets are present, reads the four data
tables, and runs six conformity checks:

1.  Column names

2.  Table sizes

3.  Column data types

4.  Category variable values

5.  Unique IDs

6.  Cross-table matching and logical consistency

Each check emits a [`message()`](https://rdrr.io/r/base/message.html),
so results can be captured outside the function with
`withCallingHandlers(message = ...)` and displayed in a console-style UI
element. A `shinyWidgets` progress bar is advanced after each of the
`n_steps` steps (1 load + 6 checks).

## Usage

``` r
fct_checkinput(
  .path,
  .pb_session = NULL,
  .pb_id = NULL,
  .pb_max = 100,
  .minislow = NA
)
```

## Arguments

- .path:

  Character. Path to the XLSX file (e.g. `input$load_xlsx$datapath`).

- .pb_session:

  Shiny session object used to update the progress bar. Pass `NULL`
  (default) to skip progress bar updates (e.g. when calling outside
  Shiny).

- .pb_id:

  Character. The `id` of the
  [`shinyWidgets::progressBar()`](https://dreamrs.github.io/shinyWidgets/reference/progress-bar.html)
  to update. Ignored when `.pb_session` is `NULL`.

- .pb_max:

  Value between 0 and 100, default to 100. What should progress bar max
  be after all checks are passed.

- .minislow:

  NA or numeric. If numeric, add that value in seconds to system sleep
  to make checks more visible in the console.

## Value

A list with three elements:

- `all_ok`:

  Logical. `TRUE` only if every check passed.

- `template_version`:

  Integer, 1 or 2, the detected template version. Downstream calculation
  functions take this whole list as `.checked_data` and read
  `.checked_data$template_version` to know where to find the carbon unit
  and carbon fraction.

- `data`:

  Named list with elements `setup`, `time`, `area`, `carbon` (the four
  loaded tables as tibbles).

The function throws an error (via
[`stop()`](https://rdrr.io/r/base/stop.html)) only for unrecoverable
failures (unreadable file, missing sheets). All check outcomes are
reported via [`message()`](https://rdrr.io/r/base/message.html).

## Server-side usage


    log_lines <- character()
    result <- withCallingHandlers(
      tryCatch(
        fct_checkinput(.path = path, .pb_session = session, .pb_id = "prog_allchecks"),
        error = function(e) {
          log_lines <<- c(log_lines, paste0("\u2717 ERROR: ", conditionMessage(e)))
          NULL
        }
      ),
      message = function(m) {
        log_lines <<- c(log_lines, trimws(conditionMessage(m)))
        invokeRestart("muffleMessage")
      }
    )
    rv$check_log <- log_lines
    if (!is.null(result)) {
      rv$inputs$setup  <- result$data$setup
      rv$inputs$time   <- result$data$time
      rv$inputs$area   <- result$data$area
      rv$inputs$carbon <- result$data$carbon
      rv$checks$all_ok <- result$all_ok
    }

## Examples

``` r
path <- system.file("extdata/example1-4pools.xlsx", package = "mocaredd")

log_lines <- character()
result <- withCallingHandlers(
  fct_checkinput(.path = path),
  message = function(m) {
    log_lines <<- c(log_lines, trimws(conditionMessage(m)))
    invokeRestart("muffleMessage")
  }
)
cat(log_lines, sep = "\n")
#> Loading data... - progress: 0%.
#> ✓ Tables loaded successfully from template v1
#> Checking column names... - progress: 14%.
#> ✓ Column names: all required columns present
#> Checking table dimensions... - progress: 29%.
#> ✓ Table sizes: all tables have sufficient rows
#> Checking column data types... - progress: 43%.
#> ✓ Data types: all columns have correct data types
#> Checking category values... - progress: 57%.
#> ✓ Category values: all categories are valid
#> Checking unique IDs... - progress: 71%.
#> ✓ Unique IDs: no duplicates or missing IDs found
#> Checking cross-table and intra_table consistency... - progress: 86%.
#> ✓ Cross-table consistency: all references match
#> -- All checks passed.
```
