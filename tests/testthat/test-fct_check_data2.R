library(mocaredd)
library(readxl)

cs <- read_xlsx(
  path = system.file("extdata/example1.xlsx", package = "mocaredd"),
  sheet = "c_stocks",
  na = "NA"
  )
ad <- read_xlsx(
  path = system.file("extdata/example1.xlsx", package = "mocaredd"),
  sheet = "AD_lu_transitions",
  na = "NA"
  )
usr <- read_xlsx(
  path = system.file("extdata/example1.xlsx", package = "mocaredd"),
  sheet = "user_inputs",
  na = "NA"
  )
time <- read_xlsx(
  path = system.file("extdata/example1.xlsx", package = "mocaredd"),
  sheet = "time_periods",
  na = "NA"
  )

app_checklist <- list(
  xlsx_tabs  = c("user_inputs", "time_periods", "AD_lu_transitions", "c_stocks"),
  cat_cunits = c("DM", "C"),
  cat_cpools = c("AGB", "BGB", "DW", "LI", "SOC", "ALL"),
  cat_racti  = c("DF", "DG", "EN", "EN_AF", "EN_RE"),
  cat_ptype  = c("REF", "REF[0-9]", "MON", "MON[0-9]"),
  cat_pdf    = c("normal", "beta"),
  col_usr    = c("trunc_pdf", "n_iter", "ran_seed", "c_unit", "c_fraction", "dg_pool", "dg_expool", "ad_annual", "conf_level"),
  col_time   = c("period_no", "year_start", "year_end", "period_type"),
  col_ad     = c("trans_no",	"trans_id",	"trans_period",	"redd_activity", "lu_initial_id", "lu_initial",	"lu_final_id", "lu_final", "trans_area", "trans_se", "trans_pdf", "trans_pdf_a", "trans_pdf_b", "trans_pdf_c"),
  col_cs     = c("c_no",	"c_id", "c_period", "lu_id", "lu_name",	"c_pool",	"c_value",	"c_se",	"c_pdf",	"c_pdf_a",	"c_pdf_b",	"c_pdf_c")
)

app_checklist$cat_cpools_all <- c(app_checklist$cat_cpools, "RS", "DG_ratio")

res <- fct_check_data2(.ad = ad, .cs = cs, .usr = usr, .time = time, .checklist = app_checklist)

testthat::test_that("function doesn't work :P", {
  testthat::expect_equal(res$all_ok, TRUE)
})

