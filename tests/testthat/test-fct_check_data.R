
cs <- readxl::read_xlsx(system.file("extdata/example1.xlsx", package = "mocaredd"), sheet = "c_stock", na = "NA")
ad <- readxl::read_xlsx(system.file("extdata/example1.xlsx", package = "mocaredd"), sheet = "AD_lu_transitions", na = "NA")

init <- list(
  c_pools = c("AGB", "BGB", "RS", "DW", "LI", "SOC", "ALL", "DG_ratio"),
  redd_acti = c("DF", "DG", "EN", "EN_AF", "EN_RE")
)



testthat::test_that("all checks pass", {
  testthat::expect_equal(all(fct_check_data(.ad = ad, .cs = cs, .init = init)), TRUE)
})
