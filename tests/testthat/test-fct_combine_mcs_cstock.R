

cs <- readxl::read_xlsx(
  system.file("extdata/example1.xlsx", package = "mocaredd"),
  sheet = "c_stocks",
  na = "NA"
  )
ad <- readxl::read_xlsx(
  system.file("extdata/example1.xlsx", package = "mocaredd"),
  sheet = "AD_lu_transitions",
  na = "NA"
  )
usr <- readxl::read_xlsx(
  system.file("extdata/example1.xlsx", package = "mocaredd"),
  sheet = "user_inputs",
  na = "NA"
  )

set.seed(1)
res <- fct_combine_mcs_cstock(.ad = ad, .cs = cs, .usr = usr)

test_res <- res |>
  dplyr::filter(.data$sim_no == 1, .data$lu_id == "open") |>
  dplyr::pull("C_all") |>
  round()


testthat::test_that("fct_combine_mcs_C works on example1", {
  testthat::expect_equal(test_res, 14)
})
