

usr <- readxl::read_xlsx(
  path = system.file("extdata/example1.xlsx", package = "mocaredd"),
  sheet = "user_inputs",
  na = "NA"
  )

cs <- readxl::read_xlsx(
  path = system.file("extdata/example1.xlsx", package = "mocaredd"),
  sheet = "c_stock",
  na = "NA"
  )

cs_clean <- cs |> dplyr::filter(!(is.na(c_value) & is.na(c_pdf_a)))

c_sub <- cs_clean |> dplyr::filter(!(is.na(c_value) & is.na(c_pdf_a)), lu_id == "ev_wet_closed")

set.seed(1)
res <- fct_combine_mcs_C(.c_sub = c_sub, .usr = usr)
test_res <- round(median(res$C_all))

testthat::test_that("fct_combine_mcs_C works on example1", {
  testthat::expect_equal(test_res, 451)
})
