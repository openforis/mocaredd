

cs <- readxl::read_xlsx(system.file("extdata/example1.xlsx", package = "mocaredd"), sheet = "c_stock", na = "NA")
ad <- readxl::read_xlsx(system.file("extdata/example1.xlsx", package = "mocaredd"), sheet = "AD_lu_transitions", na = "NA")
usr <- readxl::read_xlsx(system.file("extdata/example1.xlsx", package = "mocaredd"), sheet = "user_inputs", na = "NA")

ad_clean <- ad |> dplyr::filter(!is.na(trans_area) | !is.na(trans_pdf_a))
cs_clean <- cs |> dplyr::filter(!is.na(c_value) | !is.na(c_pdf_a))

res <- fct_combine_mcs_E(.ad = ad_clean, .cs = cs_clean, .usr = usr)
test_res <- round(median(res$E_sim))


test_that("multiplication works", {
  expect_equal(test_res, 75830)

})
