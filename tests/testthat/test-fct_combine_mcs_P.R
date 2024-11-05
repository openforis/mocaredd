
cs <- readxl::read_xlsx(system.file("extdata/example1.xlsx", package = "mocaredd"), sheet = "c_stock", na = "NA")
ad <- readxl::read_xlsx(system.file("extdata/example1.xlsx", package = "mocaredd"), sheet = "AD_lu_transitions", na = "NA")
usr <- readxl::read_xlsx(system.file("extdata/example1.xlsx", package = "mocaredd"), sheet = "user_inputs", na = "NA")
time <- readxl::read_xlsx(system.file("extdata/example1.xlsx", package = "mocaredd"), sheet = "time_periods", na = "NA")

ad_clean <- ad |> dplyr::filter(!is.na(trans_area) | !is.na(trans_pdf_a))
cs_clean <- cs |> dplyr::filter(!is.na(c_value) | !is.na(c_pdf_a))
time_clean <- time |> dplyr::mutate(nb_years = year_end - year_start + 1)

sim_trans <- fct_combine_mcs_E(.ad = ad_clean, .cs = cs_clean, .usr = usr)

sim_FREL <- fct_combine_mcs_P(
  .data = sim_trans,
  .time = time_clean,
  .period_type = "REF",
  .ad_annual = usr$ad_annual
)

hist(sim_FREL$E_sim)
test_res <- round(median(sim_FREL$E_sim))

testthat::test_that("function doesn't work :P", {
  testthat::expect_equal(test_res, 4591614)

})
