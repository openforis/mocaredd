
library(mocaredd)
library(readxl)
library(dplyr)

path <- system.file("extdata/example2-with-sims.xlsx", package = "mocaredd")

cs <- read_xlsx(path = path, sheet = "c_stocks", na = "NA")
ad <- read_xlsx(path = path, sheet = "AD_lu_transitions", na = "NA")
usr <- read_xlsx(path = path, sheet = "user_inputs", na = "NA")
time <- read_xlsx(path = path, sheet = "time_periods", na = "NA")

ad_clean <- ad |> dplyr::filter(!is.na(trans_area) | !is.na(trans_pdf_a))
cs_clean <- cs |> dplyr::filter(!is.na(c_value) | !is.na(c_pdf_a))
time_p <- time |> dplyr::mutate(nb_years = year_end - year_start + 1)

set.seed(1)
sim_trans <- fct_combine_mcs_E(.ad = ad_clean, .cs = cs_clean, .usr = usr, .time = time_p)

sim_FREL <- fct_combine_mcs_P(
  .data = sim_trans,
  .time = time_p,
  .period_type = "REF",
  .ad_annual = usr$ad_annual
)

test_res <- round(median(sim_FREL$E))

testthat::test_that("function doesn't work :P", {
  testthat::expect_equal(test_res, 20713724)
})
