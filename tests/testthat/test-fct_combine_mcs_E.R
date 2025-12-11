
library(mocaredd)
library(readxl)
library(dplyr)


path <- system.file("extdata/example2-with-sims.xlsx", package = "mocaredd")

cs <- read_xlsx(path = path, sheet = "c_stocks", na = "NA")
ad <- read_xlsx(path = path, sheet = "AD_lu_transitions", na = "NA")
usr <- read_xlsx(path = path, sheet = "user_inputs", na = "NA")
time <- read_xlsx(path = path, sheet = "time_periods", na = "NA")

cs_clean <- cs |> filter(!is.na(c_value) | !is.na(c_pdf_a))
time_p   <- time |> dplyr::mutate(nb_years = year_end - year_start + 1)

set.seed(1)
res <- fct_combine_mcs_E(.ad = ad, .cs = cs_clean, .usr = usr, .time = time_p)
test_res <- res |>filter(trans_id == "T1_EV_Crop")


testthat::test_that("Works", {
  testthat::expect_equal(round(median(test_res$E / 10^6)), 14)
})
