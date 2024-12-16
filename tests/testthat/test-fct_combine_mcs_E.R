
library(readxl)
library(dplyr)
library(mocaredd)

path <- system.file("extdata/example1-4pools.xlsx", package = "mocaredd")

cs <- read_xlsx(path = path, sheet = "c_stocks", na = "NA")
ad <- read_xlsx(path = path, sheet = "AD_lu_transitions", na = "NA")
usr <- read_xlsx(path = path, sheet = "user_inputs", na = "NA")

cs_clean <- cs |> filter(!is.na(c_value) | !is.na(c_pdf_a))

set.seed(1)
res <- fct_combine_mcs_E(.ad = ad, .cs = cs_clean, .usr = usr)
test_res <- round(median(res$E_sim))


testthat::test_that("Works", {
  testthat::expect_equal(test_res, 79642)

})
