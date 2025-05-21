
library(mocaredd)
library(readxl)
library(dplyr)

path <- system.file("extdata/example2-with-sims.xlsx", package = "mocaredd")

cs <- read_xlsx(path = path, sheet = "c_stocks", na = "NA")
ad <- read_xlsx(path = path, sheet = "AD_lu_transitions", na = "NA")
usr <- read_xlsx(path = path, sheet = "user_inputs", na = "NA")

set.seed(1)
res <- fct_combine_mcs_cstock(.ad = ad, .cs = cs, .usr = usr)

test_res <- res |>
  dplyr::filter(.data$sim_no == 1, .data$lu_id == "EV") |>
  dplyr::pull("c_stock") |>
  round()

testthat::test_that("fct_combine_mcs_C works on example2", {
  testthat::expect_equal(test_res, 136)
})
