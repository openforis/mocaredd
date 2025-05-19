library(mocaredd)
library(readxl)

path <- system.file("extdata/example1-4pools.xlsx", package = "mocaredd")

cs <- read_xlsx(path = path, sheet = "c_stocks", na = "NA")
ad <- read_xlsx(path = path, sheet = "AD_lu_transitions", na = "NA")
usr <- read_xlsx(path = path, sheet = "user_inputs", na = "NA")
time <- read_xlsx(path = path, sheet = "time_periods", na = "NA")

res <- fct_check_data2(.ad = ad, .cs = cs, .usr = usr, .time = time)

testthat::test_that("function doesn't work :P", {
  testthat::expect_equal(res$all_ok, TRUE)
})

