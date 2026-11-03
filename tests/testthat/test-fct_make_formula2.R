test_that("AGB + RS is factored, v1 and v2", {
  expect_equal(fct_make_formula2(c("AGB", "RS"), "DM"), "AGB * (1 + RS) * CF")
  expect_equal(fct_make_formula2(c("AGB", "RS"), c("DM", NA), .version = 2), "AGB * (1 + RS) * CF")
  expect_equal(fct_make_formula2(c("AGB", "RS", "DW"), c("DM", NA, "C"), .version = 2), "AGB * (1 + RS) * CF + DW")
  expect_equal(fct_make_formula2(c("AGB", "RS"), "C"), "AGB * (1 + RS)")
})

test_that("AGB + BGB, other pools, ALL, empty", {
  expect_equal(fct_make_formula2(c("AGB", "BGB", "SOC"), "DM"), "(AGB + BGB) * CF + SOC")
  expect_equal(fct_make_formula2(c("AGB", "BGB"), c("DM", "C"), .version = 2), "AGB * CF + BGB")
  expect_equal(fct_make_formula2("ALL", "DM"), "ALL")
  expect_equal(fct_make_formula2("ALL", "DM", .version = 2), "ALL * CF")
  expect_equal(fct_make_formula2("DG_ratio", NA, .version = 2), "0")
  expect_error(fct_make_formula2("RS", "DM"))
})
