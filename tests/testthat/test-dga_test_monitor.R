library(testthat)
library(CNAIM)

context("DGA test modifier")

test_that("default case", {
  res <- dga_test_modifier(hydrogen = "Default",
                           methane = "Default",
                           ethylene = "Default",
                           ethane = "Default",
                           acetylene = "Default",
                           hydrogen_pre = "Default",
                           methane_pre = "Default",
                           ethylene_pre = "Default",
                           ethane_pre = "Default",
                           acetylene_pre = "Default")

  expected_df <- data.frame(dga_test_factor = 1, dga_test_cap = 10,
                            dga_test_collar = 1)

  expect_equal(res, expected_df)
})
