library(testthat)
library(CNAIM)

context("Duty Factor for all cables (incl. submarine cables).")

test_that("Default case", {
  res <- duty_factor_cables(utilisation_pct = "Default",
                            operating_voltage_pct = "Default",
                            voltage_level = "EHV")


  expected_val <- 1

  expect_equal(res, expected_val)
})
