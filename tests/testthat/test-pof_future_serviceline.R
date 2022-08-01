library(testthat)
library(CNAIM)

context("Future Probability of Failure for Serviceline")

test_that("pof_future_serviceline", {

  res <-    pof_future_serviceline(
     utilisation_pct = 80,
     operating_voltage_pct = 60,
     sheath_test = "Default",
     partial_discharge = "Default",
     fault_hist = "Default",
     reliability_factor = "Default",
     age = 50,
     k_value = 0.0329,
     c_value = 1.087,
     normal_expected_life = 75,
     simulation_end_year = 100)


  expected_value <- readRDS(system.file("testdata/pof_future_serviceline.rds", package =
                                          "CNAIM"))

  expect_equal(res, expected_value)

})
