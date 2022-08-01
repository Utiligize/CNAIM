library(testthat)
library(CNAIM)

context("Future Probability of Failure for Cables 10 kv oil")

test_that("pof_future_cables_10kv_oil", {

  res <-   pof_future_cables_10kv_oil(
     utilisation_pct = 80,
     operating_voltage_pct = 60,
     sheath_test = "Default",
     partial_discharge = "Default",
     fault_hist = "Default",
     reliability_factor = "Default",
     age = 50,
     k_value = 0.24,
     c_value = 1.087,
     normal_expected_life = 80,
     simulation_end_year = 100)


  expected_value <- readRDS(system.file("testdata/pof_future_cables_10kv_oil.rds", package =
                                          "CNAIM"))

  expect_equal(res, expected_value)

})
