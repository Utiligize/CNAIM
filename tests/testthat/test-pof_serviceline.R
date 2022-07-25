library(testthat)
library(CNAIM)

context("Current Probability of Failure for Serviceline")

test_that("pof_serviceline", {

  res <- pof_serviceline(utilisation_pct = 80,
                         operating_voltage_pct = "Default",
                         sheath_test = "Default",
                         partial_discharge = "Default",
                         fault_hist = "Default",
                         reliability_factor = "Default",
                         age = 50,
                         k_value = 0.0329,
                         c_value = 1.087,
                         normal_expected_life = 75)

  expect_equal(res, 0.003467356)

})
