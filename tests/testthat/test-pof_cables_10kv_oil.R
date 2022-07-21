library(testthat)
library(CNAIM)

context("Current Probability of Failure for 10kV UG Cable (Oil)")

test_that("pof_cables_10kv_oil", {

  res <- pof_cables_10kv_oil(utilisation_pct = 80,
                             operating_voltage_pct = "Default",
                             sheath_test = "Default",
                             partial_discharge = "Default",
                             fault_hist = "Default",
                             reliability_factor = "Default",
                             age = 50,
                             k_value = 0.24,
                             c_value = 1.087,
                             normal_expected_life = 80)

  expect_equal(res, 0.02109723)

})
