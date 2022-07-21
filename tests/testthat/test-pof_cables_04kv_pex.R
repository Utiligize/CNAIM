library(testthat)
library(CNAIM)

context("Current Probability of Failure for 0.4kV UG Cable (Non Pressurised)")

test_that("pof_cables_04kv_pex", {

  res <- pof_cables_04kv_pex(utilisation_pct = 80,
                             operating_voltage_pct = "Default",
                             sheath_test = "Default",
                             partial_discharge = "Default",
                             fault_hist = "Default",
                             reliability_factor = "Default",
                             age = 50,
                             k_value = 0.0658,
                             c_value = 1.087,
                             normal_expected_life = 80)

  expect_equal(res, 0.005784159)

})
