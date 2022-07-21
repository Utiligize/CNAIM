library(testthat)
library(CNAIM)

context("Future Probability of Failure for 0.4kV UG Cable (Non Pressurised)")

test_that("pof_future_cables_04kv_pex", {

res <- pof_future_cables_04kv_pex(utilisation_pct = 80,
                                  operating_voltage_pct = 60,
                                  sheath_test = "Default",
                                  partial_discharge = "Default",
                                  fault_hist = "Default",
                                  reliability_factor = "Default",
                                  age = 50,
                                  k_value = 0.0658,
                                  c_value = 1.087,
                                  normal_expected_life = 80,
                                  simulation_end_year = 100)

  expect_equal(res$PoF[which(res$year == 50)], 0.12098057)

})
