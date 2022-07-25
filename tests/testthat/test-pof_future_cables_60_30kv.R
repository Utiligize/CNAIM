library(testthat)
library(CNAIM)

context("Future Probability of Failure for 30/60kV UG Cable (Non Pressurised)")

test_that("pof_future_cables_60_30kv", {

  res <- pof_future_cables_60_30kv(cable_type = "60kV UG Cable (Non Pressurised)",
                                   sub_division = "Aluminium sheath - Aluminium conductor",
                                   utilisation_pct = 75,
                                   operating_voltage_pct = 50,
                                   sheath_test = "Default",
                                   partial_discharge = "Default",
                                   fault_hist = "Default",
                                   leakage = "Default",
                                   reliability_factor = "Default",
                                   age = 1,
                                   k_value = "Default",
                                   c_value = 1.087,
                                   normal_expected_life = "Default",
                                   simulation_end_year = 100)

  expect_equal(res$PoF[which(res$year == 50)], 0.01875330)

})
