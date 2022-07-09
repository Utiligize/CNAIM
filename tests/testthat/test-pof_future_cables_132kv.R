library(testthat)
library(CNAIM)

context("Future Probability of Failure for 132kV UG Cables (Non Pressurised)")

test_that("pof_future_cables_132kv", {

  res <- pof_future_cables_132kv(cable_type = "132kV UG Cable (Non Pressurised)",
                                               sub_division = "Aluminium sheath - Aluminium conductor",
                                               utilisation_pct = 75,
                                               operating_voltage_pct = 50,
                                               sheath_test = "Default",
                                               partial_discharge = "Default",
                                               fault_hist = "Default",
                                               leakage = "Default",
                                               reliability_factor = "Default",
                                               age = 1,
                                               simulation_end_year = 100)

  expect_equal(res$PoF[which(res$year == 12)], 0.0187533)

})
