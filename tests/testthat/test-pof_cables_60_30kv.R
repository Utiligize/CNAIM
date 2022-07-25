library(testthat)
library(CNAIM)

context("Current Probability of Failure for 60kV UG Cable (Non Pressurised)")

test_that("pof_cables_60_30kv", {

  res <- pof_cables_60_30kv(cable_type = "60kV UG Cable (Non Pressurised)",
                            sub_division = "Lead sheath - Copper conductor",
                            utilisation_pct = 80,
                            operating_voltage_pct = 60,
                            sheath_test = "Default",
                            partial_discharge = "Default",
                            fault_hist = "Default",
                            leakage = "Default",
                            reliability_factor = "Default",
                            age = 50)

  expect_equal(res, 0.004274284)

})
