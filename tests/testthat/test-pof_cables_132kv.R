library(testthat)
library(CNAIM)

context("Current Probability of Failure for 132kV UG Cable (Non Pressurised)")

test_that("pof_cables_123kv", {

  res <- pof_cables_132kv(cable_type = "132kV UG Cable (Non Pressurised)",
                                    sub_division = "Lead sheath - Copper conductor",
                                    utilisation_pct = 80,
                                    operating_voltage_pct = 60,
                                    sheath_test = "Default",
                                    partial_discharge = "Default",
                                    fault_hist = "Default",
                                    leakage = "Default",
                                    reliability_factor = "Default",
                                    age = 50) %>% round(5)

  expected_val <- data.frame(pof = 0.00427, chs = 1.86955)

  expect_equal(res, expected_val)

})
