library(testthat)
library(CNAIM)

context("Current Probability of Failure for 33kV UG Cable (Oil)")

test_that("pof_cables_66_33kv", {
  # TODO: verify correctness

  res <- pof_cables_66_33kv(cable_type = "33kV UG Cable (Oil)",
                                     sub_division = "Lead sheath - Copper conductor",
                                     utilisation_pct = 10,
                                     operating_voltage_pct = 10,
                                     sheath_test = "Default",
                                     partial_discharge = "Default",
                                     fault_hist = "Default",
                                     leakage = "Default",
                                     reliability_factor = "Default",
                                     age = 40)

  expect_equal(res, 0.08815061)

})
