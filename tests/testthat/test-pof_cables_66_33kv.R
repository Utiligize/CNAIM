library(testthat)
library(CNAIM)

context("Current Probability of Failure for 33kV UG Cable (Oil)")

test_that("pof_cables_66_33kv", {

  res <- pof_cables_66_33kv(cable_type = "33kV UG Cable (Oil)",
                                     sub_division = "Lead sheath - Copper conductor",
                                     utilisation_pct = 10,
                                     operating_voltage_pct = 10,
                                     sheath_test = "Default",
                                     partial_discharge = "Default",
                                     fault_hist = "Default",
                                     leakage = "Default",
                                     reliability_factor = "Default",
                                     age = 40) %>% round(6)

  expected_val <- data.frame(pof = 0.088151, chs = 1.385358)

  expect_equal(res, expected_val)
})
