library(testthat)
library(CNAIM)

context("POF Cables 60 30 KV")

test_that("POF Cables 60 30 KV", {
  res <- pof_cables_60_30kv(cable_type = "66kV UG Cable (Non Pressurised)",
                            sub_division = "Lead sheath - Copper conductor",
                            utilisation_pct = 80,
                            operating_voltage_pct = 60,
                            sheath_test = "Default",
                            partial_discharge = "Default",
                            fault_hist = "Default",
                            leakage = "Default",
                            reliability_factor = "Default",
                            age = 50) %>% round(5)

  expected_val <- data.frame(pof = 0.64666, chs = 2.90174)

  expect_equal(res, expected_val)
})
