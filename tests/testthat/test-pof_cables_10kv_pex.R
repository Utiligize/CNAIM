library(testthat)
library(CNAIM)

context("POF Cables 10KV Pex")

test_that("Cables 10KV Pex", {
  res <-  pof_cables_10kv_pex(
    utilisation_pct = 80,
    operating_voltage_pct = "Default",
    sheath_test = "Default",
    partial_discharge = "Default",
    fault_hist = "Default",
    reliability_factor = "Default",
    age = 50,
    k_value = 0.0658,
    c_value = 1.087,
    normal_expected_life = 80) %>% round(5)

  expected_val <- data.frame(pof = 0.00578, chs = 2.2379)

  expect_equal(res, expected_val)
})
