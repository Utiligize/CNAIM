library(testthat)
library(CNAIM)

context("POF Cables 04 KV Pex")

test_that("Cables 04KV Pex", {
  res <-  pof_cables_04kv_pex(
    utilisation_pct = 80,
    operating_voltage_pct = "Default",
    sheath_test = "Default",
    partial_discharge = "Default",
    fault_hist = "Default",
    reliability_factor = "Default",
    age = 50,
    k_value = 0.0658,
    c_value = 1.087,
    normal_expected_life = 80)


  expected_val <- data.frame(pof = 0.005784, chs = 2.237899)

  expect_equal(res %>% round(6), expected_val)
})
