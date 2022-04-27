library(testthat)
library(CNAIM)

context("Current Probability of Failure for 10-20kV cable, PEX")

test_that("pof_cables_20_10_04kv", {
  # TODO: verify correctness

  res <- pof_cables_20_10_04kv(hv_lv_cable_type = "10-20kV cable, PEX",
                               sub_division = "Aluminium sheath - Aluminium conductor",
                               utilisation_pct = 100,
                               operating_voltage_pct = 100,
                               sheath_test = "Failed Major",
                               partial_discharge = "High",
                               fault_hist = "Default",
                               reliability_factor = "Default",
                               age = 10,
                               normal_expected_life_cable = 80)

  expect_equal(res, 0.05304718)

})

