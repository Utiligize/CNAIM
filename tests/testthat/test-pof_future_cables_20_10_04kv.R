library(testthat)
library(CNAIM)

context("Future Probability of Failure for 10-20kV cable, PEX")

test_that("pof_future_cables_20_10_04kv", {
  # TODO: verify correctness

  res <- pof_future_cables_20_10_04kv(hv_lv_cable_type = "10-20kV cable, PEX",
                                   sub_division = "Aluminium sheath - Aluminium conductor",
                                   utilisation_pct = 100,
                                   operating_voltage_pct = 106,
                                   sheath_test = "Default",
                                   partial_discharge = "Default",
                                   fault_hist = "Default",
                                   reliability_factor = "Default",
                                   age = 30,
                                   simulation_end_year = 100)

  expect_equal(res$PoF[which(res$years == 65)], 0.029754193)

})


