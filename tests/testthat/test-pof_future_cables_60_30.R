library(testthat)
library(CNAIM)

context("Future Probability of Failure for Cables 60 30 KV")

test_that("pof_future_cables_60_30kv", {

  res <-  pof_future_cables_60_30kv(cable_type = "60kV UG Cable (Non Pressurised)",
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


  expected_value <- readRDS(system.file("testdata/pof_future_cables_60_30.rds", package =
                                          "CNAIM"))

  expect_equal(res, expected_value)

})
