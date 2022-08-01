library(testthat)
library(CNAIM)

context("Future Probability of Failure for 66kV UG Cable (Non Pressurised)")

test_that("pof_future_cables_66_33kv", {

  res <- pof_future_cables_66_33kv(cable_type = "66kV UG Cable (Non Pressurised)",
                                   sub_division = "Aluminium sheath - Aluminium conductor",
                                   utilisation_pct = 75,
                                   operating_voltage_pct = 70,
                                   sheath_test = "Pass",
                                   partial_discharge = "Low",
                                   fault_hist = "No historic faults recorded",
                                   leakage = "Default",
                                   reliability_factor = "Default",
                                   age = 10,
                                   simulation_end_year = 100)

  expected_value <- readRDS(system.file("testdata/pof_future_cables_66_33kv.rds", package =
                                           "CNAIM"))

  expect_equal(res, expected_value)

})
