library(testthat)
library(CNAIM)

context("Future Probability of Failure for a EHV submarine cable")

test_that("pof_future_submarine_cables", {
  # TODO: verify correctness

  res <- pof_future_submarine_cables(sub_cable_type = "EHV Sub Cable",
                                     utilisation_pct = 95,
                                     operating_voltage_pct = 80,
                                     topography = "High Detrimental Topography",
                                     sitution = "Laid on bed",
                                     wind_wave = 3,
                                     intensity = "Low",
                                     landlocked = "no",
                                     sheath_test = "Failed Minor",
                                     partial_discharge = "Medium",
                                     fault_hist = "Default",
                                     condition_armour = "Good",
                                     age = 30,
                                     reliability_factor = "Default",
                                     simulation_end_year = 100)

  expect_equal(res$PoF[which(res$year == 1)], 0.03367656)

})
