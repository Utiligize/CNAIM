library(testthat)
library(CNAIM)

context("Current Probability of Failure for 132kV Sub Cable")

test_that("pof_submarine_cables", {

  res <- pof_submarine_cables(sub_cable_type = "132kV Sub Cable",
                               utilisation_pct = 95,
                               operating_voltage_pct = 80,
                               topography = "High Detrimental Topography",
                               situation = "Laid on bed",
                               wind_wave = 3,
                               intensity = "Low",
                               landlocked = "no",
                               sheath_test = "Failed Minor",
                               partial_discharge = "Medium",
                               fault_hist = "Default",
                               condition_armour = "Good",
                               age = 30,
                               reliability_factor = "Default") %>% round(6)

  expected_val <- data.frame(pof = 0.028709, chs = 7.7)

  expect_equal(res, expected_val)

})

