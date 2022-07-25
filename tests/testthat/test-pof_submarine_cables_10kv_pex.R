library(testthat)
library(CNAIM)

context("Current Probability of Failure for 10kV Sub Cable (PEX)")

test_that("pof_submarine_cables_10kv_pex", {

  res <- pof_submarine_cables_10kv_pex(utilisation_pct = "Default",
                                       operating_voltage_pct = "Default",
                                       topography = "Default",
                                       situation = "Default",
                                       wind_wave = "Default",
                                       intensity = "Default",
                                       landlocked = "no",
                                       sheath_test = "Default",
                                       partial_discharge = "Default",
                                       fault_hist = "Default",
                                       condition_armour = "Default",
                                       age = 10,
                                       reliability_factor = "Default",
                                       k_value = 0.0202,
                                       c_value = 1.087,
                                       normal_expected_life = 60)

  expect_equal(res, 0.0003470267)

})

