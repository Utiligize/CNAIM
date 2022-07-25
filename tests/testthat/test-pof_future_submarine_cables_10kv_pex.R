library(testthat)
library(CNAIM)

context("Future Probability of Failure for 10kV submarine cable (PEX)")

test_that("pof_future_submarine_cables_10kv_pex", {

  res <- pof_future_submarine_cables_10kv_pex(utilisation_pct = "Default",
                                              operating_voltage_pct = "Default",
                                              topography = "Default",
                                              sitution = "Default",
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
                                              normal_expected_life = 60,
                                              simulation_end_year = 100)

  expect_equal(res$PoF[which(res$year == 1)], 0.005757091 )

})
