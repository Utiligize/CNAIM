library(testthat)
library(CNAIM)

context("Future Probability of Failure for 0.4kV OHL (Tower Line) Conductor")

test_that("pof_future_ohl_cond_04_10kv", {
  # TODO: verify correctness

  res <- pof_future_ohl_cond_04_10kv(
    ohl_conductor = "0.4kV OHL (Tower Line) Conductor",
    utilisation_pct = 50,
    operating_voltage_pct = 80,
    placement = "Default",
    altitude_m = 1000,
    distance_from_coast_km = 60,
    corrosion_category_index = "Default",
    age = 10,
    conductor_samp = "Default",
    corr_mon_survey = "Default",
    visual_cond = "Default",
    midspan_joints = "Default",
    reliability_factor = "Default",
    normal_expected_life_cond = 30,
    simulation_end_year = 100)

  expect_equal(res$PoF[which(res$year == 50)], 0.0698149686)

})
