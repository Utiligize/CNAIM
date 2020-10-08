library(testthat)
library(CNAIM)

context("Current Probability of Failure for 10kV UG Cable (Oil)")

test_that("pof_ohl_cond_04_10kv", {
  # TODO: verify correctness

  res <- pof_ohl_cond_04_10kv(
    ohl_conductor = "10kV OHL (Tower Line) Conductor",
    utilisation_pct = 110,
    operating_voltage_pct = 80,
    placement = "Default",
    altitude_m = 5,
    distance_from_coast_km = 1,
    corrosion_category_index = "Default",
    age = 10,
    conductor_samp = "Default",
    corr_mon_survey = "Default",
    visual_cond = "Default",
    midspan_joints = "Default",
    reliability_factor = "Default")

  expect_equal(res, 0.0001900938)

})
