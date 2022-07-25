library(testthat)
library(CNAIM)

context("Current Probability of Failure for 50kV OHL (Tower Line) Conductor")

test_that("pof_ohl_cond_50kv", {

  res <- pof_ohl_cond_50kv(sub_division = "Cu",
                           placement = "Default",
                           altitude_m = "Default",
                           distance_from_coast_km = "Default",
                           corrosion_category_index = "Default",
                           age = 10,
                           conductor_samp = "Default",
                           corr_mon_survey = "Default",
                           visual_cond = "Default",
                           midspan_joints = "Default",
                           reliability_factor = "Default",
                           k_value = 0.0080,
                           c_value = 1.087,
                           normal_expected_life = "Default")

  expect_equal(res, 0.0001706678)

})
