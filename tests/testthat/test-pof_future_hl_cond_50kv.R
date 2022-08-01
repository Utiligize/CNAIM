library(testthat)
library(CNAIM)

context("Future Probability of Failure for OHL conductor 50kv")

test_that("pof_future_ohl_cond_50kv", {

  res <-  pof_future_ohl_cond_50kv(
     sub_division = "Cu",
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
   normal_expected_life = "Default",
   simulation_end_year = 100)


  expected_value <- readRDS(system.file("testdata/pof_future_ohl_cond_50kv.rds", package =
                                          "CNAIM"))

  expect_equal(res, expected_value)

})
