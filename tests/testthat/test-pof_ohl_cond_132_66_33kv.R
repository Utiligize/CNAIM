library(testthat)
library(CNAIM)

context("Current Probability of Failure for 66kV OHL (Tower Line) Conductor")

test_that("pof_ohl_cond_132_66_33kv", {
  # TODO: verify correctness

  res <- pof_ohl_cond_132_66_33kv(ohl_conductor = "66kV OHL (Tower Line) Conductor",
                                  sub_division = "Cu",
                                  placement = "Default",
                                  altitude_m = 130,
                                  distance_from_coast_km = 1,
                                  corrosion_category_index = 1,
                                  age = 12,
                                  conductor_samp = "Medium/Normal",
                                  corr_mon_survey = "Medium/Normal",
                                  visual_cond = "Normal Wear",
                                  midspan_joints = 3,
                                  reliability_factor = "Default")

  expect_equal(res, 0.004837129)

})

