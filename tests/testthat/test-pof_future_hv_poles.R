library(testthat)
library(CNAIM)

context("Future Probability of Failure for Cables 04 KV pex")

test_that("pof_future_cables_04kv_pex", {

  res <-  pof_future_poles(
     pole_asset_category = "20kV Poles",
     sub_division = "Wood",
     placement = "Default",
     altitude_m = "Default",
     distance_from_coast_km = "Default",
     corrosion_category_index = "Default",
     age = 10,
     observed_condition_inputs =
     list("visual_pole_cond" =
     list("Condition Criteria: Pole Top Rot Present?" = "Default"),
     "pole_leaning" = list("Condition Criteria: Pole Leaning?" = "Default"),
   "bird_animal_damage" =
   list("Condition Criteria: Bird/Animal Damage?" = "Default"),
   "top_rot"  = list("Condition Criteria: Pole Top Rot Present?" = "Default")),
   pole_decay = "Default",
   reliability_factor = "Default",
   simulation_end_year = 100)


  expected_value <- readRDS(system.file("testdata/pof_future_poles.rds", package =
                                          "CNAIM"))

  expect_equal(res, expected_value)

})
