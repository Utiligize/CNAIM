library(testthat)
library(CNAIM)

context("POF HV Poles")

test_that("Default case", {
  res <- pof_hv_poles(
    hv_asset_category = "20kV Poles",
    sub_division = "Wood",
    placement = "Default",
    altitude_m = "Default",
    distance_from_coast_km = "Default",
    corrosion_category_index = "Default",
    age = 10,
    observed_condition_inputs =list("visual_pole_cond" = list("Condition Criteria: Pole Top Rot Present?" = "Default"),
                                    "pole_leaning" = list("Condition Criteria: Pole Leaning?" = "Default"),
                                    "bird_animal_damage" = list("Condition Criteria: Bird/Animal Damage?" = "Default")),
    measured_condition_inputs = list("pole_decay" = list("Condition Criteria: Degree of Decay/Deterioration" = "Default")),
    reliability_factor = "Default") %>% round(6)

  expected_val <- 0.00049

  expect_equal(res, expected_val)
})
