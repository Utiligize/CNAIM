library(testthat)
library(CNAIM)

context("POF HV Poles")

test_that("20kV case", {
  res <- pof_poles(
    pole_asset_category = "20kV Poles",
    sub_division = "Wood",
    placement = "Default",
    altitude_m = "Default",
    distance_from_coast_km = "Default",
    corrosion_category_index = "Default",
    age = 10,
    observed_condition_inputs =list("visual_pole_cond" = list("Condition Criteria: Pole Top Rot Present?" = "Default"),
                                    "pole_leaning" = list("Condition Criteria: Pole Leaning?" = "Default"),
                                    "bird_animal_damage" = list("Condition Criteria: Bird/Animal Damage?" = "Default"),
                                    "top_rot"  = list("Condition Criteria: Pole Top Rot Present?" = "Default")),
    measured_condition_inputs = list("pole_decay" = list("Condition Criteria: Degree of Decay/Deterioration" = "Default")),
    reliability_factor = "Default") %>% round(6)

  expected_val <- 0.00049

  expect_equal(res, expected_val)
})


test_that("6.6/11kV case", {
  res <- pof_poles(
    pole_asset_category = "6.6/11kV Poles",
    sub_division = "Wood",
    placement = "Default",
    altitude_m = "Default",
    distance_from_coast_km = "Default",
    corrosion_category_index = "Default",
    age = 10,
    observed_condition_inputs =list("visual_pole_cond" = list("Condition Criteria: Pole Top Rot Present?" = "Default"),
                                    "pole_leaning" = list("Condition Criteria: Pole Leaning?" = "Default"),
                                    "bird_animal_damage" = list("Condition Criteria: Bird/Animal Damage?" = "Default"),
                                    "top_rot"  = list("Condition Criteria: Pole Top Rot Present?" = "Default")),
    measured_condition_inputs = list("pole_decay" = list("Condition Criteria: Degree of Decay/Deterioration" = "Default")),
    reliability_factor = "Default") %>% round(6)

  expected_val <- 0.00049

  expect_equal(res, expected_val)
})


context("POF EHV Poles")

test_that("33kV case", {
  res <- pof_poles(
    pole_asset_category = "33kV Pole",
    sub_division = "Wood",
    placement = "Default",
    altitude_m = "Default",
    distance_from_coast_km = "Default",
    corrosion_category_index = "Default",
    age = 10,
    observed_condition_inputs =list("visual_pole_cond" = list("Condition Criteria: Pole Top Rot Present?" = "Default"),
                                    "pole_leaning" = list("Condition Criteria: Pole Leaning?" = "Default"),
                                    "bird_animal_damage" = list("Condition Criteria: Bird/Animal Damage?" = "Default"),
                                    "top_rot"  = list("Condition Criteria: Pole Top Rot Present?" = "Default")),
    measured_condition_inputs = list("pole_decay" = list("Condition Criteria: Degree of Decay/Deterioration" = "Default")),
    reliability_factor = "Default") %>% round(6)

  expected_val <- 0.00049

  expect_equal(res, expected_val)
})


test_that("66kV case", {
  res <- pof_poles(
    pole_asset_category = "66kV Pole",
    sub_division = "Wood",
    placement = "Default",
    altitude_m = "Default",
    distance_from_coast_km = "Default",
    corrosion_category_index = "Default",
    age = 10,
    observed_condition_inputs =list("visual_pole_cond" = list("Condition Criteria: Pole Top Rot Present?" = "Default"),
                                    "pole_leaning" = list("Condition Criteria: Pole Leaning?" = "Default"),
                                    "bird_animal_damage" = list("Condition Criteria: Bird/Animal Damage?" = "Default"),
                                    "top_rot"  = list("Condition Criteria: Pole Top Rot Present?" = "Default")),
    measured_condition_inputs = list("pole_decay" = list("Condition Criteria: Degree of Decay/Deterioration" = "Default")),
    reliability_factor = "Default") %>% round(6)

  expected_val <- 0.00049

  expect_equal(res, expected_val)
})


context("POF LV Poles")

test_that("LV case", {
  res <- pof_poles(
    pole_asset_category = "LV Poles",
    sub_division = "Steel",
    placement = "Default",
    altitude_m = "Default",
    distance_from_coast_km = "Default",
    corrosion_category_index = "Default",
    age = 10,
    observed_condition_inputs =list("visual_pole_cond" = list("Condition Criteria: Pole Top Rot Present?" = "Default"),
                                    "pole_leaning" = list("Condition Criteria: Pole Leaning?" = "Default"),
                                    "bird_animal_damage" = list("Condition Criteria: Bird/Animal Damage?" = "Default"),
                                    "top_rot"  = list("Condition Criteria: Pole Top Rot Present?" = "Default")),
    measured_condition_inputs = list("pole_decay" = list("Condition Criteria: Degree of Decay/Deterioration" = "Default")),
    reliability_factor = "Default") %>% round(6)

  expected_val <- 0.00049

  expect_equal(res, expected_val)
})
