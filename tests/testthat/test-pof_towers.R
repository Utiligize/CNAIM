library(testthat)
library(CNAIM)

context("POF Towers : EHV")

test_that("33kV case", {
  res <- pof_towers(
    tower_asset_category = "33kV Tower",
    number_of_operations = "Default",
    placement = "Default",
    altitude_m = "Default",
    distance_from_coast_km = "Default",
    corrosion_category_index = "Default",
    age = 10,
    paint_type = "Paint System - Galvanising",
    foundation_type = "Foundation - Earth Grillage",
    observed_condition_inputs_steelwork =list("tower_legs" = list("Condition Criteria: Observed Condition" = "Default"),
    "tower_bracings" = list("Condition Criteria: Observed Condition" = "Default"),
    "tower_crossarms" = list("Condition Criteria: Observed Condition" = "Default"),
    "tower_peak" = list("Condition Criteria: Observed Condition" = "Default")),
    observed_condition_inputs_paint = list("paintwork_cond" = list("Condition Criteria: Observed Condition" = "Default")),
    observed_condition_inputs_foundation = list("foundation_cond" = list("Condition Criteria: Observed Condition" = "Default")),
  reliability_factor = "Default") %>% round(6)

  expected_val <- data.frame(pof = 0.001127, chs = 0.674752)

  expect_equal(res, expected_val)
})


context("POF Towers : 132kV")

test_that("132kV case", {
  res <- pof_towers(
    tower_asset_category = "132kV Tower",
    number_of_operations = "Default",
    placement = "Default",
    altitude_m = "Default",
    distance_from_coast_km = "Default",
    corrosion_category_index = "Default",
    age = 10,
    paint_type = "Paint System - Galvanising",
    foundation_type = "Foundation - Earth Grillage",
    observed_condition_inputs_steelwork =list("tower_legs" = list("Condition Criteria: Observed Condition" = "Default"),
                                              "tower_bracings" = list("Condition Criteria: Observed Condition" = "Default"),
                                              "tower_crossarms" = list("Condition Criteria: Observed Condition" = "Default"),
                                              "tower_peak" = list("Condition Criteria: Observed Condition" = "Default")),
    observed_condition_inputs_paint = list("paintwork_cond" = list("Condition Criteria: Observed Condition" = "Default")),
    observed_condition_inputs_foundation = list("foundation_cond" = list("Condition Criteria: Observed Condition" = "Default")),
    reliability_factor = "Default") %>% round(6)

  expected_val <- data.frame(pof = 0.001127, chs = 0.674752)

  expect_equal(res, expected_val)
})
