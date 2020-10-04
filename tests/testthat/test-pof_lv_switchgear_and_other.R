library(testthat)
library(CNAIM)

context("POF LV switch gear and others")

test_that("Circuit breaker", {
  res <- pof_lv_switchgear_and_other(
    lv_asset_category = "LV Circuit Breaker",
    placement = "Default",
    altitude_m = "Default",
    distance_from_coast_km = "Default",
    corrosion_category_index = "Default",
    age = 10,
    observed_condition_inputs =list("external_condition" = list("Condition Criteria: Observed Condition" = "Default")),
    measured_condition_inputs = list("operational_adequacy" = list("Condition Criteria: Operational Adequacy" = "Default")),
    reliability_factor = "Default") %>% round(5)

  expected_value <- 0.00007

  expect_equal(res, expected_value)
})


test_that("LV Pillars", {
  res <- pof_lv_switchgear_and_other(
    lv_asset_category = "LV Pillar (ID)",
    placement = "Default",
    altitude_m = "Default",
    distance_from_coast_km = "Default",
    corrosion_category_index = "Default",
    age = 10,
    observed_condition_inputs =list("compound_leak" = list("Condition Criteria: Observed Condition" = "Default"),
                                    "switchgear_external_condition" = list("Condition Criteria: Observed Condition" = "Default"),
                                    "switchgear_internal_condition_and_operation" = list("Condition Criteria: Observed Condition" = "Default"),
                                    "insulation_condition" = list("Condition Criteria: Observed Condition" = "Default"),
                                    "signs_heating" = list("Condition Criteria: Observed Condition" = "Default"),
                                    "phase_barrier" = list("Condition Criteria: Observed Condition" = "Default")),
    measured_condition_inputs = list("operational_adequacy" = list("Condition Criteria: Operational Adequacy" = "Default")),
    reliability_factor = "Default") %>% round(6)

  expected_value <- 0.000079

  expect_equal(res, expected_value)
})



test_that("LV Board(WM)", {
  res <- pof_lv_switchgear_and_other(
    lv_asset_category = "LV Board (WM)",
    placement = "Default",
    altitude_m = "Default",
    distance_from_coast_km = "Default",
    corrosion_category_index = "Default",
    age = 10,
    observed_condition_inputs =list("compound_leak" = list("Condition Criteria: Observed Condition" = "Default"),
                                    "switchgear_external_condition" = list("Condition Criteria: Observed Condition" = "Default"),
                                    "switchgear_internal_condition_and_operation" = list("Condition Criteria: Observed Condition" = "Default")),
    measured_condition_inputs = list("operational_adequacy" = list("Condition Criteria: Operational Adequacy" = "Default"),
                                     "security" = list("Condition Criteria: Security" = "Default")),
    reliability_factor = "Default") %>% round(6)

  expected_value <- 0.00015

  expect_equal(res, expected_value)
})
