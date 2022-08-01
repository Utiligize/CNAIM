library(CNAIM)
library(testthat)

context("POF LV UGB")

test_that("LV UGB", {
  res <- pof_lv_ugb(
    lv_asset_category = "LV UGB",
    placement = "Default",
    altitude_m = "Default",
    distance_from_coast_km = "Default",
    corrosion_category_index = "Default",
    age = 10,
    observed_condition_inputs =
      list("steel_cover_and_pit_condition" = list("Condition Criteria: Observed Condition" = "Default"),
           "water_moisture" = list("Condition Criteria: Observed Condition" = "Default"),
           "bell_cond" = list("Condition Criteria: Observed Condition" = "Default"),
           "insulation_cond" = list("Condition Criteria: Observed Condition" = "Default"),
           "signs_heating" = list("Condition Criteria: Observed Condition" = "Default"),
           "phase_barriers" = list("Condition Criteria: Observed Condition" = "Default")),
    measured_condition_inputs = list("opsal_adequacy" = list("Condition Criteria: Operational Adequacy" = "Default")),
    reliability_factor = "Default") %>% round(5)

  expected_val <- data.frame(pof = 0.00013, chs = 0.5)

  expect_equal(res, expected_val)
})

