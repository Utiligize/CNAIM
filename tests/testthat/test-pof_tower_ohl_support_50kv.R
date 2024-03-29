library(testthat)
library(CNAIM)

context("POF Tower OHL SUPPORT 50 KV")

test_that("Tower OHL SUPPORT 50 KV", {
  res <-  pof_tower_ohl_support_50kv(
     number_of_operations = "Default",
     placement = "Default",
     altitude_m = "Default",
     distance_from_coast_km = "Default",
     corrosion_category_index = "Default",
     age = 10,
     paint_type = "Paint System - Galvanising",
     foundation_type = "Foundation - Earth Grillage",
     observed_condition_inputs_steelwork =
     list("tower_legs" = list("Condition Criteria: Observed Condition" = "Default"),
     "tower_bracings" = list("Condition Criteria: Observed Condition" = "Default"),
   "tower_crossarms" = list("Condition Criteria: Observed Condition" = "Default"),
   "tower_peak" = list("Condition Criteria: Observed Condition" = "Default")),
   observed_condition_inputs_paint =
   list("paintwork_cond" = list("Condition Criteria: Observed Condition" = "Default")),
   observed_condition_inputs_foundation =
   list("foundation_cond" = list("Condition Criteria: Observed Condition" = "Default")),
   reliability_factor = "Default",
   k_value = 0.0545,
   c_value = 1.087,
   normal_expected_life = "Default") %>% round(5)

  expected_val <- data.frame(pof = 0.00113, chs = 0.67475)

  expect_equal(res, expected_val)
})
