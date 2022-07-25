library(testthat)
library(CNAIM)

context("POF Board 0.4 kV")

test_that("LV Board(WM)", {
  res <- pof_board_04kv(
    placement = "Default",
    altitude_m = "Default",
    distance_from_coast_km = "Default",
    corrosion_category_index = "Default",
    age = 10,
    observed_condition_inputs =
      list("external_cond" =
             list("Condition Criteria: Observed Condition" = "Default"),
           "compound_leaks" = list("Condition Criteria: Observed Condition" = "Default"),
           "internal_cond" = list("Condition Criteria: Observed Condition" = "Default"),
           "insulation" = list("Condition Criteria: Observed Condition" = "Default"),
           "signs_heating" = list("Condition Criteria: Observed Condition" = "Default"),
           "phase_barriers" = list("Condition Criteria: Observed Condition" = "Default")),
    measured_condition_inputs =
      list("opsal_adequacy" =
             list("Condition Criteria: Operational Adequacy" = "Default")),
    reliability_factor = "Default",
    k_value = 0.0069,
    c_value = 1.087,
    normal_expected_life = 60)

  expected_value <- 0.0001185388

  expect_equal(res, expected_value)
})
