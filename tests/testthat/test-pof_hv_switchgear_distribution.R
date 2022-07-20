library(testthat)
library(CNAIM)

context("POF HV switchgear distribution")

test_that("Default case", {
  res <- pof_hv_switchgear_distribution(
    hv_asset_category = "6.6/11kV CB (GM) Secondary",
    placement = "Default",
    altitude_m = "Default",
    distance_from_coast_km = "Default",
    corrosion_category_index = "Default",
    age = 10,
    observed_condition_inputs =list("external_condition" = list("Condition Criteria: Observed Condition" = "Default"),
                                    "oil_gas" = list("Condition Criteria: Observed Condition" = "Default"),
                                    "thermo_assment" = list("Condition Criteria: Observed Condition" = "Default"),
                                    "internal_condition" = list("Condition Criteria: Observed Condition" = "Default"),
                                    "indoor_env" = list("Condition Criteria: Observed Condition" = "Default")),
    measured_condition_inputs = list("partial_discharge" = list("Condition Criteria: Partial Discharge Test Results" = "Default"),
                                     "ductor_test" = list("Condition Criteria: Ductor Test Results" = "Default"),
                                     "oil_test" = list("Condition Criteria: Oil Test Results" = "Default"),
                                     "temp_reading" = list("Condition Criteria: Temperature Readings" = "Default"),
                                     "trip_test" = list("Condition Criteria: Trip Timing Test Result" = "Default")),
    reliability_factor = "Default") %>% round(5)

  expected_val <- data.frame(pof = 0.00015, chs = 0.74836)

  expect_equal(res, expected_val)
})
