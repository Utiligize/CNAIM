library(testthat)
library(CNAIM)

context("POF switchgear 30 and 60 kV")

test_that("30 kV category", {
  res <- pof_switchgear_30_60kv(
    asset_type = "30kV",
    number_of_operations = "Default",
    placement = "Default",
    altitude_m = "Default",
    distance_from_coast_km = "Default",
    corrosion_category_index = "Default",
    age = 10,
    observed_condition_inputs =
      list("external_condition" =
             list("Condition Criteria: Observed Condition" = "Default"),
           "oil_gas" = list("Condition Criteria: Observed Condition" = "Default"),
           "thermo_assment" = list("Condition Criteria: Observed Condition" = "Default"),
           "internal_condition" = list("Condition Criteria: Observed Condition" = "Default"),
           "indoor_env" = list("Condition Criteria: Observed Condition" = "Default"),
           "support_structure" = list("Condition Criteria: Observed Condition" = "Default")),
    measured_condition_inputs =
      list("partial_discharge" =
             list("Condition Criteria: Partial Discharge Test Results" = "Default"),
           "ductor_test" = list("Condition Criteria: Ductor Test Results" = "Default"),
           "oil_test" = list("Condition Criteria: Oil Test Results" = "Default"),
           "temp_reading" = list("Condition Criteria: Temperature Readings" = "Default"),
           "trip_test" = list("Condition Criteria: Trip Timing Test Result" = "Default"),
           "ir_test" = list("Condition Criteria: IR Test Results" = "Default" )),
    reliability_factor = "Default",
    k_value = "Default",
    c_value = 1.087,
    normal_expected_life = 55) %>% round(6)

  expected_val <- 0.000498

  expect_equal(res, expected_val)
})



test_that("60 kV category", {
  res <- pof_switchgear_30_60kv(
         asset_type = "60kV",
         number_of_operations = "Default",
         placement = "Default",
         altitude_m = "Default",
         distance_from_coast_km = "Default",
         corrosion_category_index = "Default",
         age = 10,
         observed_condition_inputs =
            list("external_condition" =
                   list("Condition Criteria: Observed Condition" = "Default"),
                 "oil_gas" = list("Condition Criteria: Observed Condition" = "Default"),
                 "thermo_assment" = list("Condition Criteria: Observed Condition" = "Default"),
                 "internal_condition" = list("Condition Criteria: Observed Condition" = "Default"),
                 "indoor_env" = list("Condition Criteria: Observed Condition" = "Default"),
                 "support_structure" = list("Condition Criteria: Observed Condition" = "Default")),
         measured_condition_inputs =
            list("partial_discharge" =
                   list("Condition Criteria: Partial Discharge Test Results" = "Default"),
                 "ductor_test" = list("Condition Criteria: Ductor Test Results" = "Default"),
                 "oil_test" = list("Condition Criteria: Oil Test Results" = "Default"),
                 "temp_reading" = list("Condition Criteria: Temperature Readings" = "Default"),
                 "trip_test" = list("Condition Criteria: Trip Timing Test Result" = "Default"),
                 "ir_test" = list("Condition Criteria: IR Test Results" = "Default" )),
         reliability_factor = "Default",
         k_value = "Default",
         c_value = 1.087,
         normal_expected_life = 55) %>% round(5)

  expected_val <- 0.00114

  expect_equal(res, expected_val)
})
