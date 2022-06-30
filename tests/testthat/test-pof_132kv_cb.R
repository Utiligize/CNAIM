library(testthat)
library(CNAIM)

context("POF 132kV CB")

test_that("132kV CB", {
  res <- pof_132kv_cb(
    cb_asset_category = "132kV CB (Air Insulated Busbars)(ID) (GM)",
    placement = "Default",
    number_of_operations = "Default",
    altitude_m = "Default",
    distance_from_coast_km = "Default",
    corrosion_category_index = "Default",
    age = 10,
    observed_condition_inputs =list("external_condition" = list("Condition Criteria: Observed Condition" = "Default"),
                                    "oil_gas" = list("Condition Criteria: Observed Condition" = "Default"),
                                    "thermo_assment" = list("Condition Criteria: Observed Condition" = "Default"),
                                    "internal_condition" = list("Condition Criteria: Observed Condition" = "Default"),
                                    "indoor_env" = list("Condition Criteria: Observed Condition" = "Default"),
                                    "support_structure" = list("Condition Criteria: Observed Condition" = "Default"),
                                    "air_systems" = list("Condition Criteria: Observed Condition" = "Default")),
    measured_condition_inputs = list("partial_discharge" = list("Condition Criteria: Partial Discharge Test Results" = "Default"),
                                     "ductor_test" = list("Condition Criteria: Ductor Test Results" = "Default"),
                                     "oil_test" = list("Condition Criteria: Oil Test/ Gas Test Results" = "Default"),
                                     "temp_reading" = list("Condition Criteria: Temperature Readings" = "Default"),
                                     "trip_test" = list("Condition Criteria: Trip Timing Test Result" = "Default"),
                                     "ir_test" = list("Condition Criteria: IR Test Results" = "Default" )),
    reliability_factor = "Default") %>% round(5)

  expected_val <- 0.00094

  expect_equal(res, expected_val)
})
