library(testthat)
library(CNAIM)

context("Future Probability of Failure for a RTU")

test_that("pof_future_rtu", {

  res <-
    pof_future_rtu(
      placement = "Default",
      altitude_m = "Default",
      distance_from_coast_km = "Default",
      corrosion_category_index = "Default",
      age = 1,
      observed_condition_inputs =
        list("external_condition" =
               list("Condition Criteria: Observed Condition" = "Default"),
             "oil_gas" = list("Condition Criteria: Observed Condition" = "Default"),
             "thermo_assment" = list("Condition Criteria: Observed Condition" = "Default"),
             "internal_condition" = list("Condition Criteria: Observed Condition" = "Default"),
             "indoor_env" = list("Condition Criteria: Observed Condition" = "Default")),
      measured_condition_inputs =
        list("partial_discharge" =
               list("Condition Criteria: Partial Discharge Test Results" = "Default"),
             "ductor_test" = list("Condition Criteria: Ductor Test Results" = "Default"),
             "oil_test" = list("Condition Criteria: Oil Test Results" = "Default"),
             "temp_reading" = list("Condition Criteria: Temperature Readings" = "Default"),
             "trip_test" = list("Condition Criteria: Trip Timing Test Result" = "Default")),
      reliability_factor = "Default",
      k_value = 0.128,
      c_value = 1.087,
      normal_expected_life = 20,
      simulation_end_year = 100)

  expect_equal(res$PoF[which(res$year == 25)], 0.26977956)

})
