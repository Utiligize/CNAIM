library(testthat)
library(CNAIM)

context("Future Probability of Failure for a OHL fittings 50 kV")

test_that("pof_future_ohl_fittings_50kv", {

  res <-
    pof_future_ohl_fittings_50kv(
      placement = "Default",
      altitude_m = "Default",
      distance_from_coast_km = "Default",
      corrosion_category_index = "Default",
      age = 10,
      observed_condition_inputs =
        list("insulator_elec_cond" =
               list("Condition Criteria: Observed Condition" = "Default"),
             "insulator_mech_cond" =
               list("Condition Criteria: Observed Condition" = "Default"),
             "conductor_fitting_cond" =
               list("Condition Criteria: Observed Condition" = "Default"),
             "tower_fitting_cond" =
               list("Condition Criteria: Observed Condition" = "Default")),
      measured_condition_inputs =
        list("thermal_imaging" =
               list("Condition Criteria: Thermal Imaging Result" = "Default"),
             "ductor_test" = list("Condition Criteria: Ductor Test Result" = "Default")),
      reliability_factor = "Default",
      k_value = 0.0096,
      c_value = 1.087,
      normal_expected_life = 40,
      simulation_end_year = 100)


  expect_equal(res$PoF[which(res$year == 20)], 0.002736043)

})
