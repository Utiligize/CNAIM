library(testthat)
library(CNAIM)

context("POF 50 kV OHL Fittings")

test_that("50kV Fittings", {
  res <- pof_ohl_fittings_50kv(
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
    normal_expected_life = 40)

  expected_val <- 0.0002535621

  expect_equal(res, expected_val)
})



