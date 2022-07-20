library(testthat)
library(CNAIM)

context("POF EHV Fittings")

test_that("33kV Fittings", {
  res <- pof_ehv_fittings(
    ehv_asset_category = "33kV Fittings",
    placement = "Default",
    altitude_m = "Default",
    distance_from_coast_km = "Default",
    corrosion_category_index = "Default",
    age = 10,
    observed_condition_inputs =list("insulator_elec_cond" = list("Condition Criteria: Observed Condition" = "Default"),
                                    "insulator_mech_cond" = list("Condition Criteria: Observed Condition" = "Default"),
                                    "conductor_fitting_cond" = list("Condition Criteria: Observed Condition" = "Default"),
                                    "tower_fitting_cond" = list("Condition Criteria: Observed Condition" = "Default")),
    measured_condition_inputs = list("thermal_imaging" = list("Condition Criteria: Thermal Imaging Result" = "Default"),
                                     "ductor_test" = list("Condition Criteria: Ductor Test Result" = "Default")),
    reliability_factor = "Default") %>% round(5)

  expected_val <- data.frame(pof = 0.00025, chs = 0.91058)

  expect_equal(res, expected_val)

})


context("POF 132kV Fittings")

test_that("132kV Fittings", {
  res <- pof_ehv_fittings(
    ehv_asset_category = "132kV Fittings",
    placement = "Default",
    altitude_m = "Default",
    distance_from_coast_km = "Default",
    corrosion_category_index = "Default",
    age = 10,
    observed_condition_inputs =list("insulator_elec_cond" = list("Condition Criteria: Observed Condition" = "Default"),
                                    "insulator_mech_cond" = list("Condition Criteria: Observed Condition" = "Default"),
                                    "conductor_fitting_cond" = list("Condition Criteria: Observed Condition" = "Default"),
                                    "tower_fitting_cond" = list("Condition Criteria: Observed Condition" = "Default")),
    measured_condition_inputs = list("thermal_imaging" = list("Condition Criteria: Thermal Imaging Result" = "Default"),
                                     "ductor_test" = list("Condition Criteria: Ductor Test Result" = "Default")),
    reliability_factor = "Default") %>% round(5)

  expected_val <- data.frame(pof = 0.00025, chs = 0.91058)

  expect_equal(res, expected_val)

})

