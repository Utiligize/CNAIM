library(testthat)
library(CNAIM)

context("Future Probability of Failure for a 50 kV OHL support poles")

test_that("pof_future_poles_support_50kv", {

  res <-
    pof_future_poles_ohl_support_50kv(
      sub_division = "Wood",
      placement = "Default",
      altitude_m = "Default",
      distance_from_coast_km = "Default",
      corrosion_category_index = "Default",
      age = 10,
      observed_condition_inputs =
        list("visual_pole_cond" =
               list("Condition Criteria: Pole Top Rot Present?" = "Default"),
             "pole_leaning" = list("Condition Criteria: Pole Leaning?" = "Default"),
             "bird_animal_damage" =
               list("Condition Criteria: Bird/Animal Damage?" = "Default"),
             "top_rot"  = list("Condition Criteria: Pole Top Rot Present?" = "Default")),
      measured_condition_inputs =
        list("pole_decay" =
               list("Condition Criteria: Degree of Decay/Deterioration" = "Default")),
      reliability_factor = "Default",
      k_value = 0.0285,
      c_value = 1.087,
      normal_expected_life = "Default",
      simulation_end_year = 100)


  expect_equal(res$PoF[which(res$year == 20)], 0.008122628)

})
