library(testthat)
library(CNAIM)

context("Future Probability of Failure for Transformer 04 KV")

test_that("pof_future_transformer_04_10kv", {

  res <-  pof_future_transformer_04_10kv(utilisation_pct = "Default",
     placement = "Default",
     altitude_m = "Default",
     distance_from_coast_km = "Default",
     corrosion_category_index = "Default",
     age = 20,
     partial_discharge = "Default",
     temperature_reading = "Default",
     observed_condition = "Default",
     reliability_factor = "Default",
     moisture = "Default",
     acidity = "Default",
   bd_strength = "Default",
   k_value = 0.0077,
   c_value = 1.087,
   normal_expected_life = 55,
   simulation_end_year = 100)

  expected_value <- readRDS(system.file("testdata/pof_future_transformer_04_10kv.rds", package =
                                          "CNAIM"))

  expect_equal(res, expected_value)

})
