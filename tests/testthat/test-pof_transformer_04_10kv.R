library(testthat)
library(CNAIM)

context("POF Transformer 04 10 KV")

test_that("Transformer 04 10 KV", {
  res <-  pof_transformer_04_10kv(utilisation_pct = "Default",
     placement = "Default",
     altitude_m = "Default",
     distance_from_coast_km = "Default",
     corrosion_category_index = "Default",
     age = 10,
     partial_discharge = "Default",
     temperature_reading = "Default",
     observed_condition = "Default",
     reliability_factor = "Default",
     moisture = "Default",
     acidity = "Default",
   bd_strength = "Default",
   k_value = 0.0077,
   c_value = 1.087,
   normal_expected_life = 55) %>% round(5)

  expected_val <- data.frame(pof = 0.00013, chs = 0.5)

  expect_equal(res, expected_val)
})
