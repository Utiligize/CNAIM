library(testthat)
library(CNAIM)

context("Current Probability of Failure for a Transformer")

test_that("pof_transformer_11_20kv", {
  res <- pof_transformer_11_20kv(hv_transformer_type = "6.6/11kV Transformer (GM)",
                                       utilisation_pct = "Default",
                                       placement = "Default",
                                       altitude_m = "Default",
                                       distance_from_coast_km = "Default",
                                       corrosion_category_index = "Default",
                                       age = 1,
                                       partial_discharge = "Default",
                                       oil_acidity = "Default",
                                       temperature_reading = "Default",
                                       observed_condition = "Default",
                                       reliability_factor = "Default") %>% round(5)

  expected_val <- data.frame(pof = 0.00013, chs = 0.5)

  expect_equal(res, expected_val)
})


