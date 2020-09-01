library(testthat)
library(CNAIM)

context("Future Probability of Failure for a Transformer")

test_that("pof_future_transformer_11kv", {
  # TODO: verify correctness
  expect_equal(pof_transformer_11kv(utilisation_pct = "Default",
                                    placement = "Default",
                                    altitude_m = "Default",
                                    distance_from_coast_km = "Default",
                                    corrosion_category_index = "Default",
                                    age = 1,
                                    partial_discharge = "Default",
                                    oil_acidity = "Default",
                                    temperature_reading = "Default",
                                    observed_condition = "Default",
                                    reliability_factor = "Default"), 0.000225564)
})
