library(testthat)
library(CNAIM)

context("Future Probability of Failure for a Transformer")

test_that("pof_future_transformer_11_20kv", {
  # TODO: verify correctness

  res <-
    pof_future_transformer_11_20kv(
      hv_transformer_type = "6.6/11kV Transformer (GM)",
      utilisation_pct = "Default",
      placement = "Default",
      altitude_m = "Default",
      distance_from_coast_km = "Default",
      corrosion_category_index = "Default",
      age=12,
      partial_discharge = "Default",
      oil_acidity = "Default",
      temperature_reading = "Default",
      observed_condition = "Default",
      reliability_factor = "Default",
      simulation_end_year = 100)

  expect_equal(res$PoF[which(res$year == 50)], 0.0013459025)

})
