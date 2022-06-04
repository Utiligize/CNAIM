library(testthat)
library(CNAIM)

context("Future Probability of Failure for 132kV Transformer (GM)")

test_that("pof_future_transformer_132kv", {

  res <-
    pof_future_transformer_132kv(transformer_type = "132kV Transformer (GM)",
                                 year_of_manufacture = 1980,
                                 utilisation_pct = "Default",
                                 no_taps = "Default",
                                 placement = "Default",
                                 altitude_m = "Default",
                                 distance_from_coast_km = "Default",
                                 corrosion_category_index = "Default",
                                 age_tf = 43,
                                 age_tc = 43,simulation_end_year = 100)

  expect_equal(res$PoF[which(res$year == 12)], 0.03657150)

})

