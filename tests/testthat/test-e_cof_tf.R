library(testthat)
library(CNAIM)

context("Environmental Consequences of Failure for transformers")

test_that("e_cof_tf", {
  # TODO: verify correctness
  expect_equal(e_cof_tf(asset_type_tf = "6.6/11kV Transformer (GM)",
                        rated_capacity = 750,
                        prox_water = 100,
                        bunded = "Yes"), 1585.5)
})
