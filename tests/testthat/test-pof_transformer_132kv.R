library(testthat)
library(CNAIM)

context("Current Probability of Failure for a 132kV Transformer (GM)")

test_that("pof_transformer_132kv", {
  # TODO: verify correctness
  expect_equal(pof_transformer_132kv(transformer_type = "132kV Transformer (GM)",
                                       year_of_manufacture = 1980,
                                       age_tf=43,
                                       age_tc=43), 0.01244618)
})
