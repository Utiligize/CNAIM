library(testthat)
library(CNAIM)

context("Current Probability of Failure for a 66kV Transformer (GM)")

test_that("pof_transformer_33_66kv", {
  expect_equal(pof_transformer_33_66kv(transformer_type = "66kV Transformer (GM)",
                                    year_of_manufacture = 1980,
                                    age_tf=43,
                                    age_tc=43), 0.01244618)
})


