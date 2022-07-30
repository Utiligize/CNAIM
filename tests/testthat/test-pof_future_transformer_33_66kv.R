library(testthat)
library(CNAIM)

context("Future Probability of Failure for 66kV Transformer (GM)")

test_that("pof_future_transformer_33_66kv", {

  res <-
    pof_future_transformer_33_66kv(transformer_type = "66kV Transformer (GM)",
                                 year_of_manufacture = 2010,
                                 age_tf = 10,
                                 age_tc = 10,
                                 simulation_end_year = 100)

  expected_value <- readRDS(system.file("testdata/pof_future_transformer_33_66kv.rds", package =
                                          "CNAIM"))

  expect_equal(res, expected_value)

})

