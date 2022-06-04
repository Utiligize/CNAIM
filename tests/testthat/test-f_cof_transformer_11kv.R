library(testthat)
library(CNAIM)

context("Financial Consequences of Failure Transformer")

test_that("f_cof_transformer_11kv", {
  expect_equal(f_cof_transformer_11kv(kva = 700,
                                      type = "Default"), 9297)
})
