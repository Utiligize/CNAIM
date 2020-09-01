library(testthat)
library(CNAIM)

context("Network cost of Failure")

test_that("n_cof_excl_ehv_132kv_tf", {
  # TODO: verify correctness
  expect_equal(n_cof_excl_ehv_132kv_tf(asset_type_ncf = "6.6/11kV Transformer (GM)",
                                       no_customers = 750,
                                       kva_per_customer = 51), 455812.5)
})

