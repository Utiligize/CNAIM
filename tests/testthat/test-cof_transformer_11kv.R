library(testthat)
library(CNAIM)

context("consequences of a failure of a transformer")

test_that("cof_transformer_11kv", {
  # TODO: verify correctness
  expect_equal(cof_transformer_11kv(kva = 500,
                                    type = "Type C",
                                    type_risk = "High",
                                    location_risk = "High",
                                    prox_water = 50,
                                    bunded = "No",
                                    no_customers = 500,
                                    kva_per_customer = 1), 39208.7)
})
