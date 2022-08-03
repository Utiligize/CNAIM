library(testthat)
library(CNAIM)

context("consequences of a failure of a 0.4/10kV transformer")

test_that("cof_transformer_04_10kv", {
  expect_equal(cof_transformer_04_10kv(kva = 500,
                                       type = "Type C",
                                       type_risk = "High",
                                       location_risk = "High",
                                       prox_water = 50, bunded = "No",
                                       no_customers = 500,
                                       kva_per_customer = 1) %>% round(1), 373500.5)

})
