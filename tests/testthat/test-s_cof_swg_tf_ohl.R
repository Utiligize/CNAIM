library(testthat)
library(CNAIM)

context("Numeric. Safety consequences of failure")

test_that("s_cof_swg_tf_ohl", {
  # TODO: verify correctness
  expect_equal(s_cof_swg_tf_ohl(type_risk = "Default",
                                location_risk = "Default",
                                asset_type_scf = "6.6/11kV Transformer (GM)"), 4262)
})
