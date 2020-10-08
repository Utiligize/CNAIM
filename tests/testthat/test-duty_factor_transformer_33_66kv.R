library(testthat)
library(CNAIM)

context("Duty Factor for 33/10kV and 66/10kV Transformers and Tapchanger")

test_that("basic case", {
  res <- duty_factor_transformer_33_66kv(utilisation_pct = 95,
                                  no_taps = 25)

  expected_df <- data.frame(category = c("transformer", "tapchanger"),
                            duty_factor = c(1.1, 1.2))

  expect_equal(res, expected_df)
})
