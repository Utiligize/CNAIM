library(CNAIM)
library(testthat)

context("Weibull predict")

test_that("predict weibull", {
  res <- predict_weibull_model(age = 50) %>% round(3)

  expected_value <- 0.005

  expect_equal(res, expected_value)
})

context("Weibull train")

test_that("train weibull", {
  res <- train_weibull_model(transformer_faults_data = transformer_11kv_faults)

  expected_value <- read.csv(system.file("testdata/expected_weibull_train_df.csv", package =
                                           "CNAIM"))

  expect_equal(res, expected_value)
})
