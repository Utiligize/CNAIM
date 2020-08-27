library(testthat)
library(CNAIM)


test_that("f_cof_transformer_11kv", {
  # TODO: verify correctness
  expect_equal(f_cof_transformer_11kv(kva = 700,
                                      type = "Default"), 7739)
})

test_that("n_cof_excl_ehv_132kv_tf", {
  # TODO: verify correctness
  expect_equal(n_cof_excl_ehv_132kv_tf(asset_type_ncf = "6.6/11kV Transformer (GM)",
                                       no_customers = 750,
                                       kva_per_customer = 51), 455812.5)
})

test_that("s_cof_swg_tf_ohl", {
  # TODO: verify correctness
  expect_equal(s_cof_swg_tf_ohl(type_risk = "Default",
                                location_risk = "Default",
                                asset_type_scf = "6.6/11kV Transformer (GM)"), 4262)
})

test_that("health_score_excl_ehv_132kv_tf", {
  # a > 1, b > 1
  expect_equal(health_score_excl_ehv_132kv_tf(5, 4), 7)
  expect_equal(health_score_excl_ehv_132kv_tf(4, 5), 7)

  # a > 1, b <= 1
  expect_equal(health_score_excl_ehv_132kv_tf(5, 1), 5)
  expect_equal(health_score_excl_ehv_132kv_tf(1, 5), 5)
  expect_equal(health_score_excl_ehv_132kv_tf(5, 0.1), 5)
  expect_equal(health_score_excl_ehv_132kv_tf(0.1, 5), 5)

  # a <= 1, b <= 1
  expect_equal(health_score_excl_ehv_132kv_tf(1, 1), 1)
  expect_equal(health_score_excl_ehv_132kv_tf(1, 0.5), 0.5)
  expect_equal(health_score_excl_ehv_132kv_tf(0.5, 1), 0.5)
})

test_that("duty_factor_transformer_11kv", {
  expect_equal(duty_factor_transformer_11kv(), 1)
  expect_equal(duty_factor_transformer_11kv(45), 0.9)
  expect_equal(duty_factor_transformer_11kv(50), 0.9)
  expect_equal(duty_factor_transformer_11kv(65), 0.95)
  expect_equal(duty_factor_transformer_11kv(70), 0.95)
  expect_equal(duty_factor_transformer_11kv(75), 1)
  expect_equal(duty_factor_transformer_11kv(100), 1)
  expect_equal(duty_factor_transformer_11kv(1000), 1.4)
})

# TODO: test more parameter combinations
test_that("current_health", {
  expect_equal(current_health(2, 3), 6)
  expect_equal(current_health(2, 3, reliability_factor = 20), 20)
  expect_equal(current_health(200, 300), 10)
})

test_that("location_factor", {
  # TODO: verify correctness
  expect_equal(location_factor(placement = "Default",
                               altitude_m = "Default",
                               distance_from_coast_km = "Default",
                               corrosion_category_index = "Default",
                               asset_type = "6.6/11kV Transformer (GM)"), 0.925)
})

test_that("e_cof_tf", {
  # TODO: verify correctness
  expect_equal(e_cof_tf(asset_type_tf = "6.6/11kV Transformer (GM)",
                        rated_capacity = 750,
                        prox_water = 100,
                        bunded = "Yes"), 1585.5)
})

test_that("pof_future_transformer_11kv", {
  # TODO: verify correctness
  expect_equal(pof_transformer_11kv(utilisation_pct = "Default",
                                    placement = "Default",
                                    altitude_m = "Default",
                                    distance_from_coast_km = "Default",
                                    corrosion_category_index = "Default",
                                    age = 1,
                                    partial_discharge = "Default",
                                    oil_acidity = "Default",
                                    temperature_reading = "Default",
                                    observed_condition = "Default",
                                    reliability_factor = "Default"), 0.000225564)
})

test_that("E4_good_EHV_transformer", {
  # 5.
  lfactor = location_factor(placement = "Default",
                            altitude_m = 100,
                            distance_from_coast_km = 5,
                            corrosion_category_index = 3,
                            asset_type = "33kV Transformer (GM)")
  expect_equal(lfactor, 1.1)

  # 8.
  elife_transformer = expected_life(normal_expected_life = 60,
                                    duty_factor = 1.0,
                                    location_factor = lfactor)
  expect_equal(elife_transformer, 54.54545454545454)

  # 9.
  elife_tapchanger = expected_life(normal_expected_life = 60,
                                   duty_factor = 0.9,
                                   location_factor = lfactor)
  expect_equal(elife_tapchanger, 60.6060606060606)

  # 10.
  b1_transformer = beta_1(elife_transformer)
  expect_equal(b1_transformer, 0.0439614133346368)

  # 11.
  b1_tapchanger = beta_1(elife_tapchanger)
  expect_equal(b1_tapchanger, 0.039565272001173116)

  # 12.
  ih_transformer = initial_health(b1 = b1_transformer, age = 40)
  expect_equal(ih_transformer, 2.90173651)

  # 13.
  ih_tapchanger = initial_health(b1 = b1_tapchanger, age = 40)
  expect_equal(ih_tapchanger, 2.43382404)

  # 15.
  ch_transformer = current_health(ih_transformer,
                                  health_score_factor = 1)
  expect_equal(ch_transformer, 2.90173651)

  # 16.
  ch_tapchanger = current_health(ih_tapchanger,
                                  health_score_factor = 1)
  expect_equal(ch_tapchanger, 2.43382404)
})
