library(testthat)
library(CNAIM)

context("Current Probability of Failure for a Building")

test_that("pof_building", {
  expect_equal(pof_building(substation_type = "Secondary",
                            material_type = "Wood",
                            placement = "Outdoor",
                            altitude_m = "Default",
                            distance_from_coast_km = "Default",
                            corrosion_category_index = "Default",
                            age = 43,
                            temperature_reading = "Default",
                            coolers_radiator = "Default",
                            kiosk = "Default",
                            cable_boxes = "Default",
                            reliability_factor = "Default",
                            k_value = "Default",
                            c_value = 1.087,
                            normal_expected_life_building = "Default"), 0.01076914)
})

