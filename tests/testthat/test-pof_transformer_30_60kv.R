library(testthat)
library(CNAIM)

context("POF 30 60 KV")

test_that("50 60 KV", {
  res <-  pof_transformer_30_60kv(transformer_type = "60kV Transformer (GM)",
     year_of_manufacture = 1980,
     utilisation_pct = "Default",
     no_taps = "Default",
     placement = "Default",
     altitude_m = "Default",
     distance_from_coast_km = "Default",
     corrosion_category_index = "Default",
     age_tf = 43,
     age_tc = 43,
     partial_discharge_tf = "Default",
     partial_discharge_tc = "Default",
   temperature_reading = "Default",
   main_tank = "Default",
   coolers_radiator = "Default",
   bushings = "Default",
   kiosk = "Default",
   cable_boxes = "Default",
   external_tap = "Default",
   internal_tap = "Default",
   mechnism_cond = "Default",
   diverter_contacts = "Default",
   diverter_braids = "Default",
   moisture = "Default",
   acidity = "Default",
   bd_strength = "Default",
   hydrogen = "Default",
   methane = "Default",
   ethylene = "Default",
   ethane = "Default",
   acetylene = "Default",
   hydrogen_pre = "Default",
   methane_pre = "Default",
   ethylene_pre = "Default",
   ethane_pre = "Default",
   acetylene_pre = "Default",
   furfuraldehyde = "Default",
   reliability_factor = "Default",
   k_value = 0.454,
   c_value = 1.087,
   normal_expected_life_tf = "Default",
   normal_expected_life_tc = "Default") %>% round(5)

  expected_val <- data.frame(pof = 0.12446, chs = 3.93159)

  expect_equal(res, expected_val)
})
