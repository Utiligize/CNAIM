#' @importFrom magrittr %>%
#' @title Future Probability of Failure for Primary Substation Building
#' and Secondary Substation Building.
#' @description This function calculates the future
#' annual probability of failure for primary substation building
#' and secondary substation building.
#' The function is a cubic curve that is based on
#' the first three terms of the Taylor series for an
#' exponential function.
#' @inheritParams pof_building
#' @param simulation_end_year Numeric. The last year of simulating probability
#' of failure. Default is 100.
#' @return Numeric. Future probability of failure.
#' @examples
#' # Future probability of failure for a Secondary substation Building
# pof_future_building(substation_type = "Secondary",
# material_type = "Wood",
# placement = "Outdoor",
# altitude_m = "Default",
# distance_from_coast_km = "Default",
# corrosion_category_index = "Default",
# age = 1,
# temperature_reading = "Default",
# coolers_radiator = "Default",
# kiosk = "Default",
# cable_boxes = "Default",
# reliability_factor = "Default",
# k_value = "Default",
# c_value = 1.087,
# normal_expected_life_building = "Default",
# simulation_end_year = 100)


pof_future_building <- function(substation_type = "Secondary",
                         material_type = "Wood",
                         placement = "Outdoor",
                         altitude_m = "Default",
                         distance_from_coast_km = "Default",
                         corrosion_category_index = "Default",
                         age,
                         temperature_reading = "Default",
                         coolers_radiator = "Default",
                         kiosk = "Default",
                         cable_boxes = "Default",
                         reliability_factor = "Default",
                         k_value = "Default",
                         c_value = 1.087,
                         normal_expected_life_building = "Default",
                         simulation_end_year = 100) {

  transformer_type <- "66kV Transformer (GM)" # This is done to use some of CNAIM's tables
  `Asset Register Category` = `Health Index Asset Category` =
    `Generic Term...1` = `Generic Term...2` = `Functional Failure Category` =
    `K-Value (%)` = `C-Value` = `Asset Register  Category` = `Sub-division` =
    `Asset Category` = NULL
  # due to NSE notes in R CMD check

  # Ref. table Categorisation of Assets and Generic Terms for Assets  --

  asset_category <- gb_ref$categorisation_of_assets %>%
    dplyr::filter(`Asset Register Category` == transformer_type) %>%
    dplyr::select(`Health Index Asset Category`) %>% dplyr::pull()

  generic_term_1 <- gb_ref$generic_terms_for_assets %>%
    dplyr::filter(`Health Index Asset Category` == asset_category) %>%
    dplyr::select(`Generic Term...1`) %>% dplyr::pull()

  generic_term_2 <- gb_ref$generic_terms_for_assets %>%
    dplyr::filter(`Health Index Asset Category` == asset_category) %>%
    dplyr::select(`Generic Term...2`) %>% dplyr::pull()

  # Normal expected life for Building -----------------------------


  if (substation_type == "Primary") {
    primary_factor <- 1.2
  } else {
    primary_factor <- 1
  }

  if (normal_expected_life_building == "Default" && material_type == "Brick") {
    normal_expected_life <- 100/primary_factor
  } else if (normal_expected_life_building == "Default" && material_type == "Steel") {
    normal_expected_life <- 80/primary_factor
  } else if (normal_expected_life_building == "Default" && material_type == "Wood") {
    normal_expected_life <- 60/primary_factor
  } else {
    normal_expected_life <- normal_expected_life_building/primary_factor
  }


  # Constants C and K for PoF function --------------------------------------
  if (k_value == "Default" && material_type == "Brick") {
    k <- (0.1/100)*primary_factor
  } else if (k_value == "Default" && material_type == "Steel") {
    k <- (0.2/100)*primary_factor
  } else if (k_value == "Default" && material_type == "Wood") {
    k <- (0.4/100)*primary_factor
  } else {
    k <- (k_value/100)*primary_factor
  }

  c <- c_value

  # Duty factor -------------------------------------------------------------
  duty_factor <- duty_factor_transformer_33_66kv()
  duty_factor <-
    duty_factor$duty_factor[which(duty_factor$category ==
                                    "transformer")]


  # Location factor ----------------------------------------------------
  location_factor <- location_factor(placement,
                                     altitude_m,
                                     distance_from_coast_km,
                                     corrosion_category_index,
                                     asset_type = transformer_type)

  # Expected life for building------------------------------
  expected_life_years <- expected_life(normal_expected_life,
                                       duty_factor,
                                       location_factor)


  # b1 (Initial Ageing Rate) ------------------------------------------------
  b1 <- beta_1(expected_life_years)

  # Initial health score ----------------------------------------------------
  initial_health_score <- initial_health(b1, age)

  ## NOTE
  # Typically, the Health Score Collar is 0.5 and
  # Health Score Cap is 10, implying no overriding
  # of the Health Score. However, in some instances
  # these parameters are set to other values in the
  # Health Score Modifier calibration tables.

  # Measured condition inputs ---------------------------------------------
  mcm_mmi_cal_df <-
    gb_ref$measured_cond_modifier_mmi_cal

  mcm_mmi_cal_df <-
    mcm_mmi_cal_df[which(mcm_mmi_cal_df$`Asset Category` == "EHV Transformer (GM)"), ]


  factor_divider_1 <-
    as.numeric(mcm_mmi_cal_df$`Parameters for Combination Using MMI Technique - Factor Divider 1`[
      which(mcm_mmi_cal_df$Subcomponent == "Main Transformer")
    ])


  factor_divider_2 <-
    as.numeric(mcm_mmi_cal_df$`Parameters for Combination Using MMI Technique - Factor Divider 2`[
      which(mcm_mmi_cal_df$Subcomponent == "Main Transformer")
    ])


  max_no_combined_factors <-
    as.numeric(mcm_mmi_cal_df$`Parameters for Combination Using MMI Technique - Max. No. of Combined Factors`[
      which(mcm_mmi_cal_df$Subcomponent == "Main Transformer")
    ])


  # Temperature readings ----------------------------------------------------
  mci_temp_readings <-
    gb_ref$mci_ehv_tf_temp_readings

  ci_factor_temp_reading <-
    mci_temp_readings$`Condition Input Factor`[which(
      mci_temp_readings$
        `Condition Criteria: Temperature Reading` ==
        temperature_reading)]

  ci_cap_temp_reading <-
    mci_temp_readings$`Condition Input Cap`[which(
      mci_temp_readings$
        `Condition Criteria: Temperature Reading` ==
        temperature_reading)]

  ci_collar_temp_reading <-
    mci_temp_readings$`Condition Input Collar`[which(
      mci_temp_readings$
        `Condition Criteria: Temperature Reading` ==
        temperature_reading)]

  # measured condition factor -----------------------------------------------
  factors <- ci_factor_temp_reading

  measured_condition_factor <- mmi(factors,
                                   factor_divider_1,
                                   factor_divider_2,
                                   max_no_combined_factors)

  # Measured condition cap --------------------------------------------------
  measured_condition_cap <- ci_cap_temp_reading

  # Measured condition collar -----------------------------------------------
  measured_condition_collar <- ci_collar_temp_reading

  # Measured condition modifier ---------------------------------------------
  measured_condition_modifier <- data.frame(measured_condition_factor,
                                            measured_condition_cap,
                                            measured_condition_collar)

  # Observed condition inputs ---------------------------------------------
  oci_mmi_cal_df <-
    gb_ref$observed_cond_modifier_mmi_cal %>%
    dplyr::filter(`Asset Category` == "EHV Transformer (GM)")

  factor_divider_1_obs <-
    as.numeric(oci_mmi_cal_df$`Parameters for Combination Using MMI Technique - Factor Divider 1`[
      which(oci_mmi_cal_df$Subcomponent ==
              "Main Transformer") ])


  factor_divider_2_obs <-
    as.numeric(oci_mmi_cal_df$`Parameters for Combination Using MMI Technique - Factor Divider 2`[
      which(oci_mmi_cal_df$Subcomponent ==
              "Main Transformer") ])


  max_no_combined_factors_obs <-
    as.numeric(oci_mmi_cal_df$`Parameters for Combination Using MMI Technique - Max. No. of Combined Factors`[
      which(oci_mmi_cal_df$Subcomponent ==
              "Main Transformer") ])


  # Building -------------------------------------------------------------

  # Coolers/Radiator condition

  oci_cooler_radiatr_cond <-
    gb_ref$oci_ehv_tf_cooler_radiatr_cond

  Oi_collar_coolers_radiator <-
    oci_cooler_radiatr_cond$`Condition Input Collar`[which(
      oci_cooler_radiatr_cond$`Condition Criteria: Observed Condition` ==
        coolers_radiator)]

  Oi_cap_coolers_radiator <-
    oci_cooler_radiatr_cond$`Condition Input Cap`[which(
      oci_cooler_radiatr_cond$`Condition Criteria: Observed Condition` ==
        coolers_radiator)]

  Oi_factor_coolers_radiator <-
    oci_cooler_radiatr_cond$`Condition Input Factor`[which(
      oci_cooler_radiatr_cond$`Condition Criteria: Observed Condition` ==
        coolers_radiator)]


  # Kiosk

  oci_kiosk_cond <-
    gb_ref$oci_ehv_tf_kiosk_cond

  Oi_collar_kiosk <-
    oci_kiosk_cond$`Condition Input Collar`[which(
      oci_kiosk_cond$`Condition Criteria: Observed Condition` ==
        kiosk)]

  Oi_cap_kiosk <-
    oci_kiosk_cond$`Condition Input Cap`[which(
      oci_kiosk_cond$`Condition Criteria: Observed Condition` ==
        kiosk)]

  Oi_factor_kiosk <-
    oci_kiosk_cond$`Condition Input Factor`[which(
      oci_kiosk_cond$`Condition Criteria: Observed Condition` ==
        kiosk)]


  # Cable box
  oci_cable_boxes_cond <-
    gb_ref$oci_ehv_tf_cable_boxes_cond

  Oi_collar_cable_boxes <-
    oci_cable_boxes_cond$`Condition Input Collar`[which(
      oci_cable_boxes_cond$`Condition Criteria: Observed Condition` ==
        cable_boxes)]

  Oi_cap_cable_boxes <-
    oci_cable_boxes_cond$`Condition Input Cap`[which(
      oci_cable_boxes_cond$`Condition Criteria: Observed Condition` ==
        cable_boxes)]

  Oi_factor_cable_boxes <-
    oci_cable_boxes_cond$`Condition Input Factor`[which(
      oci_cable_boxes_cond$`Condition Criteria: Observed Condition` ==
        cable_boxes)]


  # Observed condition factor --------------------------------------

  factors_obs <- c(Oi_factor_coolers_radiator,
                   Oi_factor_kiosk,
                   Oi_factor_cable_boxes)

  observed_condition_factor <- mmi(factors_obs,
                                   factor_divider_1_obs,
                                   factor_divider_2_obs,
                                   max_no_combined_factors_obs)


  # Observed condition cap -----------------------------------------
  caps_obs <- c(Oi_cap_coolers_radiator,
                Oi_cap_kiosk,
                Oi_cap_cable_boxes)

  observed_condition_cap <- min(caps_obs)

  # Observed condition collar ---------------------------------------
  collars_obs <- c(Oi_collar_coolers_radiator,
                   Oi_collar_kiosk,
                   Oi_collar_cable_boxes)

  observed_condition_collar <- max(collars_obs)


  # Observed condition modifier ---------------------------------------------
  observed_condition_modifier <- data.frame(observed_condition_factor,
                                            observed_condition_cap,
                                            observed_condition_collar)


  # Health score factor ---------------------------------------------------

  health_score_factor <-  gb_ref$health_score_factor_for_tf


  factor_divider_1_health <-
    health_score_factor$`Parameters for Combination Using MMI Technique - Factor Divider 1`

  factor_divider_2_health <-
    health_score_factor$`Parameters for Combination Using MMI Technique - Factor Divider 2`

  max_no_combined_factors_health <-
    health_score_factor$`Parameters for Combination Using MMI Technique - Max. No. of Condition Factors`


  # Health score modifier -----------------------------------------------------

  obs_factor <- observed_condition_modifier$observed_condition_factor
  mea_factor <- measured_condition_modifier$measured_condition_factor


  factors_health <- c(obs_factor,
                      mea_factor)

  health_score_factor <- mmi(factors_health,
                             factor_divider_1_health,
                             factor_divider_2_health,
                             max_no_combined_factors_health)


  # Health score cap --------------------------------------------------------

  # Transformer
  health_score_cap <- min(observed_condition_modifier$observed_condition_cap,
                          measured_condition_modifier$measured_condition_cap)




  # Health score collar -----------------------------------------------------

  health_score_collar <- max(observed_condition_modifier$observed_condition_collar,
                             measured_condition_modifier$measured_condition_collar)


  # Health score modifier ---------------------------------------------------

  health_score_modifier <- data.frame(health_score_factor,
                                      health_score_cap,
                                      health_score_collar)

  # Current health score ----------------------------------------------------

  current_health_score <-
    current_health(initial_health_score,
                   health_score_modifier$health_score_factor,
                   health_score_modifier$health_score_cap,
                   health_score_modifier$health_score_collar,
                   reliability_factor = reliability_factor)


  # Probability of failure for the 6.6/11 kV transformer today -----------------
  probability_of_failure <- k *
    (1 + (c * current_health_score) +
       (((c * current_health_score)^2) / factorial(2)) +
       (((c * current_health_score)^3) / factorial(3)))

  # Future probability of failure -------------------------------------------

  # the Health Score of a new asset
  H_new <- 0.5

  # the Health Score of the asset when it reaches its Expected Life
  b2 <- beta_2(current_health_score, age)

  if (b2 > 2*b1){
    b2 <- b1
  } else if (current_health_score == 0.5){
    b2 <- b1
  }

  if (current_health_score < 2) {
    ageing_reduction_factor <- 1
  } else if (current_health_score <= 5.5) {
    ageing_reduction_factor <- ((current_health_score - 2)/7) + 1
  } else {
    ageing_reduction_factor <- 1.5
  }

  # Dynamic part
  pof_year <- list()
  year <- seq(from=0,to=simulation_end_year,by=1)

  for (y in 1:length(year)){
    t <- year[y]

    future_health_Score <- current_health_score*exp((b2/ageing_reduction_factor) * t)

    H <- future_health_Score

    future_health_score_limit <- 15
    if (H > future_health_score_limit){
      H <- future_health_score_limit
    }

    pof_year[[paste(y)]] <- k * (1 + (c * H) +
                                   (((c * H)^2) / factorial(2)) +
                                   (((c * H)^3) / factorial(3)))
  }

  pof_future <- data.frame(year=year, PoF=as.numeric(unlist(pof_year)))
  pof_future$age <- NA
  pof_future$age[1] <- age

  for(i in 2:nrow(pof_future)) {

    pof_future$age[i] <- age + i -1

  }

  return(pof_future)
}

