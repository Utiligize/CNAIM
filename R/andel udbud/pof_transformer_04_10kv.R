#' @importFrom magrittr %>%
#' @title Current Probability of Failure for Danish 0.4/10kV Transformers
#' @description This function calculates the current
#' annual probability of failure for Danish 0.4/10kV Transformers.
#' The function is a cubic curve that is based on
#' the first three terms of the Taylor series for an
#' exponential function.
#' @inheritParams duty_factor_transformer_11_20kv # this is the same for 0.4/10kV Transformers
#' @inheritParams location_factor
#' @inheritParams current_health
#' @param age  Numeric. The current age in years.
#' @param partial_discharge String. Indicating the
#' level of partial discharge. Options for \code{partial_discharge}:
#' \code{partial_discharge = c("Low", "Medium", "High (Not Confirmed)",
#'  "High (Confirmed)", "Default")}.
#' @inheritParams oil_test_modifier
#' @param temperature_reading String. Indicating the criticality.
#' Options for \code{temperature_reading}:
#' \code{temperature_reading = c("Normal", "Moderately High",
#' "Very High", "Default")}.
#' @param observed_condition String. Indicating the observed condition of the
#'  transformer. Options for \code{observed_condition}:
#' \code{observed_condition = c("No deterioration", "Superficial/minor deterioration", "Slight Deterioration",
#'  "Some deterioration", "Substantial deterioration", "Default")}.
#' @param k_value Numeric. \code{k_value = 0.0077} by default. This number is
#' given in a percentage. The default value is accordingly to the standard
#' "DE-10kV apb kabler CNAIM" on p. 34.
#' @param c_value Numeric. \code{c_value = 1.087} by default.
#' The default value is accordingly to the CNAIM standard see page 110
#' @param normal_expected_life Numeric. \code{normal_expected_life = 55} by default.
#' The default value is accordingly to the standard
#' "DE-10kV apb kabler CNAIM" on p. 33.
#' @return Numeric. Current probability of failure.
#' @export
#' @examples
#' # Current probability of failure for Danish 0.4/10kV Transformers
# pof_transformer_04_10kv(utilisation_pct = "Default",
# placement = "Default",
# altitude_m = "Default",
# distance_from_coast_km = "Default",
# corrosion_category_index = "Default",
# age = 10,
# partial_discharge = "Default",
# temperature_reading = "Default",
# observed_condition = "Default",
# reliability_factor = "Default",
# moisture = "Default",
# acidity = "Default",
# bd_strength = "Default",
# k_value = 0.0077,
# c_value = 1.087,
# normal_expected_life = 55)

pof_transformer_04_10kv <- function(utilisation_pct = "Default",
                                    placement = "Default",
                                    altitude_m = "Default",
                                    distance_from_coast_km = "Default",
                                    corrosion_category_index = "Default",
                                    age,
                                    partial_discharge = "Default",
                                    temperature_reading = "Default",
                                    observed_condition = "Default",
                                    reliability_factor = "Default",
                                    moisture = "Default",
                                    acidity = "Default",
                                    bd_strength = "Default",
                                    k_value = 0.0077,
                                    c_value = 1.087,
                                    normal_expected_life = 55) {

  hv_transformer_type <- "6.6/11kV Transformer (GM)"
  # this is in order to access tables the are identical to the ones 0.4/10kV transformer is using


  # Ref. table Categorisation of Assets and Generic Terms for Assets  --
  asset_type <- hv_transformer_type

  asset_category <- gb_ref$categorisation_of_assets %>%
    dplyr::filter(`Asset Register Category` == asset_type) %>%
    dplyr::select(`Health Index Asset Category`) %>% dplyr::pull()

  generic_term_1 <- gb_ref$generic_terms_for_assets %>%
    dplyr::filter(`Health Index Asset Category` == asset_category) %>%
    dplyr::select(`Generic Term...1`) %>% dplyr::pull()

  generic_term_2 <- gb_ref$generic_terms_for_assets %>%
    dplyr::filter(`Health Index Asset Category` == asset_category) %>%
    dplyr::select(`Generic Term...2`) %>% dplyr::pull()

  # Constants C and K for PoF function --------------------------------------
  k <- k_value/100 # see page 31 in DE-10kV apb kabler CNAIM
  c <- c_value # see page 31 in DE-10kV apb kabler CNAIM

  # Duty factor -------------------------------------------------------------
  duty_factor_tf_11kv <- duty_factor_transformer_11_20kv(utilisation_pct)

  # Location factor ----------------------------------------------------
  location_factor_transformer <- location_factor(placement,
                                                 altitude_m,
                                                 distance_from_coast_km,
                                                 corrosion_category_index,
                                                 asset_type)

  # Expected life for 0.4/10 kV transformer ------------------------------
  expected_life_years <- expected_life(normal_expected_life,
                                       duty_factor_tf_11kv,
                                       location_factor_transformer)

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
    mcm_mmi_cal_df[which(mcm_mmi_cal_df$`Asset Category` == asset_category), ]

  factor_divider_1 <-
    as.numeric(mcm_mmi_cal_df$
                 `Parameters for Combination Using MMI Technique - Factor Divider 1`)

  factor_divider_2 <-
    as.numeric(mcm_mmi_cal_df$
                 `Parameters for Combination Using MMI Technique - Factor Divider 2`)

  max_no_combined_factors <-
    as.numeric(mcm_mmi_cal_df$
                 `Parameters for Combination Using MMI Technique - Max. No. of Combined Factors`
    )

  # Partial discharge -------------------------------------------------------
  mci_hv_tf_partial_discharge <-
    gb_ref$mci_hv_tf_partial_discharge

  ci_factor_partial_discharge <-
    mci_hv_tf_partial_discharge$`Condition Input Factor`[which(
      mci_hv_tf_partial_discharge$
        `Condition Criteria: Partial Discharge Test Result` ==
        partial_discharge)]

  ci_cap_partial_discharge <-
    mci_hv_tf_partial_discharge$`Condition Input Cap`[which(
      mci_hv_tf_partial_discharge$
        `Condition Criteria: Partial Discharge Test Result` ==
        partial_discharge)]

  ci_collar_partial_discharge <-
    mci_hv_tf_partial_discharge$`Condition Input Collar`[which(
      mci_hv_tf_partial_discharge$
        `Condition Criteria: Partial Discharge Test Result` ==
        partial_discharge)]


  # Oil test modifier -------------------------------------------------------
  oil_test_mod <- oil_test_modifier(moisture,
                                    acidity,
                                    bd_strength)

  # Temperature readings ----------------------------------------------------
  mci_hv_tf_temp_readings <-
    gb_ref$mci_hv_tf_temp_readings

  ci_factor_temp_reading <-
    mci_hv_tf_temp_readings$`Condition Input Factor`[which(
      mci_hv_tf_temp_readings$
        `Condition Criteria: Temperature Reading` ==
        temperature_reading)]

  ci_cap_temp_reading <-
    mci_hv_tf_temp_readings$`Condition Input Cap`[which(
      mci_hv_tf_temp_readings$
        `Condition Criteria: Temperature Reading` ==
        temperature_reading)]

  ci_collar_temp_reading <-
    mci_hv_tf_temp_readings$`Condition Input Collar`[which(
      mci_hv_tf_temp_readings$
        `Condition Criteria: Temperature Reading` ==
        temperature_reading)]

  # measured condition factor -----------------------------------------------
  factors <- c(ci_factor_partial_discharge,
               oil_test_mod$oil_condition_factor,
               ci_factor_temp_reading)

  measured_condition_factor <- mmi(factors,
                                   factor_divider_1,
                                   factor_divider_2,
                                   max_no_combined_factors)

  # Measured condition cap --------------------------------------------------
  caps <- c(ci_cap_partial_discharge,
            oil_test_mod$oil_condition_cap,
            ci_cap_temp_reading)
  measured_condition_cap <- min(caps)

  # Measured condition collar -----------------------------------------------
  collars <- c(ci_collar_partial_discharge,
               oil_test_mod$oil_condition_collar,
               ci_collar_temp_reading)
  measured_condition_collar <- max(collars)

  # Measured condition modifier ---------------------------------------------
  measured_condition_modifier <- data.frame(measured_condition_factor,
                                            measured_condition_cap,
                                            measured_condition_collar)

  # Observed condition inputs ---------------------------------------------
  oci_mmi_cal_df <-
    gb_ref$observed_cond_modifier_mmi_cal

  oci_mmi_cal_df <-
    oci_mmi_cal_df[which(oci_mmi_cal_df$`Asset Category` == asset_category), ]

  factor_divider_1 <-
    as.numeric(oci_mmi_cal_df$
                 `Parameters for Combination Using MMI Technique - Factor Divider 1`)

  factor_divider_2 <-
    as.numeric(oci_mmi_cal_df$
                 `Parameters for Combination Using MMI Technique - Factor Divider 2`)

  max_no_combined_factors <-
    as.numeric(oci_mmi_cal_df$
                 `Parameters for Combination Using MMI Technique - Max. No. of Combined Factors`
    )

  oci_hv_tf_tf_ext_cond_df <-
    gb_ref$oci_hv_tf_tf_ext_cond

  ci_factor_ext_cond <-
    oci_hv_tf_tf_ext_cond_df$`Condition Input Factor`[which(
      oci_hv_tf_tf_ext_cond_df$`Condition Criteria: Observed Condition` ==
        observed_condition)]

  ci_cap_ext_cond <-
    oci_hv_tf_tf_ext_cond_df$`Condition Input Cap`[which(
      oci_hv_tf_tf_ext_cond_df$`Condition Criteria: Observed Condition` ==
        observed_condition)]

  ci_collar_ext_cond <-
    oci_hv_tf_tf_ext_cond_df$`Condition Input Collar`[which(
      oci_hv_tf_tf_ext_cond_df$`Condition Criteria: Observed Condition` ==
        observed_condition)]

  # Observed condition factor -----------------------------------------------
  observed_condition_factor <- mmi(factors = ci_factor_ext_cond,
                                   factor_divider_1,
                                   factor_divider_2,
                                   max_no_combined_factors)

  # Observed condition cap ---------------------------------------------
  observed_condition_cap <- ci_cap_ext_cond

  # Observed condition collar ---------------------------------------------
  observed_condition_collar <- ci_collar_ext_cond

  # Observed condition modifier ---------------------------------------------
  observed_condition_modifier <- data.frame(observed_condition_factor,
                                            observed_condition_cap,
                                            observed_condition_collar)

  # Health score factor ---------------------------------------------------
  health_score_factor <-
    health_score_excl_ehv_132kv_tf(observed_condition_factor,
                                   measured_condition_factor)

  # Health score cap --------------------------------------------------------
  health_score_cap <- min(observed_condition_cap, measured_condition_cap)

  # Health score collar -----------------------------------------------------
  health_score_collar <-  max(observed_condition_collar,
                              measured_condition_collar)

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

  # Probability of failure for the 6.6/11kV and 20kV transformer today -----------------
  probability_of_failure <- k *
    (1 + (c * current_health_score) +
       (((c * current_health_score)^2) / factorial(2)) +
       (((c * current_health_score)^3) / factorial(3)))

  return(probability_of_failure)
}
