#' @importFrom magrittr %>%
#' @title Current Probability of Failure for 6.6/11kV and 20kV Transformers
#' @description This function calculates the current
#' annual probability of failure for 6.6/11kV and 20kV transformers.
#' The function is a cubic curve that is based on
#' the first three terms of the Taylor series for an
#' exponential function. For more information about the
#' probability of failure function see section 6
#' on page 34 in CNAIM (2021).
#' @param hv_transformer_type String. Refers to the high voltage transformer
#' type the calculation is done for. Options: \code{hv_transformer_type =
#' c("6.6/11kV Transformer (GM)", "20kV Transformer (GM)")}. The default setting
#' is \code{hv_transformer_type = 6.6/11kV Transformer (GM)}.
#' @inheritParams duty_factor_transformer_11_20kv
#' @inheritParams location_factor
#' @inheritParams current_health
#' @param age  Numeric. The current age in years.
#' @param partial_discharge String. Indicating the
#' @param oil_acidity Oil Acidity
#' level of partial discharge. Options for \code{partial_discharge}:
#' \code{partial_discharge = c("Low", "Medium", "High (Not Confirmed)",
#'  "High (Confirmed)", "Default")}. See page 153, table 171 in CNAIM (2021).
#' @inheritParams oil_test_modifier
#' See page 162, table 204 in CNAIM (2021).
#' @param temperature_reading String. Indicating the criticality.
#' Options for \code{temperature_reading}:
#' \code{temperature_reading = c("Normal", "Moderately High",
#' "Very High", "Default")}. See page 153, table 172 in CNAIM (2021).
#' @param observed_condition String. Indicating the observed condition of the
#'  transformer. Options for \code{observed_condition}:
#' \code{observed_condition = c("No deterioration", "Superficial/minor deterioration", "Slight deterioration",
#'  "Some Deterioration", "Substantial Deterioration", "Default")}. See page 130, table 81 in CNAIM (2021).
#' @param moisture Numeric. the amount of moisture given in (ppm) See page 162, table 203 in CNAIM (2021).
#' @param acidity Numeric. the amount of acidicy given in (mg KOH/g) See page 162, table 204 in CNAIM (2021).
#' @param bd_strength Numeric. the amount of breakdown strength given in (kV) See page 162, table 205 in CNAIM (2021).
#' @param corrosion_category_index Integer.
#' Specify the corrosion index category, 1-5.
#' @return Numeric. Current probability of failure.
#' @source DNO Common Network Asset Indices Methodology (CNAIM),
#' Health & Criticality - Version 2.1, 2021:
#' \url{https://www.ofgem.gov.uk/sites/default/files/docs/2021/04/dno_common_network_asset_indices_methodology_v2.1_final_01-04-2021.pdf}
#' @export
#' @examples
#' # Current probability of failure for a 6.6/11 kV transformer
#' pof_transformer_11_20kv(hv_transformer_type = "6.6/11kV Transformer (GM)",
#' utilisation_pct = "Default",
#'placement = "Default",
#'altitude_m = "Default",
#'distance_from_coast_km = "Default",
#'corrosion_category_index = "Default",
#'age = 10,
#'partial_discharge = "Default",
#'temperature_reading = "Default",
#'observed_condition = "Default",
#'reliability_factor = "Default",
#'moisture = "Default",
#'oil_acidity = "Default",
#'bd_strength = "Default")
pof_transformer_11_20kv <- function(hv_transformer_type = "6.6/11kV Transformer (GM)",
                                    utilisation_pct = "Default",
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
                                    oil_acidity = "Default",
                                    bd_strength = "Default") {

  `Asset Register Category` = `Health Index Asset Category` =
    `Generic Term...1` = `Generic Term...2` = `Functional Failure Category` =
    `K-Value (%)` = `C-Value` = `Asset Register  Category` = NULL
  # due to NSE notes in R CMD check

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

  # Normal expected life for 6.6/11 kV transformer ------------------------------
  normal_expected_life <- gb_ref$normal_expected_life %>%
    dplyr::filter(`Asset Register  Category` == asset_type) %>%
    dplyr::pull()

  # Constants C and K for PoF function --------------------------------------
  k <- gb_ref$pof_curve_parameters %>%
    dplyr::filter(`Functional Failure Category` ==
                    asset_category) %>% dplyr::select(`K-Value (%)`) %>%
    dplyr::pull()/100

  c <- gb_ref$pof_curve_parameters %>%
    dplyr::filter(`Functional Failure Category` ==
                    asset_category) %>% dplyr::select(`C-Value`) %>% dplyr::pull()

  # Duty factor -------------------------------------------------------------
  duty_factor_tf_11kv <- duty_factor_transformer_11_20kv(utilisation_pct)

  # Location factor ----------------------------------------------------
  location_factor_transformer <- location_factor(placement,
                                                 altitude_m,
                                                 distance_from_coast_km,
                                                 corrosion_category_index,
                                                 asset_type)

  # Expected life for6.6/11 kV transformer ------------------------------
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
  # These overriding values are shown in Table 35 to Table 202
  # and Table 207 in Appendix B.

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
                                    oil_acidity,
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
