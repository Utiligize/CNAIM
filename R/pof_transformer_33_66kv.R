#' @importFrom magrittr %>%
#' @title Current Probability of Failure for 33/10kV and 66/10kV Transformers
#' @description This function calculates the current
#' annual probability of failure for 33/10kV and 66/10kV transformers.
#' The function is a cubic curve that is based on
#' the first three terms of the Taylor series for an
#' exponential function. For more information about the
#' probability of failure function see section 6
#' on page 30 in CNAIM (2017).
#' @param transformer_type String. A sting that refers to the specific
#' asset category. See See page 15, table 1 in CNAIM (2017).
#' Options:
#' \code{transformer_type = c("33kV Transformer (GM)", "66kV Transformer (GM)")}
#' @param year_of_manufacture Numeric. Normal expected life depends on the
#' year for manufacture, see page 103 table 20 in CNAIM (2017).
#' @inheritParams duty_factor_transformer_33_66kv
#' @inheritParams location_factor
#' @inheritParams current_health
#' @param age_tf Numeric. The current age in years
#' of the transformer.
#' @param age_tc Numeric. The current age in years
#' of the tapchanger
#' @param partial_discharge_tf String. Indicating the
#' level of partial discharge in the transformer.
#' Options:
#' \code{partial_discharge_tf = c("Low", "Medium", "High (Not Confirmed)",
#'  "High (Confirmed)", "Default")}. See page 139, table 162 in CNAIM (2017).
#' @param partial_discharge_tc String. Indicating the
#' level of partial discharge in the tapchanger
#' Options:
#' \code{partial_discharge_tc = c("Low", "Medium", "High (Not Confirmed)",
#'  "High (Confirmed)", "Default")}. See page 140, table 164 in CNAIM (2017).
#' @param temperature_reading String. Indicating the criticality.
#' Options:
#' \code{temperature_reading = c("Normal", "Moderately High",
#' "Very High", "Default")}. See page 139, table 163 in CNAIM (2017).
#' @param observed_condition String. Indicating the observed condition of the
#' 10-kV transformer. Options for \code{observed_condition}:
#' \code{observed_condition = c("As New", "Good", "Slight Deterioration",
#'  "Poor", "Very Poor", "Default")}. See page 120, table 73 in CNAIM (2017).
#' @return Numeric. Current probability of failure.
#' @source DNO Common Network Asset Indices Methodology (CNAIM),
#' Health & Criticality - Version 1.1, 2017:
#' \url{https://www.ofgem.gov.uk/system/files/docs/2017/05/dno_common_network_asset_indices_methodology_v1.1.pdf}
#' @export
#' @examples
#' # Current probability of failure for a 66/10kV transformer
#' pof_transformer_11kv(utilisation_pct = "Default",
#'placement = "Default",
#'altitude_m = "Default",
#'distance_from_coast_km = "Default",
#'corrosion_category_index = "Default",
#'age = 10,
#'partial_discharge = "Default",
#'oil_acidity = "Default",
#'temperature_reading = "Default",
#'observed_condition = "Default",
#'reliability_factor = "Default")

pof_transformer_33_66kv <- function(transformer_type = "66kV Transformer (GM)",
                                    year_of_manufacture = 1980,
                                    utilisation_pct = "Default",
                                    no_taps = "Default",
                                    placement = "Default",
                                    altitude_m = "Default",
                                    distance_from_coast_km = "Default",
                                    corrosion_category_index = "Default",
                                    age_tf,
                                    age_tc,
                                    partial_discharge_tf = "Default",
                                    partial_discharge_tc = "Default",
                                    temperature_reading = "Default",


                                    observed_condition = "Default",
                                    reliability_factor = "Default") {

  `Asset Register Category` = `Health Index Asset Category` =
    `Generic Term...1` = `Generic Term...2` = `Functional Failure Category` =
    `K-Value (%)` = `C-Value` = `Asset Register  Category` = NULL
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

  # Normal expected life for transformer -----------------------------

  if (year_of_manufacture < 1980) {
    sub_division <- "Transformer - Pre 1980"
  } else {
    sub_division <- "Transformer - Post 1980"

  }

  normal_expected_life_tf <- gb_ref$normal_expected_life %>%
    dplyr::filter(`Asset Register  Category` == transformer_type & `Sub-division` ==
                    sub_division) %>%
    dplyr::pull()

  # Normal expected life for tapchanger -----------------------------

  normal_expected_life_tc <- gb_ref$normal_expected_life %>%
    dplyr::filter(`Asset Register  Category` == transformer_type & `Sub-division` ==
                    "Tapchanger") %>%
    dplyr::pull()

  # Constants C and K for PoF function --------------------------------------
  k <- gb_ref$pof_curve_parameters %>%
    dplyr::filter(`Functional Failure Category` ==
                    'EHV Transformer/ 132kV Transformer') %>% dplyr::select(`K-Value (%)`) %>%
    dplyr::pull()/100

  c <- gb_ref$pof_curve_parameters %>%
    dplyr::filter(`Functional Failure Category` ==
                    'EHV Transformer/ 132kV Transformer') %>% dplyr::select(`C-Value`) %>% dplyr::pull()

  # Duty factor -------------------------------------------------------------
  duty_factor_tf_11kv <- duty_factor_transformer_33_66kv(utilisation_pct,
                                                         no_taps)
  duty_factor_tf <-
    duty_factor_tf_11kv$duty_factor[which(duty_factor_tf_11kv$category ==
                                            "transformer")]
  duty_factor_tc <-
    duty_factor_tf_11kv$duty_factor[which(duty_factor_tf_11kv$category ==
                                            "tapchanger")]


  # Location factor ----------------------------------------------------
  location_factor_transformer <- location_factor(placement,
                                                 altitude_m,
                                                 distance_from_coast_km,
                                                 corrosion_category_index,
                                                 asset_type = transformer_type)

  # Expected life for transformer ------------------------------
  expected_life_years_tf <- expected_life(normal_expected_life =
                                            normal_expected_life_tf,
                                          duty_factor_tf,
                                          location_factor_transformer)

  # Expected life for tapchanger ------------------------------
  expected_life_years_tc <- expected_life(normal_expected_life =
                                            normal_expected_life_tc,
                                          duty_factor_tc,
                                          location_factor_transformer)

  # b1 (Initial Ageing Rate) ------------------------------------------------
  b1_tf <- beta_1(expected_life_years_tf)
  b1_tc <- beta_1(expected_life_years_tc)

  # Initial health score ----------------------------------------------------
  initial_health_score_tf <- initial_health(b1_tf, age_tf)
  initial_health_score_tc <- initial_health(b1_tc, age_tc)

  ## NOTE
  # Typically, the Health Score Collar is 0.5 and
  # Health Score Cap is 10, implying no overriding
  # of the Health Score. However, in some instances
  # these parameters are set to other values in the
  # Health Score Modifier calibration tables.
  # These overriding values are shown in Table 34 to Table 195
  # and Table 200 in Appendix B.

  # Measured condition inputs ---------------------------------------------
  mcm_mmi_cal_df <-
    gb_ref$measured_cond_modifier_mmi_cal

  mcm_mmi_cal_df <-
    mcm_mmi_cal_df[which(mcm_mmi_cal_df$`Asset Category` == "EHV Transformer (GM)"), ]


  factor_divider_1_tf <-
    as.numeric(mcm_mmi_cal_df$`Parameters for Combination Using MMI Technique - Factor Divider 1`[
      which(mcm_mmi_cal_df$Subcomponent == "Main Transformer")
    ])

  factor_divider_1_tc <-
    as.numeric(mcm_mmi_cal_df$`Parameters for Combination Using MMI Technique - Factor Divider 1`[
      which(mcm_mmi_cal_df$Subcomponent == "Tapchanger")
    ])


  factor_divider_2_tf <-
    as.numeric(mcm_mmi_cal_df$`Parameters for Combination Using MMI Technique - Factor Divider 2`[
      which(mcm_mmi_cal_df$Subcomponent == "Main Transformer")
    ])

  factor_divider_2_tc <-
    as.numeric(mcm_mmi_cal_df$`Parameters for Combination Using MMI Technique - Factor Divider 2`[
      which(mcm_mmi_cal_df$Subcomponent == "Tapchanger")
    ])

  max_no_combined_factors_tf <-
    as.numeric(mcm_mmi_cal_df$`Parameters for Combination Using MMI Technique - Max. No. of Combined Factors`[
      which(mcm_mmi_cal_df$Subcomponent == "Main Transformer")
    ])

  max_no_combined_factors_tc <-
    as.numeric(mcm_mmi_cal_df$`Parameters for Combination Using MMI Technique - Max. No. of Combined Factors`[
      which(mcm_mmi_cal_df$Subcomponent == "Tapchanger")
    ])


  # Partial discharge transformer ----------------------------------------------
  mci_hv_tf_partial_discharge <-
    gb_ref$mci_ehv_tf_main_tf_prtl_dis

  ci_factor_partial_discharge_tf <-
    mci_hv_tf_partial_discharge$`Condition Input Factor`[which(
      mci_hv_tf_partial_discharge$
        `Condition Criteria: Partial Discharge Test Result` ==
        partial_discharge_tf)]

  ci_cap_partial_discharge_tf <-
    mci_hv_tf_partial_discharge$`Condition Input Cap`[which(
      mci_hv_tf_partial_discharge$
        `Condition Criteria: Partial Discharge Test Result` ==
        partial_discharge_tf)]

  ci_collar_partial_discharge_tf <-
    mci_hv_tf_partial_discharge$`Condition Input Collar`[which(
      mci_hv_tf_partial_discharge$
        `Condition Criteria: Partial Discharge Test Result` ==
        partial_discharge_tf)]


  # Partial discharge tapchanger ------------------------------------------------
  mci_hv_tf_partial_discharge_tc <-
    gb_ref$mci_ehv_tf_tapchngr_prtl_dis

  ci_factor_partial_discharge_tc <-
    mci_hv_tf_partial_discharge_tc$`Condition Input Factor`[which(
      mci_hv_tf_partial_discharge_tc$
        `Condition Criteria: Partial Discharge Test Result` ==
        partial_discharge_tc)]

  ci_cap_partial_discharge_tc <-
    mci_hv_tf_partial_discharge_tc$`Condition Input Cap`[which(
      mci_hv_tf_partial_discharge_tc$
        `Condition Criteria: Partial Discharge Test Result` ==
        partial_discharge_tc)]

  ci_collar_partial_discharge_tc <-
    mci_hv_tf_partial_discharge_tc$`Condition Input Collar`[which(
      mci_hv_tf_partial_discharge_tc$
        `Condition Criteria: Partial Discharge Test Result` ==
        partial_discharge_tc)]


  # Temperature readings ----------------------------------------------------
  mci_hv_tf_temp_readings <-
    gb_ref$mci_ehv_tf_temp_readings

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
  factors_tf <- c(ci_factor_partial_discharge_tf,
                  ci_factor_temp_reading)

  measured_condition_factor_tf <- mmi(factors_tf,
                                      factor_divider_1_tf,
                                      factor_divider_2_tf,
                                      max_no_combined_factors_tf)


  measured_condition_factor_tc <- mmi(ci_factor_partial_discharge_tc,
                                      factor_divider_1_tc,
                                      factor_divider_2_tc,
                                      max_no_combined_factors_tc)

  # Measured condition cap --------------------------------------------------
  caps_tf <- c(ci_cap_partial_discharge_tf,
               ci_cap_temp_reading)
  measured_condition_cap_tf <- min(caps_tf)


  measured_condition_cap_tc <- ci_cap_partial_discharge_tc

  # Measured condition collar -----------------------------------------------
  collars_tf <- c(ci_collar_partial_discharge_tf,
                  ci_collar_temp_reading)
  measured_condition_collar_tf <- max(collars_tf)

  measured_condition_collar_tc <- ci_collar_partial_discharge_tc

  # Measured condition modifier ---------------------------------------------
  measured_condition_modifier_tf <- data.frame(measured_condition_factor_tf,
                                            measured_condition_cap_tf,
                                            measured_condition_collar_tf)

  measured_condition_modifier_tc <- data.frame(measured_condition_factor_tc,
                                               measured_condition_cap_tc,
                                               measured_condition_collar_tc)

  # Observed condition inputs ---------------------------------------------
  oci_mmi_cal_df <-
    gb_ref$observed_cond_modifier_mmi_cal

  oci_mmi_cal_df <-
    oci_mmi_cal_df[which(oci_mmi_cal_df$`Asset Category` == "EHV Transformer (GM)"), ]

  factor_divider_1_tf <-
    as.numeric(oci_mmi_cal_df$`Parameters for Combination Using MMI Technique - Factor Divider 1`[
      which(oci_mmi_cal_df$Subcomponent ==
              "Main Transformer") ])

  factor_divider_1_tc <-
    as.numeric(oci_mmi_cal_df$`Parameters for Combination Using MMI Technique - Factor Divider 1`[
      which(oci_mmi_cal_df$Subcomponent ==
              "Tapchanger") ])

  factor_divider_2_tf <-
    as.numeric(oci_mmi_cal_df$`Parameters for Combination Using MMI Technique - Factor Divider 2`[
      which(oci_mmi_cal_df$Subcomponent ==
              "Main Transformer") ])

  factor_divider_2_tc <-
    as.numeric(oci_mmi_cal_df$`Parameters for Combination Using MMI Technique - Factor Divider 2`[
      which(oci_mmi_cal_df$Subcomponent ==
              "Tapchanger") ])


  max_no_combined_factors_tf <-
    as.numeric(oci_mmi_cal_df$`Parameters for Combination Using MMI Technique - Max. No. of Combined Factors`[
      which(oci_mmi_cal_df$Subcomponent ==
              "Main Transformer") ])

  max_no_combined_factors_tc <-
    as.numeric(oci_mmi_cal_df$`Parameters for Combination Using MMI Technique - Max. No. of Combined Factors`[
      which(oci_mmi_cal_df$Subcomponent ==
              "Tapchanger") ])


  # Transformer -------------------------------------------------------------

  oci_ehv_tf_main_tank_cond <-
    gb_ref$oci_ehv_tf_main_tank_cond

  oci_ehv_tf_cooler_radiatr_cond <-
    gb_ref$oci_ehv_tf_cooler_radiatr_cond

  oci_ehv_tf_bushings_cond <-
    gb_ref$oci_ehv_tf_bushings_cond

  oci_ehv_tf_kiosk_cond <-
    gb_ref$oci_ehv_tf_kiosk_cond

  oci_ehv_tf_cable_boxes_cond <-
    gb_ref$oci_ehv_tf_cable_boxes_cond


  # Tapchanger --------------------------------------------------------------

  oci_ehv_tf_tapchanger_ext_cond <-
    gb_ref$oci_ehv_tf_tapchanger_ext_cond

  oci_ehv_tf_int_cond <-
    gb_ref$oci_ehv_tf_int_cond

  oci_ehv_tf_drive_mechnism_cond <-
    gb_ref$oci_ehv_tf_drive_mechnism_cond

  oci_ehv_tf_cond_select_divrter_braids <-
    gb_ref$oci_ehv_tf_cond_select_divrter

  oci_ehv_tf_cond_select_divrter_braids <-
    gb_ref$oci_ehv_tf



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

  # Probability of failure for the 6.6/11 kV transformer today -----------------
  probability_of_failure <- k *
    (1 + (c * current_health_score) +
       (((c * current_health_score)^2) / factorial(2)) +
       (((c * current_health_score)^3) / factorial(3)))

  return(probability_of_failure)
}
