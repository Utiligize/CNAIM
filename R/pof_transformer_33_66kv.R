#' @importFrom magrittr %>%
#' @title Current Probability of Failure for 33/10kV and 66/10kV Transformers
#' @description This function calculates the current
#' annual probability of failure for 33/10kV and 66/10kV transformers.
#' The function is a cubic curve that is based on
#' the first three terms of the Taylor series for an
#' exponential function. For more information about the
#' probability of failure function see section 6
#' on page 34 in CNAIM (2021).
#' @param transformer_type String. A sting that refers to the specific
#' asset category. See See page 17, table 1 in CNAIM (2021).
#' Options:
#' \code{transformer_type =
#' c("33kV Transformer (GM)", "66kV Transformer (GM)")}. The default setting is
#' \code{transformer_type = "66kV Transformer (GM)"}
#' @param year_of_manufacture Numeric. Normal expected life depends on the
#' year for manufacture, see page 107 table 20 in CNAIM (2021).
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
#'  "High (Confirmed)", "Default")}. See page 154, table 173 in CNAIM (2021).
#' @param partial_discharge_tc String. Indicating the
#' level of partial discharge in the tapchanger
#' Options:
#' \code{partial_discharge_tc = c("Low", "Medium", "High (Not Confirmed)",
#'  "High (Confirmed)", "Default")}. See page 155, table 175 in CNAIM (2021).
#' @param temperature_reading String. Indicating the criticality.
#' Options:
#' \code{temperature_reading = c("Normal", "Moderately High",
#' "Very High", "Default")}. See page 154, table 174 in CNAIM (2021).
#' @param main_tank String. Indicating the observed condition of the
#' main tank. Options:
#' \code{main_tank = c("Superficial/minor deterioration", "Some Deterioration",
#' "Substantial Deterioration", "Default")}. See page 131, table 83
#' in CNAIM (2021).
#' @param coolers_radiator String. Indicating the observed condition of the
#' coolers/radiators. Options:
#' \code{coolers_radiator = c("Superficial/minor deterioration", "Some Deterioration",
#' "Substantial Deterioration", "Default")}. See page 131, table 84
#' in CNAIM (2021).
#' @param bushings String. Indicating the observed condition of the
#' bushings. Options:
#' \code{bushings = c("Superficial/minor deterioration", "Some Deterioration",
#' "Substantial Deterioration", "Default")}. See page 131, table 85
#' in CNAIM (2021).
#' @param kiosk String. Indicating the observed condition of the
#' kiosk. Options:
#' \code{kiosk = c("Superficial/minor deterioration", "Some Deterioration",
#' "Substantial Deterioration", "Default")}. See page 132, table 86
#' in CNAIM (2021).
#' @param cable_boxes String. Indicating the observed condition of the
#' cable boxes. Options:
#' \code{cable_boxes = c("No Deterioration","Superficial/minor deterioration", "Some Deterioration",
#' "Substantial Deterioration", "Default")}. See page 132, table 87
#' in CNAIM (2021).
#' @param external_tap String. Indicating the observed external condition of the
#'  tapchanger. Options:
#' \code{external_tap = c("Superficial/minor deterioration", "Some Deterioration",
#' "Substantial Deterioration", "Default")}. See page 133, table 88
#' in CNAIM (2021).
#' @param internal_tap String. Indicating the observed internal condition of the
#'  tapchanger. Options:
#' \code{internal_tap = c("Superficial/minor deterioration", "Some Deterioration",
#' "Substantial Deterioration", "Default")}. See page 133, table 89
#' in CNAIM (2021).
#' @param mechnism_cond String. Indicating the observed condition of the
#'  drive mechnism. Options:
#' \code{mechnism_cond = c("No deterioration", "Superficial/minor deterioration", "Some Deterioration",
#' "Substantial Deterioration", "Default")}. See page 133, table 90
#' in CNAIM (2021).
#' @param diverter_contacts String. Indicating the observed condition of the
#' selector and diverter contacts. Options:
#' \code{diverter_contacts = c("No deterioration", "Superficial/minor deterioration", "Some Deterioration",
#' "Substantial Deterioration", "Default")}. See page 133, table 91
#' in CNAIM (2021).
#' @param diverter_braids String. Indicating the observed condition of the
#' selector and diverter braids. Options:
#' \code{diverter_braids = c("No deterioration", "Superficial/minor deterioration", "Some Deterioration",
#' "Substantial Deterioration", "Default")}. See page 134, table 92
#' in CNAIM (2021)
#' @param corrosion_category_index Integer.
#' Specify the corrosion index category, 1-5.
#' @param moisture Numeric. the amount of moisture given in (ppm) See page 162, table 203 in CNAIM (2021).
#' @param acidity Numeric. the amount of acidicy given in (mg KOH/g) See page 162, table 204 in CNAIM (2021).
#' @param bd_strength Numeric. the amount of breakdown strength given in (kV) See page 162, table 205 in CNAIM (2021).
#' @inheritParams oil_test_modifier
#' @inheritParams dga_test_modifier
#' @inheritParams ffa_test_modifier
#' @param gb_ref_given optional parameter to use custom reference values
#' @return DataFrame Current probability of failure
#' per annum per kilometer along with current health score.
#' @source DNO Common Network Asset Indices Methodology (CNAIM),
#' Health & Criticality - Version 2.1, 2021:
#' \url{https://www.ofgem.gov.uk/sites/default/files/docs/2021/04/dno_common_network_asset_indices_methodology_v2.1_final_01-04-2021.pdf}
#' @export
#' @examples
#' # Current probability of failure for a 66/10kV transformer
#' pof_transformer_33_66kv(transformer_type = "66kV Transformer (GM)",
#' year_of_manufacture = 1980,
#' utilisation_pct = "Default",
#' no_taps = "Default",
#' placement = "Default",
#' altitude_m = "Default",
#' distance_from_coast_km = "Default",
#' corrosion_category_index = "Default",
#' age_tf = 43,
#' age_tc = 43,
#' partial_discharge_tf = "Default",
#' partial_discharge_tc = "Default",
#' temperature_reading = "Default",
#' main_tank = "Default",
#' coolers_radiator = "Default",
#' bushings = "Default",
#' kiosk = "Default",
#' cable_boxes = "Default",
#' external_tap = "Default",
#' internal_tap = "Default",
#' mechnism_cond = "Default",
#' diverter_contacts = "Default",
#' diverter_braids = "Default",
#' moisture = "Default",
#' acidity = "Default",
#' bd_strength = "Default",
#' hydrogen = "Default",
#' methane = "Default",
#' ethylene = "Default",
#' ethane = "Default",
#' acetylene = "Default",
#' hydrogen_pre = "Default",
#' methane_pre = "Default",
#' ethylene_pre = "Default",
#' ethane_pre = "Default",
#' acetylene_pre = "Default",
#' furfuraldehyde = "Default",
#' reliability_factor = "Default")

pof_transformer_33_66kv <- function(transformer_type = "66kV Transformer (GM)",
                                    year_of_manufacture,
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
                                    gb_ref_given = NULL) {

  `Asset Register Category` = `Health Index Asset Category` =
    `Generic Term...1` = `Generic Term...2` = `Functional Failure Category` =
    `K-Value (%)` = `C-Value` = `Asset Register  Category` = `Sub-division` =
    `Asset Category` = NULL
  # due to NSE notes in R CMD check
  if(is.null(gb_ref_given)){
    gb_ref_taken <- gb_ref
  }else{
    check_gb_ref_given(gb_ref_given)
    gb_ref_taken <- gb_ref_given
  }

  # Ref. table Categorisation of Assets and Generic Terms for Assets  --

  asset_category <- gb_ref_taken$categorisation_of_assets %>%
    dplyr::filter(`Asset Register Category` == transformer_type) %>%
    dplyr::select(`Health Index Asset Category`) %>% dplyr::pull()

  generic_term_1 <- gb_ref_taken$generic_terms_for_assets %>%
    dplyr::filter(`Health Index Asset Category` == asset_category) %>%
    dplyr::select(`Generic Term...1`) %>% dplyr::pull()

  generic_term_2 <- gb_ref_taken$generic_terms_for_assets %>%
    dplyr::filter(`Health Index Asset Category` == asset_category) %>%
    dplyr::select(`Generic Term...2`) %>% dplyr::pull()

  # Normal expected life for transformer -----------------------------

  if (year_of_manufacture < 1980) {
    sub_division <- "Transformer - Pre 1980"
  } else {
    sub_division <- "Transformer - Post 1980"

  }

  normal_expected_life_tf <- gb_ref_taken$normal_expected_life %>%
    dplyr::filter(`Asset Register  Category` == transformer_type & `Sub-division` ==
                    sub_division) %>%
    dplyr::pull()

  # Normal expected life for tapchanger -----------------------------

  normal_expected_life_tc <- gb_ref_taken$normal_expected_life %>%
    dplyr::filter(`Asset Register  Category` == transformer_type & `Sub-division` ==
                    "Tapchanger") %>%
    dplyr::pull()

  # Constants C and K for PoF function --------------------------------------
  k <- gb_ref_taken$pof_curve_parameters %>%
    dplyr::filter(`Functional Failure Category` ==
                    'EHV Transformer/ 132kV Transformer') %>% dplyr::select(`K-Value (%)`) %>%
    dplyr::pull()/100

  c <- gb_ref_taken$pof_curve_parameters %>%
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
  # These overriding values are shown in Table 35 to Table 202
  # and Table 207 in Appendix B.

  # Measured condition inputs ---------------------------------------------
  mcm_mmi_cal_df <-
    gb_ref_taken$measured_cond_modifier_mmi_cal

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
    gb_ref_taken$mci_ehv_tf_main_tf_prtl_dis

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
    gb_ref_taken$mci_ehv_tf_tapchngr_prtl_dis

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
    gb_ref_taken$mci_ehv_tf_temp_readings

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
    gb_ref_taken$observed_cond_modifier_mmi_cal %>%
    dplyr::filter(`Asset Category` == "EHV Transformer (GM)")

  factor_divider_1_tf_obs <-
    as.numeric(oci_mmi_cal_df$`Parameters for Combination Using MMI Technique - Factor Divider 1`[
      which(oci_mmi_cal_df$Subcomponent ==
              "Main Transformer") ])

  factor_divider_1_tc_obs <-
    as.numeric(oci_mmi_cal_df$`Parameters for Combination Using MMI Technique - Factor Divider 1`[
      which(oci_mmi_cal_df$Subcomponent ==
              "Tapchanger") ])

  factor_divider_2_tf_obs <-
    as.numeric(oci_mmi_cal_df$`Parameters for Combination Using MMI Technique - Factor Divider 2`[
      which(oci_mmi_cal_df$Subcomponent ==
              "Main Transformer") ])

  factor_divider_2_tc_obs <-
    as.numeric(oci_mmi_cal_df$`Parameters for Combination Using MMI Technique - Factor Divider 2`[
      which(oci_mmi_cal_df$Subcomponent ==
              "Tapchanger") ])


  max_no_combined_factors_tf_obs <-
    as.numeric(oci_mmi_cal_df$`Parameters for Combination Using MMI Technique - Max. No. of Combined Factors`[
      which(oci_mmi_cal_df$Subcomponent ==
              "Main Transformer") ])

  max_no_combined_factors_tc_obs <-
    as.numeric(oci_mmi_cal_df$`Parameters for Combination Using MMI Technique - Max. No. of Combined Factors`[
      which(oci_mmi_cal_df$Subcomponent ==
              "Tapchanger") ])


  # Transformer -------------------------------------------------------------

  # Main tank condition
  oci_ehv_tf_main_tank_cond <-
    gb_ref_taken$oci_ehv_tf_main_tank_cond

  Oi_collar_main_tank <-
    oci_ehv_tf_main_tank_cond$`Condition Input Collar`[which(
      oci_ehv_tf_main_tank_cond$`Condition Criteria: Observed Condition` ==
        main_tank)]

  Oi_cap_main_tank <-
    oci_ehv_tf_main_tank_cond$`Condition Input Cap`[which(
      oci_ehv_tf_main_tank_cond$`Condition Criteria: Observed Condition` ==
        main_tank)]

  Oi_factor_main_tank <-
    oci_ehv_tf_main_tank_cond$`Condition Input Factor`[which(
      oci_ehv_tf_main_tank_cond$`Condition Criteria: Observed Condition` ==
        main_tank)]

  # Coolers/Radiator condition

  oci_ehv_tf_cooler_radiatr_cond <-
    gb_ref_taken$oci_ehv_tf_cooler_radiatr_cond

  Oi_collar_coolers_radiator <-
    oci_ehv_tf_cooler_radiatr_cond$`Condition Input Collar`[which(
      oci_ehv_tf_cooler_radiatr_cond$`Condition Criteria: Observed Condition` ==
        coolers_radiator)]

  Oi_cap_coolers_radiator <-
    oci_ehv_tf_cooler_radiatr_cond$`Condition Input Cap`[which(
      oci_ehv_tf_cooler_radiatr_cond$`Condition Criteria: Observed Condition` ==
        coolers_radiator)]

  Oi_factor_coolers_radiator <-
    oci_ehv_tf_cooler_radiatr_cond$`Condition Input Factor`[which(
      oci_ehv_tf_cooler_radiatr_cond$`Condition Criteria: Observed Condition` ==
        coolers_radiator)]


  # Bushings

  oci_ehv_tf_bushings_cond <-
    gb_ref_taken$oci_ehv_tf_bushings_cond

  Oi_collar_bushings <-
    oci_ehv_tf_bushings_cond$`Condition Input Collar`[which(
      oci_ehv_tf_bushings_cond$`Condition Criteria: Observed Condition` ==
        bushings)]

  Oi_cap_bushings <-
    oci_ehv_tf_bushings_cond$`Condition Input Cap`[which(
      oci_ehv_tf_bushings_cond$`Condition Criteria: Observed Condition` ==
        bushings)]

  Oi_factor_bushings <-
    oci_ehv_tf_bushings_cond$`Condition Input Factor`[which(
      oci_ehv_tf_bushings_cond$`Condition Criteria: Observed Condition` ==
        bushings)]

  # Kiosk

  oci_ehv_tf_kiosk_cond <-
    gb_ref_taken$oci_ehv_tf_kiosk_cond

  Oi_collar_kiosk <-
    oci_ehv_tf_kiosk_cond$`Condition Input Collar`[which(
      oci_ehv_tf_kiosk_cond$`Condition Criteria: Observed Condition` ==
        kiosk)]

  Oi_cap_kiosk <-
    oci_ehv_tf_kiosk_cond$`Condition Input Cap`[which(
      oci_ehv_tf_kiosk_cond$`Condition Criteria: Observed Condition` ==
        kiosk)]

  Oi_factor_kiosk <-
    oci_ehv_tf_kiosk_cond$`Condition Input Factor`[which(
      oci_ehv_tf_kiosk_cond$`Condition Criteria: Observed Condition` ==
        kiosk)]


  # Cable box
  oci_ehv_tf_cable_boxes_cond <-
    gb_ref_taken$oci_ehv_tf_cable_boxes_cond

  Oi_collar_cable_boxes <-
    oci_ehv_tf_cable_boxes_cond$`Condition Input Collar`[which(
      oci_ehv_tf_cable_boxes_cond$`Condition Criteria: Observed Condition` ==
        cable_boxes)]

  Oi_cap_cable_boxes <-
    oci_ehv_tf_cable_boxes_cond$`Condition Input Cap`[which(
      oci_ehv_tf_cable_boxes_cond$`Condition Criteria: Observed Condition` ==
        cable_boxes)]

  Oi_factor_cable_boxes <-
    oci_ehv_tf_cable_boxes_cond$`Condition Input Factor`[which(
      oci_ehv_tf_cable_boxes_cond$`Condition Criteria: Observed Condition` ==
        cable_boxes)]


  # Tapchanger --------------------------------------------------------------

  # External condition
  oci_ehv_tf_tapchanger_ext_cond <-
    gb_ref_taken$oci_ehv_tf_tapchanger_ext_cond

  Oi_collar_external_tap <-
    oci_ehv_tf_tapchanger_ext_cond$`Condition Input Collar`[which(
      oci_ehv_tf_tapchanger_ext_cond$`Condition Criteria: Observed Condition` ==
        external_tap)]

  Oi_cap_external_tap <-
    oci_ehv_tf_tapchanger_ext_cond$`Condition Input Cap`[which(
      oci_ehv_tf_tapchanger_ext_cond$`Condition Criteria: Observed Condition` ==
        external_tap)]

  Oi_factor_external_tap <-
    oci_ehv_tf_tapchanger_ext_cond$`Condition Input Factor`[which(
      oci_ehv_tf_tapchanger_ext_cond$`Condition Criteria: Observed Condition` ==
        external_tap)]


  # Internal condition
  oci_ehv_tf_int_cond <-
    gb_ref_taken$oci_ehv_tf_int_cond

  Oi_collar_internal_tap <-
    oci_ehv_tf_int_cond$`Condition Input Collar`[which(
      oci_ehv_tf_int_cond$`Condition Criteria: Observed Condition` ==
        internal_tap)]

  Oi_cap_internal_tap <-
    oci_ehv_tf_int_cond$`Condition Input Cap`[which(
      oci_ehv_tf_int_cond$`Condition Criteria: Observed Condition` ==
        internal_tap)]

  Oi_factor_internal_tap <-
    oci_ehv_tf_int_cond$`Condition Input Factor`[which(
      oci_ehv_tf_int_cond$`Condition Criteria: Observed Condition` ==
        internal_tap)]

  # Drive mechanism
  oci_ehv_tf_drive_mechnism_cond <-
    gb_ref_taken$oci_ehv_tf_drive_mechnism_cond

  Oi_collar_mechnism_cond <-
    oci_ehv_tf_drive_mechnism_cond$`Condition Input Collar`[which(
      oci_ehv_tf_drive_mechnism_cond$`Condition Criteria: Observed Condition` ==
        mechnism_cond)]

  Oi_cap_mechnism_cond <-
    oci_ehv_tf_drive_mechnism_cond$`Condition Input Cap`[which(
      oci_ehv_tf_drive_mechnism_cond$`Condition Criteria: Observed Condition` ==
        mechnism_cond)]

  Oi_factor_mechnism_cond <-
    oci_ehv_tf_drive_mechnism_cond$`Condition Input Factor`[which(
      oci_ehv_tf_drive_mechnism_cond$`Condition Criteria: Observed Condition` ==
        mechnism_cond)]

  # Selecter diverter contacts
  oci_ehv_tf_cond_select_divrter_cst <-
    gb_ref_taken$oci_ehv_tf_cond_select_div_cts

  Oi_collar_diverter_contacts <-
    oci_ehv_tf_cond_select_divrter_cst$`Condition Input Collar`[which(
      oci_ehv_tf_cond_select_divrter_cst$`Condition Criteria: Observed Condition` ==
        diverter_contacts)]

  Oi_cap_diverter_contacts <-
    oci_ehv_tf_cond_select_divrter_cst$`Condition Input Cap`[which(
      oci_ehv_tf_cond_select_divrter_cst$`Condition Criteria: Observed Condition` ==
        diverter_contacts)]

  Oi_factor_diverter_contacts <-
    oci_ehv_tf_cond_select_divrter_cst$`Condition Input Factor`[which(
      oci_ehv_tf_cond_select_divrter_cst$`Condition Criteria: Observed Condition` ==
        diverter_contacts)]


  # Selecter diverter braids
  oci_ehv_tf_cond_select_divrter_brd <-
    gb_ref_taken$oci_ehv_tf_cond_select_div_brd

  Oi_collar_diverter_braids <-
    oci_ehv_tf_cond_select_divrter_brd$`Condition Input Collar`[which(
      oci_ehv_tf_cond_select_divrter_brd$`Condition Criteria: Observed Condition` ==
        diverter_braids)]

  Oi_cap_diverter_braids <-
    oci_ehv_tf_cond_select_divrter_brd$`Condition Input Cap`[which(
      oci_ehv_tf_cond_select_divrter_brd$`Condition Criteria: Observed Condition` ==
        diverter_braids)]

  Oi_factor_diverter_braids <-
    oci_ehv_tf_cond_select_divrter_brd$`Condition Input Factor`[which(
      oci_ehv_tf_cond_select_divrter_brd$`Condition Criteria: Observed Condition` ==
        diverter_braids)]


  # Observed condition factor --------------------------------------

  # Transformer
  factors_tf_obs <- c(Oi_factor_main_tank,
                      Oi_factor_coolers_radiator,
                      Oi_factor_bushings,
                      Oi_factor_kiosk,
                      Oi_factor_cable_boxes)

  observed_condition_factor_tf <- mmi(factors_tf_obs,
                                      factor_divider_1_tf_obs,
                                      factor_divider_2_tf_obs,
                                      max_no_combined_factors_tf_obs)


  # Tapchanger

  factors_tc_obs <- c(Oi_factor_external_tap,
                      Oi_factor_internal_tap,
                      Oi_factor_mechnism_cond,
                      Oi_factor_diverter_contacts,
                      Oi_factor_diverter_braids)

  observed_condition_factor_tc <- mmi(factors_tc_obs,
                                      factor_divider_1_tc_obs,
                                      factor_divider_2_tc_obs,
                                      max_no_combined_factors_tc_obs)



  # Observed condition cap -----------------------------------------

  # Transformer
  caps_tf_obs <- c(Oi_cap_main_tank,
                   Oi_cap_coolers_radiator,
                   Oi_cap_bushings,
                   Oi_cap_kiosk,
                   Oi_cap_cable_boxes)

  observed_condition_cap_tf <- min(caps_tf_obs)

  # Tapchanger

  caps_tc_obs <- c(Oi_cap_external_tap,
                   Oi_cap_internal_tap,
                   Oi_cap_mechnism_cond,
                   Oi_cap_diverter_contacts,
                   Oi_cap_diverter_braids)

  observed_condition_cap_tc <- min(caps_tc_obs)


  # Observed condition collar ---------------------------------------

  # Transformer
  collars_tf_obs <- c(Oi_collar_main_tank,
                      Oi_collar_coolers_radiator,
                      Oi_collar_bushings,
                      Oi_collar_kiosk,
                      Oi_collar_cable_boxes)

  observed_condition_collar_tf <- max(collars_tf_obs)

  # Tapchanger

  collars_tc_obs <- c(Oi_collar_external_tap,
                      Oi_collar_internal_tap,
                      Oi_collar_mechnism_cond,
                      Oi_collar_diverter_contacts,
                      Oi_collar_diverter_braids)

  observed_condition_collar_tc <- max(collars_tc_obs)


  # Observed condition modifier ---------------------------------------------

  # Transformer
  observed_condition_modifier_tf <- data.frame(observed_condition_factor_tf,
                                               observed_condition_cap_tf,
                                               observed_condition_collar_tf)

  # Tapchanger
  observed_condition_modifier_tc <- data.frame(observed_condition_factor_tc,
                                               observed_condition_cap_tc,
                                               observed_condition_collar_tc)


  # Oil test modifier -------------------------------------------------------
  oil_test_mod <- oil_test_modifier(moisture,
                                    acidity,
                                    bd_strength)

  # DGA test modifier -------------------------------------------------------
  dga_test_mod <- dga_test_modifier(hydrogen,
                                    methane,
                                    ethylene,
                                    ethane,
                                    acetylene,
                                    hydrogen_pre,
                                    methane_pre,
                                    ethylene_pre,
                                    ethane_pre,
                                    acetylene_pre)
  # FFA test modifier -------------------------------------------------------
  ffa_test_mod <- ffa_test_modifier(furfuraldehyde)

  # Health score factor ---------------------------------------------------

  health_score_factor_for_tf <-  gb_ref_taken$health_score_factor_for_tf
  health_score_factor_tapchanger <-  gb_ref_taken$health_score_factor_tapchanger


  # Transformer

  factor_divider_1_tf_health <-
    health_score_factor_for_tf$`Parameters for Combination Using MMI Technique - Factor Divider 1`

  factor_divider_2_tf_health <-
    health_score_factor_for_tf$`Parameters for Combination Using MMI Technique - Factor Divider 2`

  max_no_combined_factors_tf_health <-
    health_score_factor_for_tf$`Parameters for Combination Using MMI Technique - Max. No. of Condition Factors`

  # Tapchanger
  factor_divider_1_tc_health <-
    health_score_factor_tapchanger$`Parameters for Combination Using MMI Technique - Factor Divider 1`

  factor_divider_2_tc_health <-
    health_score_factor_tapchanger$`Parameters for Combination Using MMI Technique - Factor Divider 2`

  max_no_combined_factors_tc_health <-
    health_score_factor_tapchanger$`Parameters for Combination Using MMI Technique - Max. No. of Condition Factors`


  # Health score modifier -----------------------------------------------------

  # Transformer
  obs_tf_factor <- observed_condition_modifier_tf$observed_condition_factor_tf
  mea_tf_factor <- measured_condition_modifier_tf$measured_condition_factor_tf
  oil_factor <- oil_test_mod$oil_condition_factor
  dga_factor <- dga_test_mod$dga_test_factor
  ffa_factor <- ffa_test_mod$ffa_test_factor

  factors_tf_health <- c(obs_tf_factor,
                         mea_tf_factor,
                         oil_factor,
                         dga_factor,
                         ffa_factor)

  health_score_factor_tf <- mmi(factors_tf_health,
                                factor_divider_1_tf_health,
                                factor_divider_2_tf_health,
                                max_no_combined_factors_tf_health)
  # tapchanger
  obs_tc_factor <- observed_condition_modifier_tc$observed_condition_factor_tc
  mea_tc_factor <- measured_condition_modifier_tc$measured_condition_factor_tc


  factors_tc_health <- c(obs_tc_factor,
                         mea_tc_factor,
                         oil_factor)


  health_score_factor_tc <- mmi(factors_tc_health,
                                factor_divider_1_tc_health,
                                factor_divider_2_tc_health,
                                max_no_combined_factors_tc_health)

  # Health score cap --------------------------------------------------------

  # Transformer
  health_score_cap_tf <- min(observed_condition_modifier_tf$observed_condition_cap_tf,
                             measured_condition_modifier_tf$measured_condition_cap_tf,
                             oil_test_mod$oil_condition_cap,
                             dga_test_mod$dga_test_cap,
                             ffa_test_mod$ffa_test_cap)

  # Tapchanger
  health_score_cap_tc <- min(observed_condition_modifier_tc$observed_condition_cap_tc,
                             measured_condition_modifier_tc$measured_condition_cap_tc,
                             oil_test_mod$oil_condition_cap)


  # Health score collar -----------------------------------------------------
  # Transformer
  health_score_collar_tf <- max(observed_condition_modifier_tf$observed_condition_collar_tf,
                                measured_condition_modifier_tf$measured_condition_collar_tf,
                                oil_test_mod$oil_condition_collar,
                                dga_test_mod$dga_test_collar,
                                ffa_test_mod$ffa_test_collar)

  # Tapchanger
  health_score_collar_tc <- max(observed_condition_modifier_tc$observed_condition_collar_tc,
                                measured_condition_modifier_tc$measured_condition_collar_tc,
                                oil_test_mod$oil_condition_collar)

  # Health score modifier ---------------------------------------------------

  # transformer
  health_score_modifier_tf <- data.frame(health_score_factor_tf,
                                         health_score_cap_tf,
                                         health_score_collar_tf)
  # Tapchanger
  health_score_modifier_tc <- data.frame(health_score_factor_tc,
                                         health_score_cap_tc,
                                         health_score_collar_tc)

  # Current health score ----------------------------------------------------

  # Transformer

  current_health_score <-
    max(current_health(initial_health_score_tf,
                       health_score_modifier_tf$health_score_factor_tf,
                       health_score_modifier_tf$health_score_cap_tf,
                       health_score_modifier_tf$health_score_collar,
                       reliability_factor = reliability_factor),
        current_health(initial_health_score_tf,
                       health_score_modifier_tc$health_score_factor_tc,
                       health_score_modifier_tc$health_score_cap_tc,
                       health_score_modifier_tc$health_score_collar_tc,
                       reliability_factor = reliability_factor))

  # Probability of failure for the 6.6/11 kV transformer today -----------------
  probability_of_failure <- k *
    (1 + (c * current_health_score) +
       (((c * current_health_score)^2) / factorial(2)) +
       (((c * current_health_score)^3) / factorial(3)))

  return(data.frame(pof = probability_of_failure, chs = current_health_score))
}
