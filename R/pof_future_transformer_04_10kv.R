#' @importFrom magrittr %>%
#' @title Future Probability of Failure for Danish 0.4/10kV Transformers
#' @description This function calculates the future
#' annual probability of failure for Danish 0.4/10kV Transformers.
#' The function is a cubic curve that is based on
#' the first three terms of the Taylor series for an
#' exponential function. For more information about the
#' probability of failure function see section 6
#' on page 34 in CNAIM (2021).
#' @inheritParams pof_transformer_04_10kv
#' @param simulation_end_year Numeric. The last year of simulating probability
#'  of failure. Default is 100.
#' @return Numeric array. Future probability of failure.
#' @source DNO Common Network Asset Indices Methodology (CNAIM),
#' Health & Criticality - Version 2.1, 2021:
#' \url{https://www.ofgem.gov.uk/sites/default/files/docs/2021/04/dno_common_network_asset_indices_methodology_v2.1_final_01-04-2021.pdf}
#' @export
#' @examples
#' # Future probability of a 0.4/10 kV transformer
#' future_pof_transformer <-
#' pof_future_transformer_04_10kv(utilisation_pct = "Default",
#'placement = "Default",
#'altitude_m = "Default",
#'distance_from_coast_km = "Default",
#'corrosion_category_index = "Default",
#'age = 20,
#'partial_discharge = "Default",
#'temperature_reading = "Default",
#'observed_condition = "Default",
#'reliability_factor = "Default",
#'moisture = "Default",
#'acidity = "Default",
#'bd_strength = "Default",
#'simulation_end_year = 100)
# # Plot
#'plot(future_pof_transformer$PoF * 100,
#'type = "line", ylab = "%", xlab = "years",
#'main = "PoF")

pof_future_transformer_04_10kv <- function(utilisation_pct = "Default",
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
                                    simulation_end_year = 100) {

  hv_transformer_type <- "6.6/11kV Transformer (GM)" # this is in order to access tables the are identical to the ones 0.4/10kV transformer is using


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

  # Normal expected life for 0.4/10 kV transformer ------------------------------
  normal_expected_life <- 55 # is set to 55 as seeb on p. 18 in DE-10kV apb kabler CNAIM

  # Constants C and K for PoF function --------------------------------------
  k <- 0.0077/100 # see page 31 in DE-10kV apb kabler CNAIM
  c <- 1.087 # # see page 31 in DE-10kV apb kabler CNAIM

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

    future_health_Score <-
      current_health_score*exp((b2/ageing_reduction_factor) * t)

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

