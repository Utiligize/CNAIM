#' @importFrom magrittr %>%
#' @title Current Probability of Failure for HV Switchgear Distribution
#' @description This function calculates the current
#' annual probability of failure per kilometer HV Switchgear Distribution
#' The function is a cubic curve that is based on
#' the first three terms of the Taylor series for an
#' exponential function. For more information about the
#' probability of failure function see section 6
#' on page 34 in CNAIM (2021).
#' @param hv_asset_category String The type of LV asset category
#' @param placement String. Specify if the asset is located outdoor or indoor.
#' @param altitude_m Numeric. Specify the altitude location for
#' the asset measured in meters from sea level.\code{altitude_m}
#' is used to derive the altitude factor. See page 111,
#' table 23 in CNAIM (2021). A setting of \code{"Default"}
#' will set the altitude factor to 1 independent of \code{asset_type}.
#' @param distance_from_coast_km Numeric. Specify the distance from the
#' coast measured in kilometers. \code{distance_from_coast_km} is used
#' to derive the distance from coast factor See page 110,
#' table 22 in CNAIM (2021). A setting of \code{"Default"} will set the
#'  distance from coast factor to 1 independent of \code{asset_type}.
#' @param corrosion_category_index Integer.
#' Specify the corrosion index category, 1-5.
#' @param age  Numeric. The current age in years of the conductor.
#' @param measured_condition_inputs Named list observed_conditions_input
#' @param observed_condition_inputs Named list observed_conditions_input
#' \code{conductor_samp = c("Low","Medium/Normal","High","Default")}.
#' See page 161, table 199 and 201 in CNAIM (2021).
#' @inheritParams current_health
#' @param gb_ref_given optional parameter to use custom reference values
#' @return DataFrame Current probability of failure
#' per annum per kilometer along with current health score.
#' @source DNO Common Network Asset Indices Methodology (CNAIM),
#' Health & Criticality - Version 2.1, 2021:
#' \url{https://www.ofgem.gov.uk/sites/default/files/docs/2021/04/dno_common_network_asset_indices_methodology_v2.1_final_01-04-2021.pdf}
#' @export
#' @examples
#' # Current annual probability of failure for HV Swicthgear distribution
#' pof_hv_switchgear_distribution(
#' hv_asset_category = "6.6/11kV CB (GM) Secondary",
#' placement = "Default",
#' altitude_m = "Default",
#' distance_from_coast_km = "Default",
#' corrosion_category_index = "Default",
#' age = 10,
#' observed_condition_inputs =
#' list("external_condition" =
#' list("Condition Criteria: Observed Condition" = "Default"),
#' "oil_gas" = list("Condition Criteria: Observed Condition" = "Default"),
#' "thermo_assment" = list("Condition Criteria: Observed Condition" = "Default"),
#' "internal_condition" = list("Condition Criteria: Observed Condition" = "Default"),
#' "indoor_env" = list("Condition Criteria: Observed Condition" = "Default")),
#' measured_condition_inputs =
#' list("partial_discharge" =
#' list("Condition Criteria: Partial Discharge Test Results" = "Default"),
#' "ductor_test" = list("Condition Criteria: Ductor Test Results" = "Default"),
#' "oil_test" = list("Condition Criteria: Oil Test Results" = "Default"),
#' "temp_reading" = list("Condition Criteria: Temperature Readings" = "Default"),
#' "trip_test" = list("Condition Criteria: Trip Timing Test Result" = "Default")),
#' reliability_factor = "Default")
pof_hv_switchgear_distribution <-
  function(hv_asset_category = "6.6/11kV CB (GM) Secondary",
           placement = "Default",
           altitude_m = "Default",
           distance_from_coast_km = "Default",
           corrosion_category_index = "Default",
           age,
           measured_condition_inputs,
           observed_condition_inputs,
           reliability_factor = "Default",
           gb_ref_given = NULL) {

    `Asset Register Category` = `Health Index Asset Category` =
      `Generic Term...1` = `Generic Term...2` = `Functional Failure Category` =
      `K-Value (%)` = `C-Value` = `Asset Register  Category` = NULL
    # due to NSE notes in R CMD check
    if(is.null(gb_ref_given)){
      gb_ref_taken <- gb_ref
    }else{
      check_gb_ref_given(gb_ref_given)
      gb_ref_taken <- gb_ref_given
    }

    asset_category <- gb_ref_taken$categorisation_of_assets %>%
      dplyr::filter(`Asset Register Category` ==
                      hv_asset_category) %>%
      dplyr::select(`Health Index Asset Category`) %>% dplyr::pull()

    generic_term_1 <- gb_ref_taken$generic_terms_for_assets %>%
      dplyr::filter(`Health Index Asset Category` == asset_category) %>%
      dplyr::select(`Generic Term...1`) %>% dplyr::pull()

    generic_term_2 <- gb_ref_taken$generic_terms_for_assets %>%
      dplyr::filter(`Health Index Asset Category` == asset_category) %>%
      dplyr::select(`Generic Term...2`) %>% dplyr::pull()

    # Normal expected life  -------------------------
    normal_expected_life_cond <- gb_ref_taken$normal_expected_life %>%
      dplyr::filter(`Asset Register  Category` ==
                      hv_asset_category) %>%
      dplyr::pull()

    # Constants C and K for PoF function --------------------------------------

    # POF function asset category. This is bit different from other tables
    kc_hv_asset_category <- "HV Switchgear (GM) - Distribution (GM)"

    k <- gb_ref_taken$pof_curve_parameters %>%
      dplyr::filter(`Functional Failure Category` %in% kc_hv_asset_category) %>%
      dplyr::select(`K-Value (%)`) %>%
      dplyr::pull()/100

    c <- gb_ref_taken$pof_curve_parameters %>%
      dplyr::filter(`Functional Failure Category` %in% kc_hv_asset_category) %>%
      dplyr::select(`C-Value`) %>%
      dplyr::pull()

    # Duty factor -------------------------------------------------------------

    duty_factor_cond <- 1

    # Location factor ----------------------------------------------------
    location_factor_cond <- location_factor(placement,
                                            altitude_m,
                                            distance_from_coast_km,
                                            corrosion_category_index,
                                            asset_type = hv_asset_category)
    # Expected life ------------------------------
    expected_life_years <- expected_life(normal_expected_life_cond,
                                         duty_factor_cond,
                                         location_factor_cond)

    # b1 (Initial Ageing Rate) ------------------------------------------------
    b1 <- beta_1(expected_life_years)

    # Initial health score ----------------------------------------------------
    initial_health_score <- initial_health(b1, age)

    # Measured conditions
    mci_table_names <- list("partial_discharge" = "mci_hv_swg_distr_prtl_dischrg",
                            "ductor_test" = "mci_hv_swg_distr_ductor_test",
                            "oil_test" = "mci_hv_swg_distr_oil_test",
                            "temp_reading" = "mci_hv_swg_distr_temp_reading",
                            "trip_test" = "mci_hv_swg_distr_trip_test")

    measured_condition_modifier <-
      get_measured_conditions_modifier_hv_switchgear(asset_category,
                                                     mci_table_names,
                                                     measured_condition_inputs,
                                                     gb_ref_taken = gb_ref_taken)

    # Observed conditions -----------------------------------------------------

    oci_table_names <- list("external_condition" = "oci_hv_swg_dist_swg_ext_cond",
                            "oil_gas" = "oci_hv_swg_dist_oil_lek_gas_pr",
                            "thermo_assment" = "oci_hv_swg_dist_thermo_assment",
                            "internal_condition" = "oci_hv_swg_dist_swg_int_cond",
                            "indoor_env" = "oci_hv_swg_dist_indoor_environ")

    observed_condition_modifier <-
      get_observed_conditions_modifier_hv_switchgear(asset_category,
                                                     oci_table_names,
                                                     observed_condition_inputs,
                                                     gb_ref_taken = gb_ref_taken)

    # Health score factor ---------------------------------------------------
    health_score_factor <-
      health_score_excl_ehv_132kv_tf(observed_condition_modifier$condition_factor,
                                     measured_condition_modifier$condition_factor)

    # Health score cap --------------------------------------------------------
    health_score_cap <- min(observed_condition_modifier$condition_cap,
                            measured_condition_modifier$condition_cap)

    # Health score collar -----------------------------------------------------
    health_score_collar <-  max(observed_condition_modifier$condition_collar,
                                measured_condition_modifier$condition_collar)

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

    # Probability of failure ---------------------------------------------------
    probability_of_failure <- k *
      (1 + (c * current_health_score) +
         (((c * current_health_score)^2) / factorial(2)) +
         (((c * current_health_score)^3) / factorial(3)))


    return(data.frame(pof = probability_of_failure, chs = current_health_score))
  }


get_measured_conditions_modifier_hv_switchgear <- function(asset_category_mmi, table_names,
                                                           measured_condition_inputs, sub_component = NULL,
                                                           gb_ref_taken = NULL){
  mcm_mmi_cal_df <-
    gb_ref_taken$measured_cond_modifier_mmi_cal

  mcm_mmi_cal_df <-
    mcm_mmi_cal_df[which(
      mcm_mmi_cal_df$`Asset Category` == asset_category_mmi), ]

  factor_divider_1 <-
    as.numeric(
      mcm_mmi_cal_df$
        `Parameters for Combination Using MMI Technique - Factor Divider 1`)

  factor_divider_2 <-
    as.numeric(
      mcm_mmi_cal_df$
        `Parameters for Combination Using MMI Technique - Factor Divider 2`)

  max_no_combined_factors <-
    as.numeric(
      mcm_mmi_cal_df$
        `Parameters for Combination Using MMI Technique - Max. No. of Combined Factors`
    )

  # Measured inputs-----------------------------------------------------------
  factor_dfs <- list()
  for(table_name in names(table_names)){
    gb_ref_table_name <- table_names[[table_name]]
    mci_table <- gb_ref[[gb_ref_table_name]]
    mci_table_check_col_name <- names(measured_condition_inputs[[table_name]])[1]
    mci_table_check_col_val <- measured_condition_inputs[[table_name]][1]
    row_number <- which(mci_table[[mci_table_check_col_name]] ==
                          mci_table_check_col_val)
    factor_df <- mci_table[row_number,] %>%
      dplyr::select(c("Condition Input Factor", "Condition Input Cap",
                      "Condition Input Collar"))
    factor_dfs[[table_name]] <- factor_df
  }

  mci_factor_df <- factor_dfs %>% plyr::ldply()

  measured_condition_factor <- mmi(mci_factor_df[["Condition Input Factor"]],
                                   factor_divider_1,
                                   factor_divider_2,
                                   max_no_combined_factors)

  measured_condition_cap <- min(mci_factor_df[["Condition Input Cap"]])

  measured_condition_collar <- max(mci_factor_df[["Condition Input Collar"]])

  # Measured condition modifier ---------------------------------------------
  measured_condition_modifier <- data.frame(condition_factor = measured_condition_factor,
                                            condition_cap = measured_condition_cap,
                                            condition_collar = measured_condition_collar)

  return(measured_condition_modifier)
}


get_observed_conditions_modifier_hv_switchgear <- function(asset_category_mmi, table_names,
                                                           observed_condition_inputs, sub_component = NULL,
                                                           gb_ref_taken = NULL){
  oci_mmi_cal_df <-
    gb_ref_taken$observed_cond_modifier_mmi_cal

  oci_mmi_cal_df <-
    oci_mmi_cal_df[which(
      oci_mmi_cal_df$`Asset Category` == asset_category_mmi), ]

  if(!is.null(sub_component)){
    oci_mmi_cal_df <-
      oci_mmi_cal_df[which(
        oci_mmi_cal_df$`Subcomponent` == sub_component), ]
  }

  factor_divider_1 <-
    as.numeric(
      oci_mmi_cal_df$`Parameters for Combination Using MMI Technique - Factor Divider 1`)

  factor_divider_2 <-
    as.numeric(
      oci_mmi_cal_df$`Parameters for Combination Using MMI Technique - Factor Divider 2`)

  max_no_combined_factors <-
    as.numeric(
      oci_mmi_cal_df$
        `Parameters for Combination Using MMI Technique - Max. No. of Combined Factors`
    )

  # Observed inputs-----------------------------------------------------------
  factor_dfs <- list()
  for(table_name in names(table_names)){
    gb_ref_table_name <- table_names[[table_name]]
    oci_table <- gb_ref[[gb_ref_table_name]]
    oci_table_check_col_name <- names(observed_condition_inputs[[table_name]])[1]
    oci_table_check_col_val <- observed_condition_inputs[[table_name]][1]
    row_number <- which(oci_table[[oci_table_check_col_name]] ==
                          oci_table_check_col_val)
    factor_df <- oci_table[row_number,] %>%
      dplyr::select(c("Condition Input Factor", "Condition Input Cap",
                      "Condition Input Collar"))
    factor_dfs[[table_name]] <- factor_df
  }

  oci_factor_df <- factor_dfs %>% plyr::ldply()

  observed_condition_factor <- mmi(oci_factor_df[["Condition Input Factor"]],
                                   factor_divider_1,
                                   factor_divider_2,
                                   max_no_combined_factors)

  observed_condition_cap <- min(oci_factor_df[["Condition Input Cap"]])

  observed_condition_collar <- max(oci_factor_df[["Condition Input Collar"]])

  # Observed condition modifier ---------------------------------------------
  observed_condition_modifier <- data.frame(condition_factor = observed_condition_factor,
                                            condition_cap = observed_condition_cap,
                                            condition_collar = observed_condition_collar)

  return(observed_condition_modifier)

}
