#' @importFrom magrittr %>%
#' @title Current Probability of Failure for Submarine Cables
#' @description This function calculates the current
#' annual probability of failure per kilometer for submarine cables.
#' The function is a cubic curve that is based on
#' the first three terms of the Taylor series for an
#' exponential function. For more information about the
#' probability of failure function see section 6
#' on page 30 in CNAIM (2017).
#' @param sub_cable_type String.
#' A sting that refers to the specific asset category.
#' See See page 15, table 1 in CNAIM (2017).
#' Options:
#' \code{sub_cable_type =
#' c("HV Sub Cable", "EHV Sub Cable", "132kV Sub Cable")}.
#' The deafult setting is \code{sub_cable_type = "EHV Sub Cable"}.
#' @inheritParams duty_factor_cables
#' @inheritParams location_factor_sub
#' @param sheath_test String. Indicating the state of the sheath. Options:
#' \code{sheath_test = c("Pass", "Failed Minor", "Failed Major",
#' "Default")}. See page 143, table 182 in CNAIM (2017).
#' @param partial_discharge String. Indicating the level of partial discharge.
#' Options:
#' \code{partial_discharge = c("Low", "Medium", "High",
#'  "Default")}. See page 144, table 183 in CNAIM (2017).
#' @param fault_hist Numeric. The calculated fault rate for the cable per annum
#' per kilometer. A setting of \code{"No historic faults recorded"}
#' indicates no fault. See page 144, table 184 in CNAIM (2017).
#' @param condition_armour String. Indicating the external condition of the
#' submarine cables armour. Options:
#' \code{condition_armour = c("Good","Poor","Critical","Default")}
#' @inheritParams current_health
#' @param age Numeric. The current age in years of the cable.
#' @return Numeric. Current probability of failure
#' per annum per kilometre.
#' @source DNO Common Network Asset Indices Methodology (CNAIM),
#' Health & Criticality - Version 1.1, 2017:
#' \url{https://www.ofgem.gov.uk/system/files/docs/2017/05/dno_common_network_asset_indices_methodology_v1.1.pdf}
#' @export
#' @examples
#' # Current annual probability of failure for 1 km EHV Sub Cable
#' pof_subcables <- pof_submarine_cables(
#'  sub_cable_type = "EHV Sub Cable",
#'  utilisation_pct = "Default",
#'  operating_voltage_pct = "Default",
#'  topography = "Default",
#'  sitution = "Default",
#'  wind_wave = "Default",
#'  intensity = "Default",
#'  landlocked = "no",
#'  sheath_test = "Default",
#'  partial_discharge = "Default",
#'  fault_hist = "Default",
#'  condition_armour = "Default",
#'  age = 10,
#'  reliability_factor = "Default"
#')
#'paste0(sprintf("Probability of failure %.4f", pof_subcables),
#'" percent per annum")
#'

pof_submarine_cables <-
  function(sub_cable_type = "EHV Sub Cable",
           utilisation_pct = "Default",
           operating_voltage_pct = "Default",
           topography = "Default",
           sitution = "Default",
           wind_wave = "Default",
           intensity = "Default",
           landlocked = "no",
           sheath_test = "Default",
           partial_discharge = "Default",
           fault_hist = "Default",
           condition_armour = "Default",
           age,
           reliability_factor = "Default") {

    `Asset Register Category` = `Health Index Asset Category` =
      `Generic Term...1` = `Generic Term...2` =
      `Functional Failure Category`= `K-Value (%)` =
      `C-Value` = `Condition Criteria: Sheath Test Result` =
      `Condition Criteria: Partial Discharge Test Result` =
      `Asset Category` = `Condition Criteria` = `Asset Register  Category` = NULL
    # due to NSE notes in R CMD check

    # Ref. table Categorisation of Assets and Generic Terms for Assets  --

    asset_category <- gb_ref$categorisation_of_assets %>%
      dplyr::filter(`Asset Register Category` == sub_cable_type) %>%
      dplyr::select(`Health Index Asset Category`) %>% dplyr::pull()

    generic_term_1 <- gb_ref$generic_terms_for_assets %>%
      dplyr::filter(`Health Index Asset Category` == asset_category) %>%
      dplyr::select(`Generic Term...1`) %>% dplyr::pull()

    generic_term_2 <- gb_ref$generic_terms_for_assets %>%
      dplyr::filter(`Health Index Asset Category` == asset_category) %>%
      dplyr::select(`Generic Term...2`) %>% dplyr::pull()

    # Normal expected life  -------------------------
    normal_expected_life_cable <- gb_ref$normal_expected_life %>%
      dplyr::filter(`Asset Register  Category` == sub_cable_type)  %>%
      dplyr::pull()

    # Constants C and K for PoF function --------------------------------------
    k <- gb_ref$pof_curve_parameters %>%
      dplyr::filter(`Functional Failure Category` ==
                      asset_category) %>% dplyr::select(`K-Value (%)`) %>%
      dplyr::pull()/100

    c <- gb_ref$pof_curve_parameters %>%
      dplyr::filter(`Functional Failure Category` ==
                      asset_category) %>% dplyr::select(`C-Value`) %>%
      dplyr::pull()

    # Duty factor -------------------------------------------------------------
    if (sub_cable_type == "EHV Sub Cable" || sub_cable_type ==
        "132kV Sub Cable") {
      sub_marine_col_level <- "EHV"
    } else {
      sub_marine_col_level <- "LV & HV"
    }

    duty_factor_sub <-
      duty_factor_cables(
        utilisation_pct,
        operating_voltage_pct,
        voltage_level = sub_marine_col_level)


    # Location factor ---------------------------------------------------------
    lf_submarine <- location_factor_sub(topography,
                                        sitution,
                                        wind_wave,
                                        intensity,
                                        landlocked)


    # Expected life ------------------------------
    expected_life_years <- expected_life(normal_expected_life_cable,
                                         duty_factor_sub,
                                         location_factor = lf_submarine)

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
    # These overriding values are shown in Table 34 to Table 195
    # and Table 200 in Appendix B.

    # Measured condition inputs ---------------------------------------------
    mcm_mmi_cal_df <-
      gb_ref$measured_cond_modifier_mmi_cal


    mcm_mmi_cal_df <-
      mcm_mmi_cal_df[which(
        mcm_mmi_cal_df$`Asset Category` == "Submarine Cable"), ]

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


    # Sheath test -------------------------------------------------------------
    mci_submarine_cbl_sheath_test <-
      gb_ref$mci_submarine_cbl_sheath_test %>% dplyr::filter(
        `Condition Criteria: Sheath Test Result` == sheath_test
      )

    ci_factor_sheath <-
      mci_submarine_cbl_sheath_test$`Condition Input Factor`
    ci_cap_sheath <-
      mci_submarine_cbl_sheath_test$`Condition Input Cap`
    ci_collar_sheath <-
      mci_submarine_cbl_sheath_test$`Condition Input Collar`

    # Partial discharge-------------------------------------------------------
    mci_submarine_cable_prtl_disc <-
      gb_ref$mci_submarine_cable_prtl_disc %>%
      dplyr::filter(
        `Condition Criteria: Partial Discharge Test Result` == partial_discharge
      )

    ci_factor_partial <-
      mci_submarine_cable_prtl_disc$`Condition Input Factor`
    ci_cap_partial <- mci_submarine_cable_prtl_disc$`Condition Input Cap`
    ci_collar_partial <-
      mci_submarine_cable_prtl_disc$`Condition Input Collar`


    # Fault -------------------------------------------------------
    mci_submarine_cable_fault_hist <-
      gb_ref$mci_submarine_cable_fault_hist

    for (n in 2:4) {
      if (fault_hist == 'Default' || fault_hist ==
          'No historic faults recorded') {
        no_row <- which(mci_submarine_cable_fault_hist$Upper == fault_hist)

        ci_factor_fault <-
          mci_submarine_cable_fault_hist$`Condition Input Factor`[no_row]
        ci_cap_fault <-
          mci_submarine_cable_fault_hist$`Condition Input Cap`[no_row]
        ci_collar_fault <-
          mci_submarine_cable_fault_hist$`Condition Input Collar`[no_row]
        break
      } else if (fault_hist >=
                 as.numeric(mci_submarine_cable_fault_hist$Lower[n]) &
                 fault_hist <
                 as.numeric(mci_submarine_cable_fault_hist$Upper[n])) {

        ci_factor_fault <-
          mci_submarine_cable_fault_hist$`Condition Input Factor`[n]
        ci_cap_fault <-
          mci_submarine_cable_fault_hist$`Condition Input Cap`[n]
        ci_collar_fault <-
          mci_submarine_cable_fault_hist$`Condition Input Collar`[n]

        break
      }
    }

    # Measured conditions

    factors <- c(ci_factor_sheath,
                 ci_factor_partial,
                 ci_factor_fault)

    measured_condition_factor <- mmi(factors,
                                     factor_divider_1,
                                     factor_divider_2,
                                     max_no_combined_factors)

    caps <- c(ci_cap_sheath,
              ci_cap_partial,
              ci_cap_fault)
    measured_condition_cap <- min(caps)

    # Measured condition collar ----------------------------------------------
    collars <- c(ci_collar_sheath,
                 ci_collar_partial,
                 ci_collar_fault)
    measured_condition_collar <- max(collars)


    # Measured condition modifier ---------------------------------------------
    measured_condition_modifier <- data.frame(measured_condition_factor,
                                              measured_condition_cap,
                                              measured_condition_collar)



    # Observed conditions -----------------------------------------------------

    oci_mmi_cal_df <-
      gb_ref$observed_cond_modifier_mmi_cal %>%
      dplyr::filter(`Asset Category` == "Submarine Cable")

    factor_divider_1_oi <-
      as.numeric(oci_mmi_cal_df$`Parameters for Combination Using MMI Technique - Factor Divider 1`)

    factor_divider_2_oi <-
      as.numeric(oci_mmi_cal_df$`Parameters for Combination Using MMI Technique - Factor Divider 2`)

    max_no_combined_factors_oi <-
      as.numeric(oci_mmi_cal_df$`Parameters for Combination Using MMI Technique - Max. No. of Combined Factors`)



    # External conditions of armour

    oci_submrn_cable_ext_cond_armr <-
      gb_ref$oci_submrn_cable_ext_cond_armr %>%
      dplyr::filter(
        `Condition Criteria` == condition_armour
      )

    oi_factor <-
      oci_submrn_cable_ext_cond_armr$`Condition Input Factor`
    oi_cap <- oci_submrn_cable_ext_cond_armr$`Condition Input Cap`
    oi_collar <-
      oci_submrn_cable_ext_cond_armr$`Condition Input Collar`


    observed_condition_factor <- mmi(oi_factor,
                                     factor_divider_1_oi,
                                     factor_divider_2_oi,
                                     max_no_combined_factors_oi)


    observed_condition_cap <- oi_cap
    observed_condition_collar <- oi_collar

    observed_condition_modifier <- data.frame(observed_condition_factor,
                                              observed_condition_cap,
                                              observed_condition_collar)

    # Health score factor ---------------------------------------------------
    health_score_factor <- health_score_excl_ehv_132kv_tf(
      observed_condition_modifier$observed_condition_factor,
      measured_condition_modifier$measured_condition_factor)


    # Health score cap --------------------------------------------------------
    health_score_cap <-
      min(observed_condition_modifier$observed_condition_cap,
          measured_condition_modifier$measured_condition_cap)

    # Health score collar -----------------------------------------------------
    health_score_collar <-
      min(observed_condition_modifier$observed_condition_collar,
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

    # Probability of failure for the 6.6/11 kV transformer today ---------------
    probability_of_failure <- k *
      (1 + (c * current_health_score) +
         (((c * current_health_score)^2) / factorial(2)) +
         (((c * current_health_score)^3) / factorial(3)))


    return(probability_of_failure)
  }
