#' @importFrom magrittr %>%
#' @title Future Probability of Failure for 30kV and 60kV Oil submarine cables
#' @description This function calculates the future
#' annual probability of failure per kilometer for a 30kV and 60kV oil submarine cables
#' The function is a cubic curve that is based on
#' the first three terms of the Taylor series for an
#' exponential function.
#' @inheritParams pof_submarine_cables_30_60kv_oil
#' @param simulation_end_year Numeric. The last year of simulating probability
#'  of failure. Default is 100.
#' @return Numeric array. Future probability of failure
#' per annum per kilometre for 30kV and 60kV submarine cables
#' @export
#' @examples
#' # Future annual probability of failure for 1 km 30kV and 60kV oil Sub Cable
# pof_future_submarine_cables_30_60kv_oil(
# utilisation_pct = "Default",
# operating_voltage_pct = "Default",
# topography = "Default",
# sitution = "Default",
# wind_wave = "Default",
# intensity = "Default",
# landlocked = "no",
# sheath_test = "Default",
# partial_discharge = "Default",
# fault_hist = "Default",
# condition_armour = "Default",
# age = 10,
# reliability_factor = "Default",
# k_value = 0.0202,
# c_value = 1.087,
# normal_expected_life = 60,
# simulation_end_year = 100)


pof_future_submarine_cables_30_60kv_oil <-
  function(utilisation_pct = "Default",
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
           reliability_factor = "Default",
           k_value = 2.0944,
           c_value = 1.087,
           normal_expected_life = 60,
           simulation_end_year = 100) {

    sub_cable_type <- "EHV Sub Cable"
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


    # Constants C and K for PoF function --------------------------------------
    k <- k_value/100

    c <- c_value

    # Duty factor -------------------------------------------------------------
      sub_marine_col_level <- "EHV"


    duty_factor_sub <-
      duty_factor_cables(
        utilisation_pct,
        operating_voltage_pct,
        voltage_level = sub_marine_col_level)


    # # Location factor ---------------------------------------------------------
    lf_submarine <- location_factor_sub(topography,
                                        sitution,
                                        wind_wave,
                                        intensity,
                                        landlocked)

    # Expected life ------------------------------
    expected_life_years <- expected_life(normal_expected_life,
                                         duty_factor_sub,
                                         lf_submarine)

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

