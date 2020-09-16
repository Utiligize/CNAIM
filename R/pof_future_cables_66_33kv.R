#' @importFrom magrittr %>%
#' @title Future Probability of Failure for 33-66kV cables
#' @description This function calculates the future
#' annual probability of failure per kilometre for a 33-66kV cables.
#' The function is a cubic curve that is based on
#' the first three terms of the Taylor series for an
#' exponential function. For more information about the
#' probability of failure function see section 6
#' on page 30 in CNAIM (2017). ###################
#' @param cable_type String.
#' A sting that refers to the specific asset category.
#' See See page 15, table 1 in CNAIM (2017).
#' Options:
#' \code{cable_type = c("33kV UG Cable (Gas)", "66kV UG Cable (Gas)",
#' "33kV UG Cable (Non Pressurised)", "66kV UG Cable (Non Pressurised)",
#' "33kV UG Cable (Oil)", "66kV UG Cable (Oil)")
#'}
#' @param sub_division String. Refers to material the sheath and conductor is
#' made of. Options:
#' \code{sub_division = c("Aluminium sheath - Aluminium conductor",
#' "Aluminium sheath - Copper conductor",
#' "Lead sheath - Aluminium conductor", "Lead sheath - Copper conductor")
#'}
#' @inheritParams duty_factor_cables_u66kv
#' @param sheath_test String. Only applied for non pressurised cables.
#' Indicating the state of the sheath. Options:
#' \code{sheath_test = c("Pass", "Failed Minor", "Failed Major",
#' "Default")}. See page 141, table 168 in CNAIM (2017).
#' @param partial_discharge String. Only applied for non pressurised cables.
#' Indicating the level of partial discharge. Options:
#' \code{partial_discharge = c("Low", "Medium", "High",
#'  "Default")}. See page 141, table 169 in CNAIM (2017).
#' @param fault_hist Numeric. Only applied for non pressurised cables.
#' The calculated fault rate for the cable in the period per kilometer.
#' A setting of \code{"No historic faults recorded"}
#' indicates no fault. See page 141, table 170 in CNAIM (2017).
#' @param leakage String. Only applied for oil and gas cables. Options:
#' \code{leakage = c("No (or very low) historic leakage recorded",
#' "Low/ moderate", "High", "Very High", "Default")}.
#' See page 142, table 171 (oil) and 172 (gas) in CNAIM (2017).
#' @param age  Numeric. The current age in years of the cable.
#' @param simulation_end_year Numeric. The last year of simulating probability
#'  of failure. Default is 100.
#' @return Numeric array. Future probability of failure
#' per annum per kilometre for 33-66kV cables.
#' @source DNO Common Network Asset Indices Methodology (CNAIM),
#' Health & Criticality - Version 1.1, 2017:
#' \url{https://www.ofgem.gov.uk/system/files/docs/2017/05/dno_common_network_asset_indices_methodology_v1.1.pdf}
#' @export
#' @examples
#' # Future probability of failure for 66kV UG Cable (Non Pressurised)
#' pof_66kV_non_pressurised <-
#' pof_future_cables_66_33kv(cable_type = "66kV UG Cable (Non Pressurised)",
#'sub_division = "Aluminium sheath - Aluminium conductor",
#'utilisation_pct = 75,
#'operating_voltage_pct = 50,
#'sheath_test = "Default",
#'partial_discharge = "Default",
#'fault_hist = "Default",
#'leakage = "Default",
#'age = 1,
#'simulation_end_year = 100)
#' # Plot
#'plot(pof_66kV_non_pressurised$PoF * 100,
#'type = "line", ylab = "%", xlab = "years",
#'main = "PoF per kilometre - 66kV UG Cable (Non Pressurised)")


pof_future_cables_66_33kv <-
  function(cable_type = "33kV UG Cable (Gas)",
           sub_division = "Aluminium sheath - Aluminium conductor",
           utilisation_pct = "Default",
           operating_voltage_pct = "Default",
           sheath_test = "Default",
           partial_discharge = "Default",
           fault_hist = "Default",
           leakage = "Default",
           age,
           simulation_end_year = 100) {

    `Asset Register Category` = `Health Index Asset Category` =
      `Generic Term...1` = `Generic Term...2` = `Functional Failure Category` =
      `K-Value (%)` = `C-Value` = `Asset Register  Category` = NULL
    # due to NSE notes in R CMD check

    # Ref. table Categorisation of Assets and Generic Terms for Assets  --

    asset_category <- gb_ref$categorisation_of_assets %>%
      dplyr::filter(`Asset Register Category` == cable_type) %>%
      dplyr::select(`Health Index Asset Category`) %>% dplyr::pull()

    generic_term_1 <- gb_ref$generic_terms_for_assets %>%
      dplyr::filter(`Health Index Asset Category` == asset_category) %>%
      dplyr::select(`Generic Term...1`) %>% dplyr::pull()

    generic_term_2 <- gb_ref$generic_terms_for_assets %>%
      dplyr::filter(`Health Index Asset Category` == asset_category) %>%
      dplyr::select(`Generic Term...2`) %>% dplyr::pull()

    # Normal expected life for 6.6/11 kV transformer ------------------------------
    normal_expected_life_cable <- gb_ref$normal_expected_life %>%
      dplyr::filter(`Asset Register  Category` == cable_66_33kV_type &
                      `Sub-division` == sub_division) %>%
      dplyr::pull()

    # Constants C and K for PoF function --------------------------------------
    if (asset_category == "EHV UG Cable (Non Pressurised)") {
      type_k_c <- gb_ref$pof_curve_parameters$`Functional Failure Category`[which(
        grepl("Non Pressurised",
              gb_ref$pof_curve_parameters$`Functional Failure Category`,
              fixed = TRUE) == TRUE
      )]
    } else {

      type_k_c <- gb_ref$pof_curve_parameters$`Functional Failure Category`[which(
        grepl(asset_category,
              gb_ref$pof_curve_parameters$`Functional Failure Category`,
              fixed = TRUE) == TRUE
      )]
    }

    k <- gb_ref$pof_curve_parameters %>%
      dplyr::filter(`Functional Failure Category` ==
                      type_k_c) %>% dplyr::select(`K-Value (%)`) %>%
      dplyr::pull()/100

    c <- gb_ref$pof_curve_parameters %>%
      dplyr::filter(`Functional Failure Category` ==
                      type_k_c) %>% dplyr::select(`C-Value`) %>%
      dplyr::pull()

    # Duty factor -------------------------------------------------------------

    duty_factor_cable <-
      duty_factor_cables_u66kv(utilisation_pct,
                               operating_voltage_pct,
                               voltage_level = "EHV") # Husk her ***

    # Expected life ------------------------------
    expected_life_years <- expected_life(normal_expected_life_cable,
                                         duty_factor_cable,
                                         location_factor = 1)

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
    asset_category_mmi <- stringr::str_remove(asset_category, pattern = "UG")
    asset_category_mmi <- stringr::str_squish(asset_category_mmi)


    mcm_mmi_cal_df <-
      gb_ref$measured_cond_modifier_mmi_cal

    mmi_type <- mcm_mmi_cal_df$`Asset Category`[which(
      grepl(asset_category_mmi,
            mcm_mmi_cal_df$`Asset Category`,
            fixed = TRUE) == TRUE
    )]


    mcm_mmi_cal_df <-
      mcm_mmi_cal_df[which(mcm_mmi_cal_df$`Asset Category` == asset_category_mmi), ]

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


    # Sheath test -------------------------------------------------------------
    if (asset_category == "EHV UG Cable (Non Pressurised)") {

      mci_ehv_cbl_non_pr_sheath_test <-
        gb_ref$mci_ehv_cbl_non_pr_sheath_test %>% dplyr::filter(
          `Condition Criteria: Sheath Test Result` == sheath_test
        )

      ci_factor_sheath <- mci_ehv_cbl_non_pr_sheath_test$`Condition Input Factor`
      ci_cap_sheath <- mci_ehv_cbl_non_pr_sheath_test$`Condition Input Cap`
      ci_collar_sheath <- mci_ehv_cbl_non_pr_sheath_test$`Condition Input Collar`


      mci_ehv_cbl_non_pr_prtl_disch <-
        gb_ref$mci_ehv_cbl_non_pr_prtl_disch %>% dplyr::filter(
          `Condition Criteria: Partial Discharge Test Result` == partial_discharge
        )


      ci_factor_partial <- mci_ehv_cbl_non_pr_prtl_disch$`Condition Input Factor`
      ci_cap_partial <- mci_ehv_cbl_non_pr_prtl_disch$`Condition Input Cap`
      ci_collar_partial <- mci_ehv_cbl_non_pr_prtl_disch$`Condition Input Collar`

      mci_ehv_cbl_non_pr_fault_hist <-
        gb_ref$mci_ehv_cbl_non_pr_fault_hist

      for (n in 2:4) {
        if (fault_hist == 'Default' || fault_hist == 'No historic faults recorded') {
          no_row <- which(mci_ehv_cbl_non_pr_fault_hist$Upper == fault_hist)

          ci_factor_fault <- mci_ehv_cbl_non_pr_fault_hist$`Condition Input Factor`[no_row]
          ci_cap_fault <- mci_ehv_cbl_non_pr_fault_hist$`Condition Input Cap`[no_row]
          ci_collar_fault <- mci_ehv_cbl_non_pr_fault_hist$`Condition Input Collar`[no_row]
          break
        } else if (fault_hist > as.numeric(mci_ehv_cbl_non_pr_fault_hist$Lower[n]) &
                   fault_hist <=
                   as.numeric(mci_ehv_cbl_non_pr_fault_hist$Upper[n])) {

          ci_factor_fault <- mci_ehv_cbl_non_pr_fault_hist$`Condition Input Factor`[n]
          ci_cap_fault <- mci_ehv_cbl_non_pr_fault_hist$`Condition Input Cap`[n]
          ci_collar_fault <- mci_ehv_cbl_non_pr_fault_hist$`Condition Input Collar`[n]

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

      # Measured condition collar -----------------------------------------------
      collars <- c(ci_collar_sheath,
                   ci_collar_partial,
                   ci_collar_fault)
      measured_condition_collar <- max(collars)

    } else if (asset_category == "EHV UG Cable (Oil)") {

      mci_ehv_cable_oil_leakage <-
        gb_ref$mci_ehv_cable_oil_leakage %>% dplyr::filter(
          `Condition Criteria: Leakage Rate` == leakage
        )

      ci_factor_leakage_oil <- mci_ehv_cable_oil_leakage$`Condition Input Factor`
      ci_cap_leakage_oil <- mci_ehv_cable_oil_leakage$`Condition Input Cap`
      ci_collar_leakage_oil <- mci_ehv_cable_oil_leakage$`Condition Input Collar`

      # Measured conditions

      measured_condition_factor <- ci_factor_leakage_oil
      measured_condition_cap <- ci_cap_leakage_oil
      measured_condition_collar <- ci_collar_leakage_oil


    } else if (asset_category == "EHV UG Cable (Gas)") {

      mci_ehv_cbl_gas <-
        gb_ref$mci_ehv_cable_gas_leakage %>% dplyr::filter(
          `Condition Criteria: Leakage Rate` == leakage
        )

      ci_factor_leakage_gas <- mci_ehv_cbl_gas$`Condition Input Factor`
      ci_cap_leakage_gas <- mci_ehv_cbl_gas$`Condition Input Cap`
      ci_collar_leakage_gas <- mci_ehv_cbl_gas$`Condition Input Collar`

      # Measured conditions

      measured_condition_factor <- ci_factor_leakage_gas
      measured_condition_cap <- ci_cap_leakage_gas
      measured_condition_collar <- ci_collar_leakage_gas

    }


    # Measured condition modifier ---------------------------------------------
    measured_condition_modifier <- data.frame(measured_condition_factor,
                                              measured_condition_cap,
                                              measured_condition_collar)


    # Health score factor ---------------------------------------------------
    health_score_factor <- measured_condition_modifier$measured_condition_factor

    # Health score cap --------------------------------------------------------
    health_score_cap <- measured_condition_modifier$measured_condition_cap

    # Health score collar -----------------------------------------------------
    health_score_collar <- measured_condition_modifier$measured_condition_collar

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
                     reliability_factor)

    # Probability of failure for the 6.6/11 kV transformer today ------------------
    probability_of_failure <- k *
      (1 + (c * current_health_score) +
         (((c * current_health_score)^2) / factorial(2)) +
         (((c * current_health_score)^3) / factorial(3)))

    # Future probability of failure -------------------------------------------

    # the Health Score of a new asset
    H_new = 0.5

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

    # Dynamic bit -------------------------------------------------------------
    pof_year <- list()
    years <- seq(from=age,to=simulation_end_year,by=1)
    for (y in 1:length(years)){
      t <- years[y]

      if (t == 1){
        H <- current_health_score
      } else {
        future_health_Score <- current_health_score*exp((b2/ageing_reduction_factor) * t)
        H <- future_health_Score
      }

      future_health_score_limit <- 15 # See page 36
      if (H > future_health_score_limit){
        H <- future_health_score_limit
      }

      pof_year[[paste(t)]] <- k * (1 + (c * H) +
                                     (((c * H)^2) / factorial(2)) +
                                     (((c * H)^3) / factorial(3)))
    }

    return(data.frame(years=years, PoF=as.numeric(unlist(pof_year))))
  }
