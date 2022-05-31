#' @importFrom magrittr %>%
#' @title Current Probability of Failure for 30-60kV cables
#' @description This function calculates the current
#' annual probability of failure per kilometer for a 30-60kV cables.
#' The function is a cubic curve that is based on
#' the first three terms of the Taylor series for an
#' exponential function.
#' @param cable_type String.
#' A sting that refers to the specific asset category.
#' Options:
#' \code{cable_type = c("30kV UG Cable (Gas)", "60kV UG Cable (Gas)",
#' "30kV UG Cable (Non Pressurised)", "60kV UG Cable (Non Pressurised)",
#' "30kV UG Cable (Oil)", "60kV UG Cable (Oil)")
#'}. The default setting is
#' \code{cable_type = "60kV UG Cable (Gas)"}.
#' @param sub_division String. Refers to material the sheath and conductor is
#' made of. Options:
#' \code{sub_division = c("Aluminium sheath - Aluminium conductor",
#' "Aluminium sheath - Copper conductor",
#' "Lead sheath - Aluminium conductor", "Lead sheath - Copper conductor")
#'}
#' @inheritParams duty_factor_cables
#' @param sheath_test String. Only applied for non pressurised cables.
#' Indicating the state of the sheath. Options:
#' \code{sheath_test = c("Pass", "Failed Minor", "Failed Major",
#' "Default")}.
#' @param partial_discharge String. Only applied for non pressurised cables.
#' Indicating the level of partial discharge. Options:
#' \code{partial_discharge = c("Low", "Medium", "High",
#'  "Default")}.
#' @param fault_hist Numeric. Only applied for non pressurised cables.
#' The calculated fault rate for the cable in the period per kilometer.
#' A setting of \code{"No historic faults recorded"}
#' indicates no fault.
#' @param leakage String. Only applied for oil and gas pressurised cables.
#' Options:
#' \code{leakage = c("No (or very low) historic leakage recorded",
#' "Low/ moderate", "High", "Very High", "Default")}.
#' @inheritParams current_health
#' @param age  Numeric. The current age in years of the cable.
#' @param k_value Numeric. \code{k_value = "Default"} by default. This number is
#' given in a percentage. The default value is accordingly to the CNAIM standard
#' on p. 110.
#' @param c_value Numeric. \code{c_value = 1.087} by default.
#' The default value is accordingly to the CNAIM standard see page 110
#' @param normal_expected_life Numeric. \code{normal_expected_life = "Default"} by default.
#' The default value is accordingly to the CNAIM standard on page 107.
#' @return Numeric. Current probability of failure
#' per annum per kilometre for 60/30kV cables.
#' @export
#' @examples
#' # Current annual probability of failure for
#' # "60kV UG Cable (Non Pressurised)", 50 years old
#'pof_cables_60kV_non <-
# pof_cables_60_30kv(cable_type = "60kV UG Cable (Non Pressurised)",
# sub_division = "Lead sheath - Copper conductor",
# utilisation_pct = 80,
# operating_voltage_pct = 60,
# sheath_test = "Default",
# partial_discharge = "Default",
# fault_hist = "Default",
# leakage = "Default",
# reliability_factor = "Default",
# age = 50,
# k_value = "Default",
# c_value = 1.087,
# normal_expected_life = "Default") * 100
#'
#'paste0(sprintf("Probability of failure %.4f", pof_cables_66kV_non),
#'" percent per annum")
pof_cables_60_30kv <-
  function(cable_type = "60kV UG Cable (Gas)",
           sub_division = "Aluminium sheath - Aluminium conductor",
           utilisation_pct = "Default",
           operating_voltage_pct = "Default",
           sheath_test = "Default",
           partial_discharge = "Default",
           fault_hist = "Default",
           leakage = "Default",
           reliability_factor = "Default",
           age,
           k_value = "Default",
           c_value = 1.087,
           normal_expected_life = "Default") {

    if (cable_type == "30kV UG Cable (Non Pressurised)" ) {
      cable_type <- "33kV UG Cable (Non Pressurised)"
    } else if ( cable_type == "30kV UG Cable (Oil)") {
      cable_type <- "33kV UG Cable (Oil)"
    } else if ( cable_type == "30kV UG Cable (Gas)") {
      cable_type <- "33kV UG Cable (Gas)"
    } else if ( cable_type == "60kV UG Cable (Non Pressurised)") {
      cable_type <- "66kV UG Cable (Non Pressurised)"
    } else if ( cable_type == "60kV UG Cable (Oil)") {
      cable_type <- "66kV UG Cable (Oil)"
    } else {
      cable_type <- "66kV UG Cable (Gas)"
    }



    `Asset Register Category` = `Health Index Asset Category` =
      `Generic Term...1` = `Generic Term...2` = `Functional Failure Category` =
      `K-Value (%)` = `C-Value` = `Asset Register  Category` =
      `Sub-division` = `Condition Criteria: Sheath Test Result` =
      `Condition Criteria: Partial Discharge Test Result` =
      `Condition Criteria: Leakage Rate` = NULL
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

    # Normal expected life  -------------------------


    if (normal_expected_life == "Default") {
      normal_expected_life_cable <- gb_ref$normal_expected_life %>%
        dplyr::filter(`Asset Register  Category` == cable_type &
                        `Sub-division` == sub_division) %>%
        dplyr::pull()
    } else {
      normal_expected_life_cable <- normal_expected_life
    }



    # Constants C and K for PoF function --------------------------------------
    if (asset_category == "EHV UG Cable (Non Pressurised)") {
      type_k_c <-
        gb_ref$pof_curve_parameters$`Functional Failure Category`[which(
          grepl("Non Pressurised",
                gb_ref$pof_curve_parameters$`Functional Failure Category`,
                fixed = TRUE) == TRUE
        )]
    } else {

      type_k_c <-
        gb_ref$pof_curve_parameters$`Functional Failure Category`[which(
          grepl(asset_category,
                gb_ref$pof_curve_parameters$`Functional Failure Category`,
                fixed = TRUE) == TRUE
        )]
    }

    if (k_value == "Default") {
      k <- gb_ref$pof_curve_parameters %>%
        dplyr::filter(`Functional Failure Category` ==
                        type_k_c) %>% dplyr::select(`K-Value (%)`) %>%
        dplyr::pull()/100
    } else {
      k <- k_value/100
    }

    c <- c_value

    # Duty factor -------------------------------------------------------------

    duty_factor_cable <-
      duty_factor_cables(utilisation_pct,
                         operating_voltage_pct,
                         voltage_level = "EHV")

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


    # Sheath test -------------------------------------------------------------
    if (asset_category == "EHV UG Cable (Non Pressurised)") {

      mci_ehv_cbl_non_pr_sheath_test <-
        gb_ref$mci_ehv_cbl_non_pr_sheath_test %>% dplyr::filter(
          `Condition Criteria: Sheath Test Result` == sheath_test
        )

      ci_factor_sheath <-
        mci_ehv_cbl_non_pr_sheath_test$`Condition Input Factor`
      ci_cap_sheath <-
        mci_ehv_cbl_non_pr_sheath_test$`Condition Input Cap`
      ci_collar_sheath <-
        mci_ehv_cbl_non_pr_sheath_test$`Condition Input Collar`


      mci_ehv_cbl_non_pr_prtl_disch <-
        gb_ref$mci_ehv_cbl_non_pr_prtl_disch %>%
        dplyr::filter(
          `Condition Criteria: Partial Discharge Test Result` == partial_discharge
        )

      # Partial discharge-------------------------------------------------------


      ci_factor_partial <-
        mci_ehv_cbl_non_pr_prtl_disch$`Condition Input Factor`
      ci_cap_partial <- mci_ehv_cbl_non_pr_prtl_disch$`Condition Input Cap`
      ci_collar_partial <-
        mci_ehv_cbl_non_pr_prtl_disch$`Condition Input Collar`

      mci_ehv_cbl_non_pr_fault_hist <-
        gb_ref$mci_ehv_cbl_non_pr_fault_hist


      # Fault -------------------------------------------------------

      for (n in 2:4) {
        if (fault_hist == 'Default' || fault_hist ==
            'No historic faults recorded') {
          no_row <- which(mci_ehv_cbl_non_pr_fault_hist$Upper == fault_hist)

          ci_factor_fault <-
            mci_ehv_cbl_non_pr_fault_hist$`Condition Input Factor`[no_row]
          ci_cap_fault <-
            mci_ehv_cbl_non_pr_fault_hist$`Condition Input Cap`[no_row]
          ci_collar_fault <-
            mci_ehv_cbl_non_pr_fault_hist$`Condition Input Collar`[no_row]
          break
        } else if (fault_hist >=
                   as.numeric(mci_ehv_cbl_non_pr_fault_hist$Lower[n]) &
                   fault_hist <
                   as.numeric(mci_ehv_cbl_non_pr_fault_hist$Upper[n])) {

          ci_factor_fault <-
            mci_ehv_cbl_non_pr_fault_hist$`Condition Input Factor`[n]
          ci_cap_fault <-
            mci_ehv_cbl_non_pr_fault_hist$`Condition Input Cap`[n]
          ci_collar_fault <-
            mci_ehv_cbl_non_pr_fault_hist$`Condition Input Collar`[n]

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

    } else if (asset_category == "EHV UG Cable (Oil)") {

      mci_ehv_cable_oil_leakage <-
        gb_ref$mci_ehv_cable_oil_leakage %>% dplyr::filter(
          `Condition Criteria: Leakage Rate` == leakage
        )

      ci_factor_leakage_oil <-
        mci_ehv_cable_oil_leakage$`Condition Input Factor`
      ci_cap_leakage_oil <-
        mci_ehv_cable_oil_leakage$`Condition Input Cap`
      ci_collar_leakage_oil <-
        mci_ehv_cable_oil_leakage$`Condition Input Collar`

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
                     reliability_factor = reliability_factor)

    # Probability of failure ---------------------------------------------------
    probability_of_failure <- k *
      (1 + (c * current_health_score) +
         (((c * current_health_score)^2) / factorial(2)) +
         (((c * current_health_score)^3) / factorial(3)))


    return(probability_of_failure)
  }
