#' @importFrom magrittr %>%
#' @title Current Probability of Failure for 20/10/0.4kV cables
#' @description This function calculates the current
#' annual probability of failure per kilometer for a 20/10/0.4kV cable.
#' The function is a cubic curve that is based on
#' the first three terms of the Taylor series for an
#' exponential function. For more information about the
#' probability of failure function see section 6
#' on page 34 in CNAIM (2021).
#' @param hv_lv_cable_type String.
#' A sting that refers to the specific asset category.
#' Options:
#' \code{hv_lv_cable_type = c("10-20kV cable, PEX","10-20kV cable, APB",
#' "0.4kV cable")}. The default setting is
#' \code{hv_lv_cable_type = "10-20kV cable, PEX"}.
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
#' "Default")}. See page 153, table 168 in CNAIM (2021).
#' @param partial_discharge String. Only applied for non pressurised cables.
#' Indicating the level of partial discharge. Options:
#' \code{partial_discharge = c("Low", "Medium", "High",
#'  "Default")}. See page 153, table 169 in CNAIM (2021).
#' @param fault_hist Numeric. Only applied for non pressurised cables.
#' The calculated fault rate for the cable in the period per kilometer.
#' A setting of \code{"No historic faults recorded"}
#' indicates no fault. See page 153, table 170 in CNAIM (2021).
#' @inheritParams current_health
#' @param age Numeric. The current age in years of the cable.
#' @param normal_expected_life_cable Numeric. The normal expected life for the
#' cable type.
#' @return Numeric. Current probability of failure
#' per annum for 20/10/0.4kV cables.
#' @source DNO Common Network Asset Indices Methodology (CNAIM),
#' Health & Criticality - Version 2.1, 2021:
#' \url{https://www.ofgem.gov.uk/sites/default/files/docs/2021/04/dno_common_network_asset_indices_methodology_v2.1_final_01-04-2021.pdf}
#' @export
#' @examples
#' # Current annual probability of failure for 10-20kV cable, APB, 50 years old
#'pof_cables_10kV_APB <-
#'pof_cables_20_10_04kv(hv_lv_cable_type = "10-20kV cable, APB",
#'sub_division = "Lead sheath - Copper conductor",
#'utilisation_pct = 80,
#'operating_voltage_pct = 60,
#'sheath_test = "Default",
#'partial_discharge = "Default",
#'fault_hist = "Default",
#'reliability_factor = "Default",
#'age = 50,
#'normal_expected_life_cable = 80) * 100
#'
#'paste0(sprintf("Probability of failure %.4f", pof_cables_10kV_APB),
#'" percent per annum")

pof_cables_20_10_04kv <-
  function(hv_lv_cable_type = "10-20kV cable, PEX",
           sub_division = "Aluminium sheath - Aluminium conductor",
           utilisation_pct = "Default",
           operating_voltage_pct = "Default",
           sheath_test = "Default",
           partial_discharge = "Default",
           fault_hist = "Default",
           reliability_factor = "Default",
           age,
           normal_expected_life_cable) {

    `Asset Register Category` = `Health Index Asset Category` =
      `Generic Term...1` = `Generic Term...2` = `Functional Failure Category` =
      `K-Value (%)` = `C-Value` = `Asset Register  Category` = `Sub-division` =
      `Condition Criteria: Sheath Test Result` =
      `Condition Criteria: Partial Discharge Test Result` =
      NULL
    # due to NSE notes in R CMD check

    if (hv_lv_cable_type ==  "10-20kV cable, PEX" ||
        hv_lv_cable_type ==  "10-20kV cable, APB" ||
        hv_lv_cable_type ==  "0.4kV cable") {
      pseudo_cable_type <- "33kV UG Cable (Non Pressurised)"
    }

    if(hv_lv_cable_type ==  "10-20kV cable, APB") {
      if(sub_division == "Aluminium sheath - Aluminium conductor" ||
         sub_division == "Lead sheath - Copper conductor") {

        warning('hv_lv_cable_type = "10-20kV cable, APB",
               cable type consists of a lead sheath.
               Please review sub_division. Selected for type for
                sub_division = "Lead sheath - Copper conductor".')
      }
    }


    # Ref. table Categorisation of Assets and Generic Terms for Assets  --
    asset_category <- gb_ref$categorisation_of_assets %>%
      dplyr::filter(`Asset Register Category` == pseudo_cable_type) %>%
      dplyr::select(`Health Index Asset Category`) %>% dplyr::pull()

    generic_term_1 <- gb_ref$generic_terms_for_assets %>%
      dplyr::filter(`Health Index Asset Category` == asset_category) %>%
      dplyr::select(`Generic Term...1`) %>% dplyr::pull()

    generic_term_2 <- gb_ref$generic_terms_for_assets %>%
      dplyr::filter(`Health Index Asset Category` == asset_category) %>%
      dplyr::select(`Generic Term...2`) %>% dplyr::pull()

    # Normal expected life  ---------------------------
    # normal_expected_life_cable <- gb_ref$normal_expected_life %>%
    #   dplyr::filter(`Asset Register  Category` == pseudo_cable_type &
    #                   `Sub-division` == sub_division) %>%
    #   dplyr::pull()

    #################################################################


    # Constants C and K for PoF function --------------------------------------
    type_k_c <- gb_ref$pof_curve_parameters$`Functional Failure Category`[which(
      grepl("Non Pressurised",
            gb_ref$pof_curve_parameters$`Functional Failure Category`,
            fixed = TRUE) == TRUE
    )]

    k <- gb_ref$pof_curve_parameters %>%
      dplyr::filter(`Functional Failure Category` ==
                      type_k_c) %>% dplyr::select(`K-Value (%)`) %>%
      dplyr::pull() / 100

    k <- k*80/60

    c <- gb_ref$pof_curve_parameters %>%
      dplyr::filter(`Functional Failure Category` ==
                      type_k_c) %>% dplyr::select(`C-Value`) %>%
      dplyr::pull()

    # Duty factor -------------------------------------------------------------

    duty_factor_cable <-
      duty_factor_cables(utilisation_pct,
                         operating_voltage_pct,
                         voltage_level = "LV & HV")

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
    # These overriding values are shown in Table 35 to Table 202
    # and Table 207 in Appendix B.

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
    mci_ehv_cbl_non_pr_sheath_test <-
      gb_ref$mci_ehv_cbl_non_pr_sheath_test %>% dplyr::filter(
        `Condition Criteria: Sheath Test Result` == sheath_test
      )

    ci_factor_sheath <- mci_ehv_cbl_non_pr_sheath_test$`Condition Input Factor`
    ci_cap_sheath <- mci_ehv_cbl_non_pr_sheath_test$`Condition Input Cap`
    ci_collar_sheath <- mci_ehv_cbl_non_pr_sheath_test$`Condition Input Collar`

    # Partial discharge-------------------------------------------------------

    mci_ehv_cbl_non_pr_prtl_disch <-
      gb_ref$mci_ehv_cbl_non_pr_prtl_disch %>% dplyr::filter(
        `Condition Criteria: Partial Discharge Test Result` == partial_discharge
      )


    ci_factor_partial <- mci_ehv_cbl_non_pr_prtl_disch$`Condition Input Factor`
    ci_cap_partial <- mci_ehv_cbl_non_pr_prtl_disch$`Condition Input Cap`
    ci_collar_partial <- mci_ehv_cbl_non_pr_prtl_disch$`Condition Input Collar`


    # Fault -------------------------------------------------------

    mci_ehv_cbl_non_pr_fault_hist <-
      gb_ref$mci_ehv_cbl_non_pr_fault_hist

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

    # Measured condition collar -----------------------------------------------
    collars <- c(ci_collar_sheath,
                 ci_collar_partial,
                 ci_collar_fault)
    measured_condition_collar <- max(collars)


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
      current_health(
        initial_health_score = initial_health_score,
        health_score_factor=  health_score_modifier$health_score_factor,
        health_score_cap = health_score_modifier$health_score_cap,
        health_score_collar = health_score_modifier$health_score_collar,
        reliability_factor = reliability_factor)

    # Probability of failure ---------------------------------------------------
    probability_of_failure <- k *
      (1 + (c * current_health_score) +
         (((c * current_health_score)^2) / factorial(2)) +
         (((c * current_health_score)^3) / factorial(3)))

    return(probability_of_failure)
  }

