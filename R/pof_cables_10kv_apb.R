#' @importFrom magrittr %>%
#' @title Current Probability of Failure for 10kV APB cables
#' @description This function calculates the current
#' annual probability of failure per kilometer for a 10 kV APB cable.
#' The function is a cubic curve that is based on
#' the first three terms of the Taylor series for an
#' exponential function. For more information about the
#' probability of failure function see section 6
#' on page 34 in CNAIM (2021).
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
#' @return Numeric. Current probability of failure
#' per annum for 10 kV apb cables.
#' @source DNO Common Network Asset Indices Methodology (CNAIM),
#' Health & Criticality - Version 2.1, 2021:
#' \url{https://www.ofgem.gov.uk/sites/default/files/docs/2021/04/dno_common_network_asset_indices_methodology_v2.1_final_01-04-2021.pdf}
#' @export
#' @examples
#' # Current annual probability of failure for 10-20kV cable, APB, 50 years old
#'pof_cables_10kv_apb_result <-
#'pof_cables_10kv_apb(
#'utilisation_pct = 80,
#'operating_voltage_pct = 60,
#'sheath_test = "Default",
#'partial_discharge = "Default",
#'fault_hist = "Default",
#'reliability_factor = "Default",
#'age = 50) * 100
#'
#'paste0(sprintf("Probability of failure %.4f", pof_cables_10kv_apb_result),
#'" percent per annum")

pof_cables_10kv_apb <-
  function(utilisation_pct = "Default",
           operating_voltage_pct = "Default",
           sheath_test = "Default",
           partial_discharge = "Default",
           fault_hist = "Default",
           reliability_factor = "Default",
           age) {

    `Asset Register Category` = `Health Index Asset Category` =
      `Generic Term...1` = `Generic Term...2` = `Functional Failure Category` =
      `K-Value (%)` = `C-Value` = `Asset Register  Category` = `Sub-division` =
      `Condition Criteria: Sheath Test Result` =
      `Condition Criteria: Partial Discharge Test Result` =
      NULL

      pseudo_cable_type <- "33kV UG Cable (Non Pressurised)"
      sub_division <- "Lead sheath - Copper conductor"


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


    # Constants C and K for PoF function --------------------------------------

    k <- 0.238/100 # see p. 34  "DE-10kV apb kabler CNAIM"
    c <- 1.087 # set to the standard accordingly in CNAIM (2021) and in "DE-10kV apb kabler CNAIM"

    duty_factor_cable <-
      duty_factor_cables(utilisation_pct,
                         operating_voltage_pct,
                         voltage_level = "LV & HV")

    # Expected life ------------------------------ # the expected life set to 80 accordingly to p. 33 in "DE-10kV apb kabler CNAIM"
    expected_life_years <- expected_life(80,
                                         duty_factor_cable,
                                         location_factor = 1)

    # b1 (Initial Ageing Rate) ------------------------------------------------
    b1 <- beta_1(expected_life_years)

    # Initial health score ----------------------------------------------------
    initial_health_score <- initial_health(b1, age)

    ## NOTE
    # Typically, the Health Score Collar is 0.5 and
    # Health Score Cap is 5.5 (p. 33 in DE-10kV apb kabler CNAIM), implying no overriding
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



