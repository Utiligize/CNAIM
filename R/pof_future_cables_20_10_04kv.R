#' @importFrom magrittr %>%
#' @title Future Probability of Failure for 20/10/0.4kV cables
#' @description This function calculates the future
#' annual probability of failure per kilometer for a 20/10/0.4kV cable.
#' The function is a cubic curve that is based on
#' the first three terms of the Taylor series for an
#' exponential function. For more information about the
#' probability of failure function see section 6
#' on page 30 in CNAIM (2017).
#' @inheritParams pof_cables_20_10_04kv
#' @param simulation_end_year Numeric. The last year of simulating probability
#'  of failure. Default is 100.
#' @return Numeric array. Future probability of failure
#' per annum for 33-66kV cables.
#' @source DNO Common Network Asset Indices Methodology (CNAIM),
#' Health & Criticality - Version 1.1, 2017:
#' \url{https://www.ofgem.gov.uk/system/files/docs/2017/05/dno_common_network_asset_indices_methodology_v1.1.pdf}
#' @export
#' @examples
#' # Future probability of failure for 66kV UG Cable (Non Pressurised)
#' pof_10kV_pex <-
#' pof_future_cables_20_10_04kv(hv_lv_cable_type = "10-20kV cable, PEX",
#'sub_division = "Aluminium sheath - Aluminium conductor",
#'utilisation_pct = "Default",
#'operating_voltage_pct = "Default",
#'sheath_test = "Default",
#'partial_discharge = "Default",
#'fault_hist = "Default",
#'reliability_factor = "Default",
#'age = 15,
#'simulation_end_year = 100)
#' # Plot
#'plot(pof_10kV_pex$PoF * 100,
#'type = "line", ylab = "%", xlab = "years",
#'main = "PoF per kilometre - 10-20kV cable, PEX")

pof_future_cables_20_10_04kv <-
  function(hv_lv_cable_type = "10-20kV cable, PEX",
           sub_division = "Aluminium sheath - Aluminium conductor",
           utilisation_pct = "Default",
           operating_voltage_pct = "Default",
           sheath_test = "Default",
           partial_discharge = "Default",
           fault_hist = "Default",
           reliability_factor = "Default",
           age,
           simulation_end_year = 100) {

    `Asset Register Category` = `Health Index Asset Category` =
      `Generic Term...1` = `Generic Term...2` = `Functional Failure Category` =
      `K-Value (%)` = `C-Value` = `Asset Register  Category` =
      `Sub-division` = `Condition Criteria: Sheath Test Result` =
      `Condition Criteria: Partial Discharge Test Result` = NULL
    # due to NSE notes in R CMD check

    if (hv_lv_cable_type ==  "10-20kV cable, PEX" ||
        hv_lv_cable_type ==  "10-20kV cable, APB" ||
        hv_lv_cable_type ==  "0.4kV cable") {
      pseudo_cable_type <- "33kV UG Cable (Non Pressurised)"
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
    normal_expected_life_cable <- gb_ref$normal_expected_life %>%
      dplyr::filter(`Asset Register  Category` == pseudo_cable_type &
                      `Sub-division` == sub_division) %>%
      dplyr::pull()

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
      } else if (fault_hist >= as.numeric(
        mci_ehv_cbl_non_pr_fault_hist$Lower[n]) &
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

