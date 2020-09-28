#' @importFrom magrittr %>%
#' @title Current Probability of Failure for 33-132kV OHL Conductors
#' @description This function calculates the current
#' annual probability of failure per kilometre 3-132kV OHL conductors.
#' The function is a cubic curve that is based on
#' the first three terms of the Taylor series for an
#' exponential function. For more information about the
#' probability of failure function see section 6
#' on page 30 in CNAIM (2017).
#' @param ohl_conductor String.
#' A sting that refers to the specific asset category.
#' See See page 15, table 1 in CNAIM (2017).
#' Options:
#' \code{ohl_conductor = c("33kV OHL (Tower Line) Conductor",
#' "66kV OHL (Tower Line) Conductor", "132kV OHL (Tower Line) Conductor")}.
#' The default setting is
#' \code{ohl_conductor = "66kV OHL (Tower Line) Conductor"}.
#' @param sub_division String. Refers to material the conductor is
#' made of. Options:
#' \code{sub_division = c("ACSR - greased",
#' "ACSR - non-greased",
#' "AAAC",
#' "Cad Cu",
#' "Cu",
#' "Other")
#'}
#' @inheritParams location_factor
#' @param age  Numeric. The current age in years of the conductor.
#' @param conductor_samp String. Conductor sampling. Options:
#' \code{conductor_samp = c("Low","Medium/Normal","High","Default")}
#' @param corr_mon_survey String. Corrosion monitoring survey. Options:
#' \code{corr_mon_survey = c("Low","Medium/Normal","High","Default")}
#' @param visual_cond String. Visual condition. Options:
#' \code{visual_cond = c("As New","Normal Wear","Some Deterioration",
#' "Substantial Deterioration", "Default")}
#' @param midspan_joints Integer. Number of midspan joints on the conductor.
#' A span includes all conductors in that span.
#' @inheritParams current_health
#' @return Numeric. Current probability of failure
#' per annum per kilometre for 20/10/0.4kV cables.
#' @source DNO Common Network Asset Indices Methodology (CNAIM),
#' Health & Criticality - Version 1.1, 2017:
#' \url{https://www.ofgem.gov.uk/system/files/docs/2017/05/dno_common_network_asset_indices_methodology_v1.1.pdf}
#' @export
#' @examples
#' # Current annual probability of failure for 66kV OHL (Tower Line) Conductor
#'pof_ohl_cond_132_66_33kv(
#'ohl_conductor = "66kV OHL (Tower Line) Conductor",
#'sub_division = "Cu",
#'placement = "Default",
#'altitude_m = "Default",
#'distance_from_coast_km = "Default",
#'corrosion_category_index = "Default",
#'age = 10,
#'conductor_samp = "Default",
#'corr_mon_survey = "Default",
#'visual_cond = "Default",
#'midspan_joints = "Default",
#'reliability_factor = "Default")

pof_ohl_cond_132_66_33kv <-
  function(ohl_conductor = "66kV OHL (Tower Line) Conductor",
           sub_division = "Cu",
           placement = "Default",
           altitude_m = "Default",
           distance_from_coast_km = "Default",
           corrosion_category_index = "Default",
           age,
           conductor_samp = "Default",
           corr_mon_survey = "Default",
           visual_cond = "Default",
           midspan_joints = "Default",
           reliability_factor = "Default") {


    `Asset Register Category` = `Health Index Asset Category` =
      `Generic Term...1` = `Generic Term...2` = `Functional Failure Category` =
      `K-Value (%)` = `C-Value` = `Asset Register  Category` = NULL
    # due to NSE notes in R CMD check

    # Ref. table Categorisation of Assets and Generic Terms for Assets  --

    asset_category <- gb_ref$categorisation_of_assets %>%
      dplyr::filter(`Asset Register Category` == ohl_conductor) %>%
      dplyr::select(`Health Index Asset Category`) %>% dplyr::pull()

    generic_term_1 <- gb_ref$generic_terms_for_assets %>%
      dplyr::filter(`Health Index Asset Category` == asset_category) %>%
      dplyr::select(`Generic Term...1`) %>% dplyr::pull()

    generic_term_2 <- gb_ref$generic_terms_for_assets %>%
      dplyr::filter(`Health Index Asset Category` == asset_category) %>%
      dplyr::select(`Generic Term...2`) %>% dplyr::pull()

    # Normal expected life  -------------------------
    normal_expected_life_cond <- gb_ref$normal_expected_life %>%
      dplyr::filter(`Asset Register  Category` == ohl_conductor &
                      `Sub-division` == sub_division) %>%
      dplyr::pull()

    # Constants C and K for PoF function --------------------------------------

    k <- gb_ref$pof_curve_parameters %>%
      dplyr::filter(`Functional Failure Category` ==
                      generic_term_2) %>% dplyr::select(`K-Value (%)`) %>%
      dplyr::pull()/100

    c <- gb_ref$pof_curve_parameters %>%
      dplyr::filter(`Functional Failure Category` ==
                      generic_term_2) %>% dplyr::select(`C-Value`) %>%
      dplyr::pull()

    # Duty factor -------------------------------------------------------------

    duty_factor_cond <- 1

    # Location factor ----------------------------------------------------
    location_factor_cond <- location_factor(placement,
                                            altitude_m,
                                            distance_from_coast_km,
                                            corrosion_category_index,
                                            asset_type = ohl_conductor)
    # Expected life ------------------------------
    expected_life_years <- expected_life(normal_expected_life_cond,
                                         duty_factor_cond,
                                         location_factor_cond)

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
    if (asset_category == "EHV OHL Conductor (Tower Lines)") {
      asset_category_mmi <- "EHV Tower Line Conductor"
    }
    if (asset_category == "132kV OHL Conductor (Tower Lines)") {
      asset_category_mmi <- "132kV Tower Line Conductor"
    }

    mcm_mmi_cal_df <-
      gb_ref$measured_cond_modifier_mmi_cal

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
    # Conductor sampling

    if (asset_category == "132kV OHL Conductor (Tower Lines)") {

      mci_132kv_twr_line_cond_sampl <-
        gb_ref$mci_132kv_twr_line_cond_sampl %>% dplyr::filter(
          `Condition Criteria: Conductor Sampling Result` == conductor_samp
        )

      ci_factor_cond_samp <-
        mci_132kv_twr_line_cond_sampl$`Condition Input Factor`
      ci_cap_cond_samp <-
        mci_132kv_twr_line_cond_sampl$`Condition Input Cap`
      ci_collar_cond_samp <-
        mci_132kv_twr_line_cond_sampl$`Condition Input Collar`


      # Corrosion monitoring survey
      mci_132kv_twr_line_cond_srvy <-
        gb_ref$mci_132kv_twr_line_cond_srvy %>% dplyr::filter(
          `Condition Criteria: Corrosion Monitoring Survey Result` ==
            corr_mon_survey
        )


      ci_factor_cond_srvy <-
        mci_132kv_twr_line_cond_srvy$`Condition Input Factor`
      ci_cap_cond_srvy <- mci_132kv_twr_line_cond_srvy$`Condition Input Cap`
      ci_collar_cond_srvy <-
        mci_132kv_twr_line_cond_srvy$`Condition Input Collar`

    } else {

      mci_ehv_twr_line_cond_sampl <-
        gb_ref$mci_ehv_twr_line_cond_sampl %>% dplyr::filter(
          `Condition Criteria: Conductor Sampling Result` == conductor_samp
        )

      ci_factor_cond_samp <-
        mci_ehv_twr_line_cond_sampl$`Condition Input Factor`
      ci_cap_cond_samp <-
        mci_ehv_twr_line_cond_sampl$`Condition Input Cap`
      ci_collar_cond_samp <-
        mci_ehv_twr_line_cond_sampl$`Condition Input Collar`


      # Corrosion monitoring survey
      mci_ehv_twr_line_cond_srvy <-
        gb_ref$mci_ehv_twr_line_cond_srvy %>% dplyr::filter(
          `Condition Criteria: Corrosion Monitoring Survey Result` ==
            corr_mon_survey
        )


      ci_factor_cond_srvy <-
        mci_ehv_twr_line_cond_srvy$`Condition Input Factor`
      ci_cap_cond_srvy <- mci_ehv_twr_line_cond_srvy$`Condition Input Cap`
      ci_collar_cond_srvy <-
        mci_ehv_twr_line_cond_srvy$`Condition Input Collar`

    }

    # Measured conditions

    factors <- c(ci_factor_cond_samp,
                 ci_factor_cond_srvy)

    measured_condition_factor <- mmi(factors,
                                     factor_divider_1,
                                     factor_divider_2,
                                     max_no_combined_factors)

    caps <- c(ci_cap_cond_samp,
              ci_cap_cond_srvy)

    measured_condition_cap <- min(caps)

    # Measured condition collar ----------------------------------------------
    collars <- c(ci_collar_cond_samp,
                 ci_collar_cond_srvy)

    measured_condition_collar <- max(collars)


    # Measured condition modifier ---------------------------------------------
    measured_condition_modifier <- data.frame(measured_condition_factor,
                                              measured_condition_cap,
                                              measured_condition_collar)



    # Observed conditions -----------------------------------------------------

    if (asset_category == "EHV OHL Conductor (Tower Lines)") {
      asset_category_mmi <- "EHV Tower Line Conductor"
    }
    if (asset_category == "132kV OHL Conductor (Tower Lines)") {
      asset_category_mmi <- "132kV Tower Line Conductor"
    }

    oci_mmi_cal_df <-
      gb_ref$observed_cond_modifier_mmi_cal

    oci_mmi_cal_df <-
      oci_mmi_cal_df[which(
        oci_mmi_cal_df$`Asset Category` == asset_category_mmi), ]

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
    # Visual condition

    if (asset_category == "132kV OHL Conductor (Tower Lines)") {

      oci_132kv_twr_line_visual_cond <-
        gb_ref$oci_132kv_twr_line_visual_cond %>% dplyr::filter(
          `Condition Criteria: Observed Condition` == visual_cond
        )

      ci_factor_visual_cond <-
        oci_132kv_twr_line_visual_cond$`Condition Input Factor`
      ci_cap_visual_cond <-
        oci_132kv_twr_line_visual_cond$`Condition Input Cap`
      ci_collar_visual_cond <-
        oci_132kv_twr_line_visual_cond$`Condition Input Collar`


      # Midspan joints

      if (is.numeric(midspan_joints)) {
        if(midspan_joints < 3) {
          midspan_joints <- as.character(midspan_joints)
        } else if (midspan_joints > 2){
          midspan_joints <- ">2"
        }

      }

      oci_132kv_twr_line_cond_midspn <-
        gb_ref$oci_132kv_twr_line_cond_midspn %>% dplyr::filter(
          `Condition Criteria: No. of Midspan Joints` == midspan_joints
        )

      ci_factor_midspan_joints <-
        oci_132kv_twr_line_cond_midspn$`Condition Input Factor`
      ci_cap_midspan_joints <- oci_132kv_twr_line_cond_midspn$`Condition Input Cap`
      ci_collar_midspan_joints <-
        oci_132kv_twr_line_cond_midspn$`Condition Input Collar`

    } else {

      oci_ehv_twr_line_visal_cond <-
        gb_ref$oci_ehv_twr_line_visal_cond %>% dplyr::filter(
          `Condition Criteria: Observed Condition` == visual_cond
        )

      ci_factor_visual_cond <-
        oci_ehv_twr_line_visal_cond$`Condition Input Factor`
      ci_cap_visual_cond <-
        oci_ehv_twr_line_visal_cond$`Condition Input Cap`
      ci_collar_visual_cond <-
        oci_ehv_twr_line_visal_cond$`Condition Input Collar`


      # Midspan joints

      if (is.numeric(midspan_joints)) {
        if(midspan_joints < 3) {
          midspan_joints <- as.character(midspan_joints)
        } else if (midspan_joints > 2){
          midspan_joints <- ">2"
        }

      }

      oci_ehv_twr_cond_midspan_joint <-
        gb_ref$oci_ehv_twr_cond_midspan_joint %>% dplyr::filter(
          `Condition Criteria: No. of Midspan Joints` == midspan_joints
        )

      ci_factor_midspan_joints <-
        oci_ehv_twr_cond_midspan_joint$`Condition Input Factor`
      ci_cap_midspan_joints <- oci_ehv_twr_cond_midspan_joint$`Condition Input Cap`
      ci_collar_midspan_joints <-
        oci_ehv_twr_cond_midspan_joint$`Condition Input Collar`
    }


    # Observed conditions

    factors <- c(ci_factor_visual_cond,
                 ci_factor_midspan_joints)

    observed_condition_factor <- mmi(factors,
                                     factor_divider_1,
                                     factor_divider_2,
                                     max_no_combined_factors)

    caps <- c(ci_cap_visual_cond,
              ci_cap_midspan_joints)

    observed_condition_cap <- min(caps)

    # Observed condition collar ----------------------------------------------
    collars <- c(ci_collar_visual_cond,
                 ci_collar_midspan_joints)

    observed_condition_collar <- max(collars)


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

    # Probability of failure for the 6.6/11 kV transformer today ---------------
    probability_of_failure <- k *
      (1 + (c * current_health_score) +
         (((c * current_health_score)^2) / factorial(2)) +
         (((c * current_health_score)^3) / factorial(3)))


    return(probability_of_failure)
  }
