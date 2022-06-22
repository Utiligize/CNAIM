#' @importFrom magrittr %>%
#' @title Future Probability of Failure for 50kV OHL Conductors
#' @description This function calculates the future
#' annual probability of failure per kilometer 50kV OHL conductors.
#' The function is a cubic curve that is based on
#' the first three terms of the Taylor series for an
#' exponential function.
#' @inheritParams pof_ohl_cond_50kv
#' @param simulation_end_year Numeric. The last year of simulating probability
#'  of failure. Default is 100.
#' @return Numeric. Future probability of failure
#' per annum per kilometer.
#' @export
#' @examples
#' # Future annual probability of failure for 50kV OHL (Tower Line) Conductor
# pof_future_ohl_cond_50kv(
# sub_division = "Cu",
# placement = "Default",
# altitude_m = "Default",
# distance_from_coast_km = "Default",
# corrosion_category_index = "Default",
# age = 10,
# conductor_samp = "Default",
# corr_mon_survey = "Default",
# visual_cond = "Default",
# midspan_joints = "Default",
# reliability_factor = "Default",
# k_value = 0.0080,
# c_value = 1.087,
# normal_expected_life = "Default",
# simulation_end_year = 100)

pof_future_ohl_cond_50kv <-
  function(sub_division = "Cu",
           placement = "Default",
           altitude_m = "Default",
           distance_from_coast_km = "Default",
           corrosion_category_index = "Default",
           age,
           conductor_samp = "Default",
           corr_mon_survey = "Default",
           visual_cond = "Default",
           midspan_joints = "Default",
           reliability_factor = "Default",
           k_value = 0.0080,
           c_value = 1.087,
           normal_expected_life = "Default",
           simulation_end_year = 100) {

    ohl_conductor <- "66kV OHL (Tower Line) Conductor"

    `Asset Register Category` = `Health Index Asset Category` =
      `Generic Term...1` = `Generic Term...2` = `Functional Failure Category` =
      `K-Value (%)` = `C-Value` = `Asset Register  Category` = `Sub-division` =
      `Condition Criteria: Conductor Sampling Result` =
      `Condition Criteria: Corrosion Monitoring Survey Result` =
      `Condition Criteria: Observed Condition` =
      `Condition Criteria: No. of Midspan Joints` = NULL
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

    if (normal_expected_life == "Default") {
      normal_expected_life_cond <- gb_ref$normal_expected_life %>%
        dplyr::filter(`Asset Register  Category` == ohl_conductor &
                        `Sub-division` == sub_division) %>%
        dplyr::pull()
    } else {
      normal_expected_life_cond <- normal_expected_life
    }

    # Constants C and K for PoF function --------------------------------------

    k <- k_value/100
    c <- c_value

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
    print(b2)
    if (b2 > 2*b1){
      b2 <- b1*2
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
      } else if (H < 4) {
        H <- 4
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

