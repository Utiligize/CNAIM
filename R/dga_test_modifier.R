#' @importFrom magrittr %>%
#' @title DGA Test Modifier
#' @description This function calculates the DGA test modifier
#' for 33/10kV, 66/10kV and 132kV transformers. See e.g. section 6.12 on page
#' 65 in CNAIM (2017).
#' @param hydrogen Numeric. Refers to the hydrogen level in the
#' transformer oil. Hydrogen levels are measured in ppm.
#' A setting of \code{"Default"} will result in the best possible result.
#' @param methane Numeric. Refers to the methane level in the
#' transformer oil. Methane levels are measured in ppm.
#' A setting of \code{"Default"} will result in the best possible result.
#' @param ethylene Numeric. Refers to the ethylene level in the
#' transformer oil. Ethylene levels are measured in ppm.
#' A setting of \code{"Default"} will result in the best possible result.
#' @param ethane Numeric. Refers to the ethane level in the
#' transformer oil. Ethane levels are measured in ppm.
#' A setting of \code{"Default"} will result in the best possible result.
#' @param acetylene Numeric. Refers to the acetylene level in the
#' transformer oil. Acetylene levels are measured in ppm.
#' A setting of \code{"Default"} will result in the best possible result.
#' @param hydrogen_pre Numeric. Previous results.
#' A setting of \code{"Default"} will result in the best possible result.
#' @param methane_pre Numeric. Previous results.
#' A setting of \code{"Default"} will result in the best possible result.
#' @param ethylene_pre Numeric. Previous results.
#' A setting of \code{"Default"} will result in the best possible result.
#' @param ethane_pre Numeric. Previous results.
#' A setting of \code{"Default"} will result in the best possible result.
#' @param acetylene_pre Numeric. Previous results.
#' A setting of \code{"Default"} will result in the best possible result.
#' @return Data table.
#' @source DNO Common Network Asset Indices Methodology (CNAIM),
#' Health & Criticality - Version 2.1, 2021:
#' \url{https://www.ofgem.gov.uk/sites/default/files/docs/2021/04/dno_common_network_asset_indices_methodology_v2.1_final_01-04-2021.pdf}
#' @export
#' @examples
#' # DGA test modifier
#' dga_test_modifier(hydrogen = "Default",
#'methane = "Default",
#'ethylene = "Default",
#'ethane = "Default",
#'acetylene = "Default",
#'hydrogen_pre = "Default",
#'methane_pre = "Default",
#'ethylene_pre = "Default",
#'ethane_pre = "Default",
#'acetylene_pre = "Default")



dga_test_modifier <- function(hydrogen = "Default",
                              methane = "Default",
                              ethylene = "Default",
                              ethane = "Default",
                              acetylene = "Default",
                              hydrogen_pre = "Default",
                              methane_pre = "Default",
                              ethylene_pre = "Default",
                              ethane_pre = "Default",
                              acetylene_pre = "Default") {

  # Previous results -----------------------------------------------------------
  if(hydrogen_pre == "Default") hydrogen_pre <- -0.1
  if(methane_pre == "Default") methane_pre <- -0.1
  if(ethylene_pre == "Default") ethylene_pre <- -0.1
  if(ethane_pre == "Default") ethane_pre <- -0.1
  if(acetylene_pre == "Default") acetylene_pre <- -0.1

  # Hydrogen
  hydrogen_cond_state_calib <-
    gb_ref$hydrogen_cond_state_calib
  hydrogen_cond_state_calib$Lower[1] <- -Inf
  hydrogen_cond_state_calib$Upper[5] <- Inf

  for (n in 1:nrow(hydrogen_cond_state_calib)){

    if (hydrogen_pre > as.numeric(hydrogen_cond_state_calib$Lower[n]) &
        hydrogen_pre <= as.numeric(hydrogen_cond_state_calib$Upper[n])) {
      hydrogen_score_pre <- hydrogen_cond_state_calib$`Hydrogen Condition State`[n]
      break

    }
  }

  # Methane

  methane_cond_state_calib <-
    gb_ref$methane_cond_state_calib
  methane_cond_state_calib$Lower[1] <- -Inf
  methane_cond_state_calib$Upper[5] <- Inf

  for (n in 1:nrow(methane_cond_state_calib)){

    if (methane_pre > as.numeric(methane_cond_state_calib$Lower[n]) &
        methane_pre <= as.numeric(methane_cond_state_calib$Upper[n])) {
      methane_score_pre <- methane_cond_state_calib$`Methane Condition State`[n]
      break

    }
  }

  # Ethylene

  ethylene_cond_state_calib <-
    gb_ref$ethylene_cond_state_calib
  ethylene_cond_state_calib$Lower[1] <- -Inf
  ethylene_cond_state_calib$Upper[5] <- Inf

  for (n in 1:nrow(ethylene_cond_state_calib)){

    if (ethylene_pre > as.numeric(ethylene_cond_state_calib$Lower[n]) &
        ethylene_pre <= as.numeric(ethylene_cond_state_calib$Upper[n])) {
      ethylene_score_pre <- ethylene_cond_state_calib$`Ethylene Condition State`[n]
      break

    }
  }


  # Ethane
  ethane_cond_state_calib <-
    gb_ref$ethane_cond_state_calib
  ethane_cond_state_calib$Lower[1] <- -Inf
  ethane_cond_state_calib$Upper[5] <- Inf

  for (n in 1:nrow(ethane_cond_state_calib)){

    if (ethane_pre > as.numeric(ethane_cond_state_calib$Lower[n]) &
        ethane_pre <= as.numeric(ethane_cond_state_calib$Upper[n])) {
      ethane_score_pre <- ethane_cond_state_calib$`Ethane Condition State`[n]
      break

    }
  }


  # Acetyle
  acetylene_cond_state_calib <-
    gb_ref$acetylene_cond_state_calib
  acetylene_cond_state_calib$Lower[1] <- -Inf
  acetylene_cond_state_calib$Upper[5] <- Inf

  for (n in 1:nrow(acetylene_cond_state_calib)){

    if (acetylene_pre > as.numeric(acetylene_cond_state_calib$Lower[n]) &
        acetylene_pre <= as.numeric(acetylene_cond_state_calib$Upper[n])) {
      acetylene_score_pre <- acetylene_cond_state_calib$`Acetylene Condition State`[n]
      break

    }
  }


  # Latest results ------------------------------------------------------------

  if(hydrogen == "Default") hydrogen <- -0.1
  if(methane == "Default") methane <- -0.1
  if(ethylene == "Default") ethylene <- -0.1
  if(ethane == "Default") ethane <- -0.1
  if(acetylene == "Default") acetylene <- -0.1

  # Hydrogen

  for (n in 1:nrow(hydrogen_cond_state_calib)){

    if (hydrogen > as.numeric(hydrogen_cond_state_calib$Lower[n]) &
        hydrogen <= as.numeric(hydrogen_cond_state_calib$Upper[n])) {
      hydrogen_score <- hydrogen_cond_state_calib$`Hydrogen Condition State`[n]
      break

    }
  }

  # Methane

  for (n in 1:nrow(methane_cond_state_calib)){

    if (methane > as.numeric(methane_cond_state_calib$Lower[n]) &
        methane <= as.numeric(methane_cond_state_calib$Upper[n])) {
      methane_score <- methane_cond_state_calib$`Methane Condition State`[n]
      break

    }
  }



  # Ethylene

  for (n in 1:nrow(ethylene_cond_state_calib)){

    if (ethylene > as.numeric(ethylene_cond_state_calib$Lower[n]) &
        ethylene <= as.numeric(ethylene_cond_state_calib$Upper[n])) {
      ethylene_score <- ethylene_cond_state_calib$`Ethylene Condition State`[n]
      break

    }
  }


  # Ethane

  for (n in 1:nrow(ethane_cond_state_calib)){

    if (ethane > as.numeric(ethane_cond_state_calib$Lower[n]) &
        ethane <= as.numeric(ethane_cond_state_calib$Upper[n])) {
      ethane_score <- ethane_cond_state_calib$`Ethane Condition State`[n]
      break

    }
  }


  # Acetyle

  for (n in 1:nrow(acetylene_cond_state_calib)){

    if (acetylene > as.numeric(acetylene_cond_state_calib$Lower[n]) &
        acetylene <= as.numeric(acetylene_cond_state_calib$Upper[n])) {
      acetylene_score <- acetylene_cond_state_calib$`Acetylene Condition State`[n]
      break

    }
  }


  # DGA tests score ----------------------------------------------------------

  dga_test_score_pre <-
    50 * hydrogen_score_pre +
    30 * methane_score_pre +
    30 * ethylene_score_pre +
    30 * ethane_score_pre +
    120 * acetylene_score_pre

  dga_test_score <-
    50 * hydrogen_score +
    30 * methane_score +
    30 * ethylene_score +
    30 * ethane_score +
    120 * acetylene_score


  dga_test_collar <- ifelse(dga_test_score / 220 < 1, 1, dga_test_score / 220 )
  dga_test_collar <- ifelse(dga_test_collar > 7, 7, dga_test_collar)

  dga_test_cap <- 10

  dga_test_factor_calib <-
    gb_ref$dga_test_factor_calib

  names(dga_test_factor_calib)[1] <- "Change Category"

  dga_change_category_calib <-
    gb_ref$dga_change_category_calib

  dga_table_calib <- dplyr::left_join(dga_change_category_calib,
                                      dga_test_factor_calib, by = "Change Category")

  dga_table_calib$Lower[1] <- -Inf
  dga_table_calib$Upper[5] <- Inf

  dga_test_changes <-
    ifelse(is.na( ((dga_test_score / dga_test_score_pre) -1) * 100), 0,
           ((dga_test_score / dga_test_score_pre) -1)*100 )

  for (n in 1:nrow(dga_table_calib)){

    if (dga_test_changes > as.numeric(dga_table_calib$Lower[n]) &
        dga_test_changes <= as.numeric(dga_table_calib$Upper[n])) {
      dga_test_factor <- dga_table_calib$`DGA Test Factor`[n]
      break

    }
  }


  dga_test_mod <- data.frame(dga_test_factor,
                             dga_test_cap,
                             dga_test_collar)


  return(dga_test_mod)
}
