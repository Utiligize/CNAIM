#' @importFrom magrittr %>%
#' @title Oil Test Modifier
#' @description This function calculates the oil test modifier
#' for 33/10kV, 66/10kV and 132kV transformers and tapchangers.
#' See e.g. section 6.11 on page 64 in CNAIM (2017).
#' @param moisture Numeric. Refers to the moisture level in the
#' transformer oil. Moisture levels are measured in ppm.
#' A setting of \code{"Default"} will result in the best possible result.
#' @param acidity Numeric. Refers to the acidity level in the
#' transformer oil. Acidity levels are measured in  (mgKOH/g).
#' A setting of \code{"Default"} will result in the best possible result.
#' @param bd_strength Numeric. Refers to the breakdown strength.
#' Breakdown strength is measured in kV.
#' A setting of \code{"Default"} will result in the best possible result.
#' @return Data table.
#' @source DNO Common Network Asset Indices Methodology (CNAIM),
#' Health & Criticality - Version 1.1, 2017:
#' \url{https://www.ofgem.gov.uk/sites/default/files/docs/2021/04/dno_common_network_asset_indices_methodology_v2.1_final_01-04-2021.pdf}
#' @export
#' @examples
#' # Oil test modifier
#' oil_test_modifier(moisture = 15,
#' acidity = 0.15,
#' bd_strength = 30)

oil_test_modifier <- function(moisture = "Default",
                              acidity = "Default",
                              bd_strength = "Default") {

  if(moisture == "Default") moisture <- -0.1
  if(acidity == "Default") acidity <- -0.1
  if(bd_strength == "Default") bd_strength <- 10000

  # Moisture
  moisture_cond_state_calib <-
    gb_ref$moisture_cond_state_calib

  moisture_cond_state_calib$Lower[1] <- -Inf
  moisture_cond_state_calib$Upper[5] <- Inf

  for (n in 1:nrow(moisture_cond_state_calib)){

    if (moisture > as.numeric(moisture_cond_state_calib$Lower[n]) &
        moisture <= as.numeric(moisture_cond_state_calib$Upper[n])) {
      moisture_score <- moisture_cond_state_calib$`Moisture Score`[n]
      break

    }
  }

  # Acidity
  acidity_cond_state_calib <-
    gb_ref$acidity_cond_state_calib

  acidity_cond_state_calib$Lower[1] <- -Inf
  acidity_cond_state_calib$Upper[5] <- Inf

  for (n in 1:nrow(acidity_cond_state_calib)){

    if (acidity > as.numeric(acidity_cond_state_calib$Lower[n]) &
        acidity <= as.numeric(acidity_cond_state_calib$Upper[n])) {
      acidity_score <- acidity_cond_state_calib$`Acidity Score`[n]
      break

    }
  }

  # Break down strength
  bd_strength_cond_state_calib <-
    gb_ref$bd_strength_cond_state_calib

  bd_strength_cond_state_calib$Lower[1] <- -Inf
  bd_strength_cond_state_calib$Upper[4] <- Inf


  for (n in 1:nrow(bd_strength_cond_state_calib)){

    if (bd_strength > as.numeric(bd_strength_cond_state_calib$Lower[n]) &
        bd_strength <= as.numeric(bd_strength_cond_state_calib$Upper[n])) {
      bd_strength_score <- bd_strength_cond_state_calib$`BD Strength Score`[n]
      break

    }
  }

  # Oil condition score
  oil_condition_score <-
    80 * moisture_score +
    125 * acidity_score +
    80 * bd_strength_score

 # Oil condition factor

  oil_test_factor_calib <-
    gb_ref$oil_test_factor_calib

  for (n in 1:nrow(oil_test_factor_calib)){

    if (oil_condition_score > as.numeric(oil_test_factor_calib$Lower[n]) &
        oil_condition_score <= as.numeric(oil_test_factor_calib$Upper[n])) {
      oil_condition_factor <- oil_test_factor_calib$`Oil Test Factor`[n]
      break

    }
  }

  # Oil condition collar

  oil_test_collar_calib <-
    gb_ref$oil_test_collar_calib

  for (n in 1:nrow(oil_test_collar_calib)){

    if (oil_condition_score > as.numeric(oil_test_collar_calib$Lower[n]) &
        oil_condition_score <= as.numeric(oil_test_collar_calib$Upper[n])) {
      oil_condition_collar <- oil_test_collar_calib$`Oil Test Collar`[n]
      break

    }
  }

  # Oil condition cap
  oil_condition_cap <- 10 # See page 65 bullet iv) Oil can be renewed

  oil_test_mod <- data.frame(oil_condition_factor,
                             oil_condition_cap,
                             oil_condition_collar)

  return(oil_test_mod)
}


