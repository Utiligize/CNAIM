#' @importFrom magrittr %>%
#' @title Oil Test Modifier
#' @description This function calculates the oil test modifier
#' for 33/10kV, 66/10kV and 132kV transformers and tapchangers.
#' See e.g. section 6.11 on page 68 in CNAIM (2021).
#' @param moisture Numeric. Refers to the moisture level in the
#' transformer oil. Moisture levels are measured in ppm.
#' A setting of \code{"Default"} will result in the best possible result.
#' @param acidity Numeric. Refers to the acidity level in the
#' transformer oil. Acidity levels are measured in  (mgKOH/g).
#' A setting of \code{"Default"} will result in the best possible result.
#' @param bd_strength Numeric. Refers to the breakdown strength.
#' Breakdown strength is measured in kV.
#' A setting of \code{"Default"} will result in the best possible result.
#' @param transformer_type_all String. A sting that refers to the specific
#' transformer type.
#' Options:
#' \code{transformer_type_all =
#' c("6.6/11kV Transformer (GM)", "20kV Transformer (GM)",
#' "33kV Transformer (GM)", "66kV Transformer (GM)",
#' "132kV Transformer (GM)" )}. The default setting is
#' \code{transformer_type = "66kV Transformer (GM)"}
#' @return Data table.
#' @source DNO Common Network Asset Indices Methodology (CNAIM),
#' Health & Criticality - Version 2.1, 2021:
#' \url{https://www.ofgem.gov.uk/sites/default/files/docs/2021/04/dno_common_network_asset_indices_methodology_v2.1_final_01-04-2021.pdf}
#' @export
#' @examples
#' # Oil test modifier
#' oil_test_modifier(moisture = 15,
#' acidity = 0.15,
#' bd_strength = 30,
#'  transformer_type_all = "20kV Transformer (GM)")

oil_test_modifier <- function(moisture = "Default",
                              acidity = "Default",
                              bd_strength = "Default",
                              transformer_type_all = "20kV Transformer (GM)") {



  if (transformer_type_all == "20kV Transformer (GM)" ||
      transformer_type_all == "6.6/11kV Transformer (GM)") {
    type_tf <- "HV Transformer (GM)"
  } else if (transformer_type_all == "33kV Transformer (GM)" ||
             transformer_type_all == "66kV Transformer (GM)") {
    type_tf <- "EHV Transformer (GM)"
  }  else if (transformer_type_all == "132kV Transformer (GM)") {
    type_tf <- "132kV Transformer (GM)"
  } else {
    warning("Wrong transformer type. See e.g. documentation")
  }

  if (moisture == "Default") moisture <- -0.1
  if (acidity == "Default") acidity <- -0.1
  if (bd_strength == "Default") bd_strength <- 10000

  moisture_df <- gb_ref$moisture_cond_state_calib
  moisture_df <- moisture_df[moisture_df$Type == type_tf,]

  moisture_df$Lower[1] <- -Inf
  moisture_df$Upper[nrow(moisture_df)] <- Inf

  for (n in 1:nrow(moisture_df)){

    if (moisture > as.numeric(moisture_df$Lower[n]) &
        moisture <= as.numeric(moisture_df$Upper[n])) {
      moisture_score <- moisture_df$`Moisture Score`[n]
      break

    }
  }


  acidity_df <- gb_ref$acidity_cond_state_calib
  acidity_df <- acidity_df[acidity_df$Type == type_tf, ]
  acidity_df <- acidity_df[!is.na(acidity_df$Lower), ]

  acidity_df$Lower[1] <- -Inf
  acidity_df$Upper[nrow(acidity_df)] <- Inf

  for (n in 1:nrow(acidity_df)){

    if (acidity > as.numeric(acidity_df$Lower[n]) &
        acidity <= as.numeric(acidity_df$Upper[n])) {
      acidity_score <- acidity_df$`Acidity Score`[n]
      break

    }
  }


  bd_strength_df <- gb_ref$bd_strength_cond_state_calib
  bd_strength_df <- bd_strength_df[bd_strength_df$Type == type_tf,]

  bd_strength_df$Lower[1] <- -Inf
  bd_strength_df$Upper[nrow(bd_strength_df)] <- Inf

  for (n in 1:nrow(bd_strength_df)){

    if (bd_strength > as.numeric(bd_strength_df$Lower[n]) &
        bd_strength <= as.numeric(bd_strength_df$Upper[n])) {
      bd_strength_score <- bd_strength_df$`BD Strength Score`[n]
      break

    }
  }


  # Oil condition score
  oil_condition_score <-
    80 * moisture_score +
    125 * acidity_score +
    80 * bd_strength_score

  # Oil condition factor
  oil_cond_factor_df <- gb_ref$oil_test_factor_calib
  oil_cond_factor_df <- oil_cond_factor_df[oil_cond_factor_df$Type == type_tf, ]
  oil_cond_factor_df <- oil_cond_factor_df[!is.na(oil_cond_factor_df$Lower), ]

  oil_cond_factor_df$Lower[1] <- -Inf
  oil_cond_factor_df$Upper[nrow(oil_cond_factor_df)] <- Inf

  for (n in 1:nrow(oil_cond_factor_df)){

    if (oil_condition_score > as.numeric(oil_cond_factor_df$Lower[n]) &
        oil_condition_score <= as.numeric(oil_cond_factor_df$Upper[n])) {
      oil_condition_factor <- oil_cond_factor_df$`Oil Test Factor`[n]
      break

    }
  }

  # Oil condition collar

  oil_cond_collar_df <- gb_ref$oil_test_collar_calib
  oil_cond_collar_df <- oil_cond_collar_df[oil_cond_collar_df$Type == type_tf, ]
  oil_cond_collar_df <- oil_cond_collar_df[!is.na(oil_cond_collar_df$Lower), ]

  for (n in 1:nrow(oil_cond_collar_df)){

    if (oil_condition_score > as.numeric(oil_cond_collar_df$Lower[n]) &
        oil_condition_score <= as.numeric(oil_cond_collar_df$Upper[n])) {
      oil_condition_collar <- oil_cond_collar_df$`Oil Test Collar`[n]
      break

    }
  }

  # Oil condition cap
  oil_condition_cap <- 10 # See page 69 bullet iv) Oil can be renewed

  oil_test_mod <- data.frame(oil_condition_factor,
                             oil_condition_cap,
                             oil_condition_collar)

  return(oil_test_mod)
}


