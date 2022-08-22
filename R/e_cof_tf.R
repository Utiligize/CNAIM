#' @importFrom magrittr %>%
#' @title Environmental Consequences of Failure for transformers
#' @description This function calculates environmental consequences of failure
#' for all type of transformers.
#' (cf. section 7.5, page 84, CNAIM, 2021). Environmental consequences
#'  of failure is used in
#' the derivation of consequences of failure see \code{\link{cof}}().
#' @param asset_type_tf String. Transformer types.
#' Options:
#' \code{asset_type_tf = c("6.6/11kV Transformer (GM)",
#' "20kV Transformer (GM)", "33kV Transformer (GM)",
#' "66kV Transformer (GM)", "132kV Transformer (GM)")}.
#' @param rated_capacity Numeric. The rated capacity for a transformer. For type
#' \code{"6.6/11kV Transformer (GM)"} and \code{"20kV Transformer (GM)"} use kVA
#' ratings. For \code{"20kV Transformer (GM)", "33kV Transformer (GM)",
#' "66kV Transformer (GM)", "132kV Transformer (GM)"} use MVA ratings.
#' A setting of \code{"Default"} will result in a
#' size environmental factor of 1 (cf. table 230, page 187, CNAIM, 2021).
#' @param prox_water Numeric. Specify the proximity to a water course in meters.
#' A setting of \code{"Default"} will result in a proximity factor of 1. Thus
#' assume the proximity to a water course is between 80m and 120m
#' (cf. table 231, page 188, CNAIM, 2021).
#' @param bunded String. Options: \code{bunded = c("Yes", "No", "Default")}.
#' A setting of \code{"Default"} will result in a bunding factor of 1.
#' @param gb_ref_given optional parameter to use custom reference values
#' @return Numeric. Financial cost of failure for a 10kV transformer.
#' @source DNO Common Network Asset Indices Methodology (CNAIM),
#' Health & Criticality - Version 2.1, 2021:
#' \url{https://www.ofgem.gov.uk/sites/default/files/docs/2021/04/dno_common_network_asset_indices_methodology_v2.1_final_01-04-2021.pdf}
#' @export
#' @examples
#' # Environmental consequences of failure for a 6.6/11 kV transformer
#' e_cof_tf(asset_type_tf = "6.6/11kV Transformer (GM)",
#' rated_capacity = 750, prox_water = 100, bunded = "Yes")

e_cof_tf <- function(asset_type_tf, rated_capacity = "Default",
                     prox_water = "Default", bunded = "Default",
                     gb_ref_given = NULL) {

  `Asset Register Category` = `Health Index Asset Category` = `Lower` = NULL
  # due to NSE notes in R CMD check

  if(is.null(gb_ref_given)){
    gb_ref_taken <- gb_ref
  }else{
    check_gb_ref_given(gb_ref_given)
    gb_ref_taken <- gb_ref_given
  }

  # Get category ------------------------------------------------------------

  asset_category <- gb_ref_taken$categorisation_of_assets %>%
    dplyr::filter(`Asset Register Category` == asset_type_tf) %>%
    dplyr::select(`Health Index Asset Category`) %>% dplyr::pull()

  # Reference cost of failure table 16 --------------------------------------
  reference_costs_of_failure <-
    gb_ref_taken$reference_costs_of_failure


    reference_costs_of_failure_tf <- dplyr::filter(reference_costs_of_failure,
                                                   `Asset Register Category` ==
                                                     asset_type_tf)


  # Reference environmental cost of failure ---------------------------------
  ecost <- reference_costs_of_failure_tf$`Environmental - (GBP)`

  # Type environmetal factor table 221 --------------------------------------
  type_environmetal_factors <- 1 # Default for assets not in table 221

  # Size environmetal factor table 222 --------------------------------------
  if (asset_type_tf == "132kV Transformer (GM)") {
    size_enviromental_factor_tf <- dplyr::filter(
      gb_ref_taken$size_enviromental_factor,
      `Asset Register Category` ==
        asset_type_tf)

    size_enviromental_factor_tf <-
      size_enviromental_factor_tf %>% dplyr::filter(!is.na(Lower))

  } else {

    size_enviromental_factor_tf <- dplyr::filter(
      gb_ref_taken$size_enviromental_factor,
      `Asset Register Category` ==
        asset_type_tf)
  }

  for (n in 1:nrow(size_enviromental_factor_tf)){
    if (rated_capacity == 'Default'){
      size_environmental_factor <- 1
      break
    } else if (rated_capacity >= as.numeric(
      size_enviromental_factor_tf$Lower[n]) &
      rated_capacity < as.numeric(
        size_enviromental_factor_tf$Upper[n])){
      size_environmental_factor <-
        size_enviromental_factor_tf$`Size Environmental Factor`[n]
      break
    }
  }


  # Location environmetal factor table 222 ----------------------------------
  location_environ_al_factor <- gb_ref_taken$location_environ_al_factor

  location_environ_al_factor_tf <- dplyr::filter(location_environ_al_factor,
                                                 `Asset Register Category` ==
                                                   asset_category)

  # Bunded "Yes", "No", "Default" ?
  if (bunded == "Default") {
    bunding_factor <- 1
  } else if (bunded == "Yes") {
    bunding_factor <-
      location_environ_al_factor_tf$`Bunding Factor: Bunded`
  } else if (bunded == "No") {
    bunding_factor <-
      location_environ_al_factor_tf$`Bunding Factor: Not bunded`
  }

  # Proximity to water.
  if(prox_water == "Default") {
    prox_factor <- 1
  } else if (prox_water >= 40 && prox_water < 80) {
    prox_factor <- location_environ_al_factor_tf$
      `Proximity Factor: Close to Water Course (between 40m and 80m)`
  } else if (prox_water >= 80 && prox_water < 120) {
    prox_factor <- location_environ_al_factor_tf$
    `Proximity Factor: Moderately Close to Water Course (between 80m and 120m)`
  } else if (prox_water > 120) {
    prox_factor <- location_environ_al_factor_tf$
      `Proximity Factor: Not Close to Water Course (>120m) or No Oil`
  } else if (prox_water < 40) {
    prox_factor <- location_environ_al_factor_tf$
      `Proximity Factor: Very Close to Water Course (<40m)`
  }

  # Location environmental factor
  location_environmental_factor <- prox_factor * bunding_factor

  # Environmental consequences factor ---------------------------------------
  environmental_consequences_factor <- (type_environmetal_factors *
                                        size_environmental_factor *
                                        location_environmental_factor)

  # Environmental consequences ----------------------------------------------
  environmental_cof <- environmental_consequences_factor * ecost
  return(environmental_cof)
}
