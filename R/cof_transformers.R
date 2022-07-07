#' @title Financial cost of Failure for Transformers
#' @description This function calculates financial consequences of failure
#' (cf. section 7.3, page 79, CNAIM, 2021). Financial consequences
#' of failure is used in
#' the derivation of consequences of failure see \code{\link{cof}}().
#' @param tf_asset_category String The type of Transformer asset category
#' Options: \code{tf_asset_category = c("6.6/11kV Transformer (GM)",
#' "20kV Transformer (GM)", "33kV Transformer (GM)", "66kV Transformer (GM) "
#' "132kV Transformer (GM) ")}.
#' @param access_factor_criteria String. Asses Financial factor criteria for Transformer
#' setting (cf. table 221, page 180, CNAIM, 2021).
#' @param type_financial_factor_size String The type financial factor size for Transformer
#' @param type_financial_factor_kva_mva Numeric The type financial factor kVA MVA for Transformer
#' @return Numeric. Financial consequences of failure for Transformer
#' @source DNO Common Network Asset Indices Methodology (CNAIM),
#' Health & Criticality - Version 2.1, 2021:
#' \url{https://www.ofgem.gov.uk/sites/default/files/docs/2021/04/dno_common_network_asset_indices_methodology_v2.1_final_01-04-2021.pdf}
#' @export
#' @examples
#' financial_cof_transformers(tf_asset_category = "33kV Transformer (GM)",
#' type_financial_factor_size = "33/20kV, CMR equivalent",
#' type_financial_factor_kva_mva = 20,
#' access_factor_criteria = "Type A")
financial_cof_transformers <- function(tf_asset_category,
                                 type_financial_factor_size = NULL,
                                 type_financial_factor_kva_mva = NULL,
                                 access_factor_criteria){
  `Asset Register Category` = `Health Index Asset Category` = `Asset Category` =
    `Type Financial Factor Criteria` = `Lower` = `Upper` = NULL

  asset_category <- gb_ref$categorisation_of_assets %>%
    dplyr::filter(`Asset Register Category` == tf_asset_category) %>%
    dplyr::select(`Health Index Asset Category`) %>% dplyr::pull()

  # Reference cost of failure table 16 --------------------------------------
  reference_costs_of_failure_tf <- dplyr::filter(gb_ref$reference_costs_of_failure,
                                                 `Asset Register Category` ==
                                                   tf_asset_category)

  # Reference financial cost of failure -------------------------------------
  fcost <- reference_costs_of_failure_tf$`Financial - (GBP)`

  # Type financial factor ---------------------------------------------------
  type_financial_factors <- gb_ref$type_financial_factors

  type_financial_factors_tf <- dplyr::filter(type_financial_factors,
                                             `Asset Register Category` == tf_asset_category)

  if(!is.null(type_financial_factor_size)){
    type_financial_factors_tf <- type_financial_factors_tf %>%
      dplyr::filter(`Type Financial Factor Criteria` == type_financial_factor_size)
  }


  if(!is.null(type_financial_factor_kva_mva)){
    type_financial_factors_tf <- type_financial_factors_tf %>%
      dplyr::filter(`Lower` <= type_financial_factor_kva_mva,
                    `Upper` > type_financial_factor_kva_mva)

  }

  type_financial_factor <- type_financial_factors_tf$`Type Financial Factor`[1]

  # Access financial factor -------------------------------------------------
  access_financial_factors <- gb_ref$access_factor_swg_tf_asset

  access_financial_factors_tf <- dplyr::filter(access_financial_factors,
                                               `Asset Category` ==
                                                 asset_category)

  if (access_factor_criteria == "Type A") {
    access_finacial_factor <-
      access_financial_factors_tf$
      `Access Factor: Type A Criteria - Normal Access ( & Default Value)`
  }
  else if (access_factor_criteria == "Type B") {
    access_finacial_factor <-
      access_financial_factors_tf$`Access Factor: Type B Criteria - Constrained Access or Confined Working Space`
  }  else if (access_factor_criteria == "Type C") {
    access_finacial_factor <-
      access_financial_factors_tf$`Access Factor: Type C Criteria - Underground substation`
  }


  # Financial consequences factor -------------------------------------------
  fc_factor <- type_financial_factor * access_finacial_factor

  # Financial consequences of failure ---------------------------------------
  return(fc_factor * fcost)
}


#' @title Safety cost of Failure for Transformer
#' @description This function calculates safety consequences of failure
#' (cf. section 7.3, page 79, CNAIM, 2021). Safety consequences
#' of failure is used in
#' the derivation of consequences of failure see \code{\link{cof}}().
#' @param tf_asset_category String The type of Transformer asset category
#' Options: \code{tf_asset_category = c("6.6/11kV Transformer (GM)",
#' "20kV Transformer (GM)", "33kV Transformer (GM)", "66kV Transformer (GM) "
#' "132kV Transformer (GM) ")}.
#' @param location_risk String Type Financial factor criteria for Transformer
#' (cf. section D1.2.1, page 178, CNAIM, 2021).
#' Options: \code{location_risk = c("Low", "Medium", "High")}.
#' The default setting is
#' \code{location_risk = "Medium"}.
#' @param type_risk String. Asses Financial factor criteria for Transformer
#' setting (cf. table 221, page 180, CNAIM, 2021).
#' Options: \code{type_risk = c("Low", "Medium", "High")}.
#' The default setting is
#' \code{type_risk = "Medium"}.
#' @return Numeric. Safety consequences of failure for Transformers
#' @source DNO Common Network Asset Indices Methodology (CNAIM),
#' Health & Criticality - Version 2.1, 2021:
#' \url{https://www.ofgem.gov.uk/sites/default/files/docs/2021/04/dno_common_network_asset_indices_methodology_v2.1_final_01-04-2021.pdf}
#' @export
#' @examples
#' safety_cof_transformers(tf_asset_category = "33kV Transformer (GM)",
#' location_risk = "Default",
#' type_risk = "Default")
safety_cof_transformers <- function(tf_asset_category,
                                   location_risk,
                                   type_risk){
  `Asset Register Category` = `Health Index Asset Category` = `Asset Category` = NULL

  asset_category <- gb_ref$categorisation_of_assets %>%
    dplyr::filter(`Asset Register Category` == tf_asset_category) %>%
    dplyr::select(`Health Index Asset Category`) %>% dplyr::pull()

  reference_costs_of_failure_tf <- dplyr::filter(gb_ref$reference_costs_of_failure,
                                                 `Asset Register Category` ==
                                                   tf_asset_category)

  # Reference financial cost of failure -------------------------------------
  scost <- reference_costs_of_failure_tf$`Safety - (GBP)`

  if (location_risk == "Default") location_risk <- "Medium (Default)"
  if (location_risk == "Medium") location_risk <- "Medium (Default)"
  if (type_risk == "Default") type_risk <- "Medium"

  safety_conseq_factor_sg_tf_oh <- gb_ref$safety_conseq_factor_sg_tf_oh

  row_no <- which(safety_conseq_factor_sg_tf_oh$
                    `Safety Consequence Factor - Switchgear, Transformers & Overhead Lines...2` ==
                    location_risk)

  col_no <- grep(type_risk, colnames(safety_conseq_factor_sg_tf_oh))

  safety_consequence_factor <- safety_conseq_factor_sg_tf_oh[row_no, col_no]

  # Safety consequence of failure -------------------------------------------
  safety_cof <- safety_consequence_factor * scost

  return(safety_cof)
}


#' @title Environmental cost of Failure for Transformers
#' @description This function calculates environmental consequences of failure
#' (cf. section 7.3, page 79, CNAIM, 2021). Environmental consequences
#' of failure is used in
#' the derivation of consequences of failure see \code{\link{cof}}().#' @return Numeric. Financial consequences of failure for LV switchgear
#' @param tf_asset_category String The type of Transformer asset category
#' Options: \code{tf_asset_category = c("6.6/11kV Transformer (GM)",
#' "20kV Transformer (GM)", "33kV Transformer (GM)", "66kV Transformer (GM) "
#' "132kV Transformer (GM) ")}.
#' @param prox_water Numeric. Specify the proximity to a water course in meters.
#' A setting of \code{"Default"} will result in a proximity factor of 1. Thus
#' assume the proximity to a water course is between 80m and 120m
#' (cf. table 231, page 188, CNAIM, 2021).
#' @param bunded String. Options: \code{bunded = c("Yes", "No", "Default")}.
#' A setting of \code{"Default"} will result in a bunding factor of 1.
#' @param size_kva_mva Numeric The MVA KVA rating for the transformer
#' @param size_conversion String The size conversion for the transformer
#' @source DNO Common Network Asset Indices Methodology (CNAIM),
#' Health & Criticality - Version 1.1, 2017:
#' \url{https://www.ofgem.gov.uk/sites/default/files/docs/2021/04/dno_common_network_asset_indices_methodology_v2.1_final_01-04-2021.pdf}
#' @export
#' @examples
#' environmental_cof_transformers(tf_asset_category = "33kV Transformer (GM)",
#' prox_water = 95, bunded = "Yes", size_kva_mva = 20, size_conversion = "33/20kV")
environmental_cof_transformers <- function(tf_asset_category,
                                     prox_water, bunded,
                                     size_kva_mva = NULL,
                                     size_conversion = NULL){
  `Asset Register Category` = `Health Index Asset Category` = `Asset Category` =
    `Type environment factor` = `Size` = `Lower` = `Upper` = NULL

  asset_category <- gb_ref$categorisation_of_assets %>%
    dplyr::filter(`Asset Register Category` == tf_asset_category) %>%
    dplyr::select(`Health Index Asset Category`) %>% dplyr::pull()

  reference_costs_of_failure_tf <- dplyr::filter(gb_ref$reference_costs_of_failure,
                                                 `Asset Register Category` ==
                                                   tf_asset_category)

  # Reference financial cost of failure -------------------------------------
  ecost <- reference_costs_of_failure_tf$`Environmental - (GBP)`

  # Type env factor -------------------------------------
  type_environmental_factor <- 1

  # Size env factor -------------------------------------
  size_environmental_factor_df <- gb_ref$size_enviromental_factor

  size_environmental_factor_df <- size_environmental_factor_df %>%
    dplyr::filter(`Asset Register Category` == tf_asset_category)

  if(!is.null(size_conversion)){
    size_environmental_factor_df <- size_environmental_factor_df %>%
      dplyr::filter(`Size` == size_conversion)
  }

  if(!is.null(size_kva_mva)){
    size_environmental_factor_df <- size_environmental_factor_df %>%
      dplyr::filter(`Lower` <= size_kva_mva,
                    `Upper` > size_kva_mva)
  }

  size_environmental_factor <- size_environmental_factor_df$`Size Environmental Factor`[1]

  # Location environmetal factor table 222 ----------------------------------
  location_environ_al_factor <- gb_ref$location_environ_al_factor

  location_environ_al_factor_tf <- dplyr::filter(location_environ_al_factor,
                                                 `Asset Register Category` ==
                                                   asset_category)

  if(nrow(location_environ_al_factor_tf) == 0){
    location_environmental_factor <- 1
  } else {
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
  }


  environmental_consequences_factor <- (type_environmental_factor *
                                          size_environmental_factor *
                                          location_environmental_factor)

  # Environmental consequences ----------------------------------------------
  environmental_cof <- environmental_consequences_factor * ecost
  return(environmental_cof)
}


#' @title Network cost of Failure for Transformers
#' @description This function calculates network cost of failure for
#' all asset categories exclusive the assets EHV and 132kV transformers.
#' (cf. section 7.6, page 87, CNAIM, 2021). Network cost of failure
#' is used in the derivation of consequences of failure see \code{\link{cof}}().
#' @param tf_asset_category String The type of Transformer asset category
#' Options: \code{tf_asset_category = c("6.6/11kV Transformer (GM)",
#' "20kV Transformer (GM)", "33kV Transformer (GM)", "66kV Transformer (GM) "
#' "132kV Transformer (GM) ")}.
#' @param actual_load_mva Numeric. The actual load on the asset
#' @param secure Boolean If the asset is in a secure network or not
#' @return Numeric. Network cost of failure.
#' @source DNO Common Network Asset Indices Methodology (CNAIM),
#' Health & Criticality - Version 2.1, 2021:
#' \url{https://www.ofgem.gov.uk/sites/default/files/docs/2021/04/dno_common_network_asset_indices_methodology_v2.1_final_01-04-2021.pdf}
#' @export
#' @examples
#' network_cof_transformers(tf_asset_category = "33kV Transformer (GM)",
#' actual_load_mva = 15)
network_cof_transformers<- function(tf_asset_category,
                             actual_load_mva,
                             secure = T) {

  `Asset Register Category` = `Health Index Asset Category` = `Asset Category` =
    `Maximum Demand Used To Derive Reference Cost (MVA)` = NULL

  reference_costs_of_failure_tf <- dplyr::filter(gb_ref$reference_costs_of_failure,
                                                 `Asset Register Category` ==
                                                   tf_asset_category)

  # Reference financial cost of failure -------------------------------------
  ncost <- reference_costs_of_failure_tf$`Network Performance - (GBP)`

  # Load factor ---------------------------------------------------------
  if(tf_asset_category == "132kV Transformer (GM)")
    load_factor_asset_category <- "132kV Transformer"
  if(tf_asset_category == "66kV Transformer (GM)")
    load_factor_asset_category <- "66kV Transformer"
  if(tf_asset_category == "33kV Transformer (GM)")
    load_factor_asset_category <- "33kV Transformer (GM)"


  ref_nw_perf_cost_fail_ehv_df <- gb_ref$ref_nw_perf_cost_of_fail_ehv
  ref_nw_perf_cost_fail_ehv_single_row_df <- dplyr::filter(ref_nw_perf_cost_fail_ehv_df,
                                                           `Asset Category` ==
                                                             load_factor_asset_category)

  load_factor <- actual_load_mva/ref_nw_perf_cost_fail_ehv_single_row_df$`Maximum Demand Used To Derive Reference Cost (MVA)`

  # Network type factor -----------------------------------
  network_type_factor <- 1

  if(!secure){
    network_type_factor <- 2.5
  }

  # Network perfomance consequence factor -----------------------------------

  network_performance_consequence_factor <- load_factor *
    network_type_factor

  # Network performance cost of failure -------------------------------------
  network_cof <- network_performance_consequence_factor * ncost

  return(network_cof)

}
