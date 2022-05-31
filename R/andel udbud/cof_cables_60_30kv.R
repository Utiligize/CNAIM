#' @title Financial cost of Failure for 30-60 kV UG cables
#' @description This function calculates financial consequences of failure
#' \code{ehv_asset_category = c("30kV UG Cable (Gas)", "60kV UG Cable (Gas)",
#' "30kV UG Cable (Non Pressurised)", "60kV UG Cable (Non Pressurised)",
#' "30kV UG Cable (Oil)", "60kV UG Cable (Oil)")
#'}. The default setting is
#' \code{ehv_asset_category = "60kV UG Cable (Gas)"}.
#' @return Numeric. Financial consequences of failure for 30-60 kV UG cables
#' @export
#' @examples
#' financial_cof_cables_60_30kv(ehv_asset_category = "30kV UG Cable (Oil)")
financial_cof_cables_60_30kv <- function(ehv_asset_category){

  if (ehv_asset_category == "30kV UG Cable (Non Pressurised)" ) {
    ehv_asset_category <- "33kV UG Cable (Non Pressurised)"
  } else if ( ehv_asset_category == "30kV UG Cable (Oil)") {
    ehv_asset_category <- "33kV UG Cable (Oil)"
  } else if ( ehv_asset_category == "30kV UG Cable (Gas)") {
    ehv_asset_category <- "33kV UG Cable (Gas)"
  } else if ( ehv_asset_category == "60kV UG Cable (Non Pressurised)") {
    ehv_asset_category <- "66kV UG Cable (Non Pressurised)"
  } else if ( ehv_asset_category == "60kV UG Cable (Oil)") {
    ehv_asset_category <- "66kV UG Cable (Oil)"
  } else {
    ehv_asset_category <- "66kV UG Cable (Gas)"
  }


  `Asset Register Category` = `Health Index Asset Category` = `Asset Category` = NULL

  asset_category <- gb_ref$categorisation_of_assets %>%
    dplyr::filter(`Asset Register Category` == ehv_asset_category) %>%
    dplyr::select(`Health Index Asset Category`) %>% dplyr::pull()

  # Reference cost of failure table 16 --------------------------------------
  reference_costs_of_failure_tf <- dplyr::filter(gb_ref$reference_costs_of_failure,
                                                 `Asset Register Category` ==
                                                   ehv_asset_category)

  # Reference financial cost of failure -------------------------------------
  fcost <- reference_costs_of_failure_tf$`Financial - (GBP)`

  # Type financial factor ---------------------------------------------------
  type_financial_factor <- 1

  # Access financial factor -------------------------------------------------
  access_financial_factor <- 1

  # Financial consequences factor -------------------------------------------
  fc_factor <- type_financial_factor * access_financial_factor

  # Financial consequences of failure ---------------------------------------
  return(fc_factor * fcost)
}


#' @title Safety cost of Failure for 30-60 kV UG cables
#' @description This function calculates safety consequences of failure
#' #' \code{ehv_asset_category = c("30kV UG Cable (Gas)", "60kV UG Cable (Gas)",
#' "30kV UG Cable (Non Pressurised)", "60kV UG Cable (Non Pressurised)",
#' "30kV UG Cable (Oil)", "60kV UG Cable (Oil)")
#'}. The default setting is
#' \code{ehv_asset_category = "60kV UG Cable (Gas)"}.
#' @return Numeric. Financial consequences of failure for 30-60 kV UG cables
#' @export
#' @examples
#' safety_cof_cables_60_30kv(ehv_asset_category = "30kV UG Cable (Oil)")
safety_cof_cables_60_30kv <- function(ehv_asset_category){

  if (ehv_asset_category == "30kV UG Cable (Non Pressurised)" ) {
    ehv_asset_category <- "33kV UG Cable (Non Pressurised)"
  } else if ( ehv_asset_category == "30kV UG Cable (Oil)") {
    ehv_asset_category <- "33kV UG Cable (Oil)"
  } else if ( ehv_asset_category == "30kV UG Cable (Gas)") {
    ehv_asset_category <- "33kV UG Cable (Gas)"
  } else if ( ehv_asset_category == "60kV UG Cable (Non Pressurised)") {
    ehv_asset_category <- "66kV UG Cable (Non Pressurised)"
  } else if ( ehv_asset_category == "60kV UG Cable (Oil)") {
    ehv_asset_category <- "66kV UG Cable (Oil)"
  } else {
    ehv_asset_category <- "66kV UG Cable (Gas)"
  }

  `Asset Register Category` = `Health Index Asset Category` = `Asset Category` = NULL

  asset_category <- gb_ref$categorisation_of_assets %>%
    dplyr::filter(`Asset Register Category` == ehv_asset_category) %>%
    dplyr::select(`Health Index Asset Category`) %>% dplyr::pull()

  reference_costs_of_failure_tf <- dplyr::filter(gb_ref$reference_costs_of_failure,
                                                 `Asset Register Category` ==
                                                   ehv_asset_category)

  # Reference financial cost of failure -------------------------------------
  scost <- reference_costs_of_failure_tf$`Safety - (GBP)`

  # Assuming that all cables are always buried
  safety_consequence_factor <- 1

  # Safety consequence of failure -------------------------------------------
  safety_cof <- safety_consequence_factor * scost

  return(safety_cof)
}


#' @title Environmental cost of Failure for 30-60 kV UG cables
#' @description This function calculates environmental consequences of failure
#' \code{ehv_asset_category = c("30kV UG Cable (Gas)", "60kV UG Cable (Gas)",
#' "30kV UG Cable (Non Pressurised)", "60kV UG Cable (Non Pressurised)",
#' "30kV UG Cable (Oil)", "60kV UG Cable (Oil)")
#'}. The default setting is
#' \code{ehv_asset_category = "60kV UG Cable (Gas)"}.
#' @param prox_water Numeric. Specify the proximity to a water course in meters.
#' A setting of \code{"Default"} will result in a proximity factor of 1. Thus
#' assume the proximity to a water course is between 80m and 120m
#' @param bunded String. Options: \code{bunded = c("Yes", "No", "Default")}.
#' A setting of \code{"Default"} will result in a bunding factor of 1.
#' @export
#' @examples
#' environmental_cof_cables_60_30kv(ehv_asset_category = "30kV UG Cable (Oil)",
#' prox_water = 95, bunded = "Yes")
environmental_cof_cables_60_30kv <- function(ehv_asset_category,
                                              prox_water,
                                              bunded){

  if (ehv_asset_category == "30kV UG Cable (Non Pressurised)" ) {
    ehv_asset_category <- "33kV UG Cable (Non Pressurised)"
  } else if ( ehv_asset_category == "30kV UG Cable (Oil)") {
    ehv_asset_category <- "33kV UG Cable (Oil)"
  } else if ( ehv_asset_category == "30kV UG Cable (Gas)") {
    ehv_asset_category <- "33kV UG Cable (Gas)"
  } else if ( ehv_asset_category == "60kV UG Cable (Non Pressurised)") {
    ehv_asset_category <- "66kV UG Cable (Non Pressurised)"
  } else if ( ehv_asset_category == "60kV UG Cable (Oil)") {
    ehv_asset_category <- "66kV UG Cable (Oil)"
  } else {
    ehv_asset_category <- "66kV UG Cable (Gas)"
  }

  `Asset Register Category` = `Health Index Asset Category` = `Asset Category` =
    `Type environment factor` = NULL

  asset_category <- gb_ref$categorisation_of_assets %>%
    dplyr::filter(`Asset Register Category` == ehv_asset_category) %>%
    dplyr::select(`Health Index Asset Category`) %>% dplyr::pull()

  reference_costs_of_failure_tf <- dplyr::filter(gb_ref$reference_costs_of_failure,
                                                 `Asset Register Category` ==
                                                   ehv_asset_category)

  # Reference financial cost of failure -------------------------------------
  ecost <- reference_costs_of_failure_tf$`Environmental - (GBP)`

  # Type env factor -------------------------------------
  type_environmental_factor <- 1

  # Size env factor -------------------------------------
  size_environmental_factor <- 1

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


#' @title Network cost of Failure for 30-60 kV UG cables
#' @description This function calculates network cost of failure for
#' \code{ehv_asset_category = c("30kV UG Cable (Gas)", "60kV UG Cable (Gas)",
#' "30kV UG Cable (Non Pressurised)", "60kV UG Cable (Non Pressurised)",
#' "30kV UG Cable (Oil)", "60kV UG Cable (Oil)")
#'}. The default setting is
#' \code{ehv_asset_category = "60kV UG Cable (Gas)"}.
#' @param actual_load_mva Numeric. The actual load on the asset
#' @param secure Boolean If the asset is in a secure network or not
#' @return Numeric. Network cost of failure.
#' @export
#' @examples
#' network_cof_cables_60_30kv(ehv_asset_category = "30kV UG Cable (Oil)",
#' actual_load_mva = 15)
network_cof_cables_60_30kv <- function(ehv_asset_category,
                                  actual_load_mva,
                                  secure = T) {

  if (ehv_asset_category == "30kV UG Cable (Non Pressurised)" ) {
    ehv_asset_category <- "33kV UG Cable (Non Pressurised)"
  } else if ( ehv_asset_category == "30kV UG Cable (Oil)") {
    ehv_asset_category <- "33kV UG Cable (Oil)"
  } else if ( ehv_asset_category == "30kV UG Cable (Gas)") {
    ehv_asset_category <- "33kV UG Cable (Gas)"
  } else if ( ehv_asset_category == "60kV UG Cable (Non Pressurised)") {
    ehv_asset_category <- "66kV UG Cable (Non Pressurised)"
  } else if ( ehv_asset_category == "60kV UG Cable (Oil)") {
    ehv_asset_category <- "66kV UG Cable (Oil)"
  } else {
    ehv_asset_category <- "66kV UG Cable (Gas)"
  }

  `Asset Register Category` = `Health Index Asset Category` = `Asset Category` =
    `Maximum Demand Used To Derive Reference Cost (MVA)` = NULL

  reference_costs_of_failure_tf <- dplyr::filter(gb_ref$reference_costs_of_failure,
                                                 `Asset Register Category` ==
                                                   ehv_asset_category)

  # Reference financial cost of failure -------------------------------------
  ncost <- reference_costs_of_failure_tf$`Network Performance - (GBP)`

  # Load factor ---------------------------------------------------------
  ref_nw_perf_cost_fail_ehv_df <- gb_ref$ref_nw_perf_cost_of_fail_ehv
  ref_nw_perf_cost_fail_ehv_single_row_df <- dplyr::filter(ref_nw_perf_cost_fail_ehv_df,
                                                           `Asset Category` ==
                                                             ehv_asset_category)

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
