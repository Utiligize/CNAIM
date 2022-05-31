#' @title Location Factor (Excl.Submarine Cables)
#' @description This function calculates the location factor for
#' an electric network asset based in the specific location of the asset.
#' See section 6.4 on page 46 in CNAIM (2021). For calculating the location
#' factor for submarine cables please see the function
#' \code{\link{location_factor_sub}()}. Note the location factor for all other
#' cables are always equal to 1 hence the function will return a location
#' factor of 1 for other cables than submarine cables.
#' @param placement String. Specify if the asset is located outdoor or indoor.
#' A setting of \code{"Outdoor"} means the asset is
#' located in an outside environment,
#' and a setting of \code{"Indoor"} means the asset is located in an
#'  indoor environment. A setting of \code{"Default"} will result
#'   in either an indoor or an outdoor environment setting that depends
#'   on the specification of \code{asset_type}. See page 110-113,
#'   table 26 in CNAIM (2021) for default environments.
#' @param altitude_m Numeric. Specify the altitude location for
#' the asset measured in meters from sea level.\code{altitude_m}
#' is used to derive the altitude factor. See page 111,
#' table 23 in CNAIM (2021). A setting of \code{"Default"}
#' will set the altitude factor to 1 independent of \code{asset_type}.
#' @param distance_from_coast_km Numeric. Specify the distance from the
#' coast measured in kilometers. \code{distance_from_coast_km} is used
#' to derive the distance from coast factor See page 110,
#' table 22 in CNAIM (2021). A setting of \code{"Default"} will set the
#'  distance from coast factor to 1 independent of \code{asset_type}.
#' @param corrosion_category_index Integer.
#' Specify the corrosion index category, 1-5.
#' \code{corrosion_category_index} is used to derive the corrosion
#' category factor. See page 111, table 24 in CNAIM (2021).
#' A setting of \code{"Default"} will set the corrosion category factor
#' to 1 independent of \code{asset_type}.
#' @param asset_type String.
#' A sting that refers to the specific asset category.
#' For LV UGB and non-submarine cables a location factor of 1 is assigned.
#' See See page 17, table 1 in CNAIM (2021).
#' Options:
#' \code{asset_type = c("LV Poles", "LV Circuit Breaker",
#' "LV Pillar (ID)", "LV Pillar (OD at Substation)",
#' "LV Pillar (OD not at a Substation)", "LV Board (WM)",
#' "LV UGB", "LV Board (X-type Network) (WM)", "6.6/11kV Poles",
#'  "20kV Poles", "6.6/11kV CB (GM) Primary",
#'"6.6/11kV CB (GM) Secondary", "6.6/11kV Switch (GM)", "6.6/11kV RMU",
#' "6.6/11kV X-type RMU", "20kV CB (GM) Primary", "20kV CB (GM) Secondary",
#'  "20kV Switch (GM)", "20kV RMU", "6.6/11kV Transformer (GM)",
#'  "20kV Transformer (GM)", "33kV Pole", "66kV Pole",
#'  "33kV OHL (Tower Line) Conductor", "33kV Tower", "33kV Fittings",
#'"66kV OHL (Tower Line) Conductor", "66kV Tower", "66kV Fittings",
#'"33kV UG Cable (Non Pressurised)", "33kV UG Cable (Oil)",
#'"33kV UG Cable (Gas)", "66kV UG Cable (Non Pressurised)",
#'"66kV UG Cable (Oil)", "66kV UG Cable (Gas)",
#'"33kV CB (Air Insulated Busbars)(ID) (GM)",
#'"33kV CB (Air Insulated Busbars)(OD) (GM)",
#'"33kV CB (Gas Insulated Busbars)(ID) (GM)",
#'"33kV CB (Gas Insulated Busbars)(OD) (GM)", "33kV Switch (GM)",
#'"33kV RMU", "66kV CB (Air Insulated Busbars)(ID) (GM)",
#'"66kV CB (Air Insulated Busbars)(OD) (GM)",
#'"66kV CB (Gas Insulated Busbars)(ID) (GM)",
#'"66kV CB (Gas Insulated Busbars)(OD) (GM)", "33kV Transformer (GM)",
#' "66kV Transformer (GM)", "132kV OHL (Tower Line) Conductor",
#'"132kV Tower", "132kV Fittings", "132kV UG Cable (Non Pressurised)",
#' "132kV UG Cable (Oil)", "132kV UG Cable (Gas)",
#' "132kV CB (Air Insulated Busbars)(ID) (GM)",
#'"132kV CB (Air Insulated Busbars)(OD) (GM)",
#'"132kV CB (Gas Insulated Busbars)(ID) (GM)",
#'"132kV CB (Gas Insulated Busbars)(OD) (GM)", "132kV Transformer (GM)")
#'}
#' @param sub_division String. Refers to material the sub division in the asset category
#' @return Numeric. Location factor
#' @source DNO Common Network Asset Indices Methodology (CNAIM),
#' Health & Criticality - Version 2.1, 2021:
#'\url{https://www.ofgem.gov.uk/sites/default/files/docs/2021/04/dno_common_network_asset_indices_methodology_v2.1_final_01-04-2021.pdf}
#' @export
#' @examples
#'  # Location factor for a 6.6/11 kV Transformer with default values
#' location_factor(placement = "Default", altitude_m = "Default",
#'distance_from_coast_km = "Default",
#'corrosion_category_index = "Default",
#'asset_type = "6.6/11kV Transformer (GM)")

location_factor <- function(placement = "Default",
                            altitude_m = "Default",
                            distance_from_coast_km = "Default",
                            corrosion_category_index = "Default",
                            asset_type = "6.6/11kV Transformer (GM)",
                            sub_division = NULL) {


if (asset_type == "LV UGB" ||
    asset_type == "33kV UG Cable (Non Pressurised)" ||
    asset_type == "33kV UG Cable (Oil)" ||
    asset_type == "33kV UG Cable (Gas)" ||
    asset_type == "66kV UG Cable (Non Pressurised)" ||
    asset_type == "66kV UG Cable (Oil)" ||
    asset_type == "66kV UG Cable (Gas)" ||
    asset_type == "132kV UG Cable (Non Pressurised)" ||
    asset_type == "132kV UG Cable (Oil)" ||
    asset_type == "132kV UG Cable (Gas)"  ){

  location_factor_asset <- 1

} else {
  # Find generic term -------------------------------------------------------
  asset_category <- gb_ref$categorisation_of_assets$
    `Health Index Asset Category`[which(gb_ref$
                                          categorisation_of_assets$
                                          `Asset Register Category` ==
                                          asset_type)]

  generic_term_1 <- gb_ref$generic_terms_for_assets$
    `Generic Term...1`[which(gb_ref$
                               generic_terms_for_assets$
                               `Health Index Asset Category` ==
                               asset_category)]



  if (asset_category == "EHV OHL Conductor (Tower Lines)" ||
      asset_category == "132kV OHL Conductor (Tower Lines)") {
    generic_term_1 <- "Towers (Conductor)"
  } else if (asset_category == "EHV OHL Fittings" ||
             asset_category == "132kV OHL Fittings") {
    generic_term_1 <- "Towers (Fittings)"
  }else if (asset_category == "HV OHL Support - Poles" ||
            asset_category == "EHV OHL Support - Poles" ||
            asset_category == "LV OHL Support" ) {
    # All the poles
    if(sub_division %>% is.null())
      stop("No sub division specified for the pole")
    if(sub_division == "Steel")
      generic_term_1 <- "Poles (Steel)"
    if(sub_division == "Concrete")
      generic_term_1 <- "Poles (Concrete)"
    if(sub_division == "Wood")
      generic_term_1 <- "Poles (Wood)"
  }else if(asset_category == "EHV OHL Support - Towers" ||
           asset_category == "132kV OHL Support - Tower"){
    generic_term_1 <- "Towers (Structure)"
  }
  if (asset_category == "Overhead Line") {
    stop(paste0("Asset type not implemented: ", asset_type))
  }

   # Altitude ----------------------------------------------------------------
  altitude_factor_asset_df <- dplyr::select(gb_ref$altitude_factor_lut,
                                            c("Lower", "Upper",
                                              generic_term_1))
  if (altitude_m == "Default") {
    row_no <- which(altitude_factor_asset_df$Lower == "Default")
  } else if (altitude_m <= 100) {
    row_no <- which(altitude_factor_asset_df$Lower == "0")
  } else if (100 < altitude_m && altitude_m <= 200) {
    row_no <- which(altitude_factor_asset_df$Lower == "100")
  } else if (200 < altitude_m && altitude_m <= 300) {
    row_no <- which(altitude_factor_asset_df$Lower == "200")
  } else {
    row_no <- which(altitude_factor_asset_df$Lower == "300")
  }

  altitude_factor <-
    as.numeric(altitude_factor_asset_df[row_no, generic_term_1])

  # Corrosion ----------------------------------------------------------------
  corrosion_category_factor_a <-
    dplyr::select(gb_ref$corrosion_category_factor_lut,
                  c("Corrosion Category Index", generic_term_1))

  if (corrosion_category_index == "Default") {
    row_no <-
      which(corrosion_category_factor_a$`Corrosion Category Index` ==
              "Default")
  } else if (corrosion_category_index == 1) {
    row_no <- which(corrosion_category_factor_a$
                      `Corrosion Category Index` == "1")
  } else if (corrosion_category_index == 2) {
    row_no <- which(corrosion_category_factor_a$
                      `Corrosion Category Index` == "2")
  } else if (corrosion_category_index == 3) {
    row_no <- which(corrosion_category_factor_a$
                      `Corrosion Category Index` == "3")
  } else if (corrosion_category_index == 4) {
    row_no <- which(corrosion_category_factor_a$
                      `Corrosion Category Index` == "4")
  }else if (corrosion_category_index == 5) {
    row_no <- which(corrosion_category_factor_a$
                      `Corrosion Category Index` == "5")
  }
  corrosion_factor <-
    as.numeric(corrosion_category_factor_a[row_no, generic_term_1])

  # Distance from coast -----------------------------------------------------
  distance_from_coast_factor_lut <- gb_ref$distance_from_coast_factor_lut

  distance_from_coast_factor_a <-
    dplyr::select(distance_from_coast_factor_lut,
                  c("Lower", "Upper", generic_term_1))

  if (distance_from_coast_km == "Default") {
    row_no <- which(distance_from_coast_factor_a$Lower == "Default")
  } else if (distance_from_coast_km <= 1) {
    row_no <- which(distance_from_coast_factor_a$Lower == "0")
  } else if (1 < distance_from_coast_km && distance_from_coast_km <= 5) {
    row_no <- which(distance_from_coast_factor_a$Lower == "1")
  } else if (5 < distance_from_coast_km && distance_from_coast_km <= 10) {
    row_no <- which(distance_from_coast_factor_a$Lower == "5")
  } else if (10 < distance_from_coast_km && distance_from_coast_km <= 20) {
    row_no <- which(distance_from_coast_factor_a$Lower == "10")
  }else {
    row_no <- which(distance_from_coast_factor_a$Lower == "20")
  }
  coast_factor <-
    as.numeric(distance_from_coast_factor_a[row_no, generic_term_1])

  # Increment constant ------------------------------------------------------
  increment_constants <- gb_ref$increment_constants
  inc_constant <- increment_constants[, generic_term_1]

  # All factors -------------------------------------------------------------
  factors <- c(coast_factor, corrosion_factor, altitude_factor)

  # Location factor outdoor -------------------------------------------------
  environment_indoor_outdoor <- gb_ref$environment_indoor_outdoor

  if (placement == "Default") {
    placement <-
      environment_indoor_outdoor$
      `Default 'environment' to be assumed when deriving Location Factor`[
        which(
          environment_indoor_outdoor$`Asset Register Category` == asset_type)]
  }

  if (placement == "Outdoor") {
    if (max(factors) > 1) {
      count_factor <- length(which(factors > 1))
      location_factor_asset <- max(factors) +
        ((count_factor - 1) * inc_constant)
    } else {
      location_factor_asset <- min(factors)
    }
  } else if (placement == "Indoor") {
    if (max(factors) > 1) {
      count_factor <- length(which(factors > 1))
      initial_location_factor <- max(factors) +
        ((count_factor - 1) * inc_constant)
    } else {
      initial_location_factor <- min(factors)
    }

    min_coast_factor <-
      min(as.numeric(distance_from_coast_factor_a[, generic_term_1]))
    min_corrosion_factor <-
      min(as.numeric(corrosion_category_factor_a[, generic_term_1]))
    min_altitude_factor <-
      min(as.numeric(altitude_factor_asset_df[, generic_term_1]))
    min_initial_location_factors <-
      c(min_coast_factor, min_corrosion_factor, min_altitude_factor)

    if (max(min_initial_location_factors) > 1) {
      count_min_factor <- length(which(min_initial_location_factors > 1))
      min_initial_location_factor <- max(min_initial_location_factors) +
        ((count_min_factor - 1) * inc_constant)
    } else {
      min_initial_location_factor <- min(min_initial_location_factors)
    }
    location_factor_asset <- 0.25 *
      (initial_location_factor - min_initial_location_factor) +
      min_initial_location_factor
  }

}



  return(location_factor_asset)
}
