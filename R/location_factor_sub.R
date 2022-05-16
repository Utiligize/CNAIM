#' @title Location Factor (Excl.Submarine Cables)
#' @description This function calculates the location factor for
#' submarine cables based in the specific location of the cable.
#' See section 6.5 on page 44 in CNAIM (2017). For calculating the location
#' factor for all other network assets please see the function
#' \code{\link{location_factor}()}.
#' @param topography String. Describe the topography around the submarine cable.
#' Options:
#' \code{typography = c("Low Detrimental Topography",
#' "Medium Detrimental Topography", "High Detrimental Topography",
#' "Very High Detrimental Topography","Default" )}
#' @param sitution String. Descibes how the submarine cable af fixed to the
#' sea floor.
#' Options:
#' \code{sitution=c("Laid on bed", "Covered", "Buried", "Default")}
#' @param wind_wave Numeric.
#' Options:
#' \code{wind_wave=c(1, 2, 3, "Default")}.
#' Settings:
#' \itemize{
#' \item{\code{wind_wave = 1}:} Sheltered sea loch, Wind <200 W/m2
#' \item{\code{wind_wave = 2}:} Wave <15kW/m, Wind 200-800 W/m2
#' \item{\code{wind_wave = 3}:} Wave <15kW/m, Wind 200-800 W/m2
#' \item{\code{wind_wave = "Default"}:} No data available
#' }
#' @param intensity String. Combined wave and current energy factor.
#' Options:
#' \code{intensity=c("Low", "Moderate", "High", "Default")}.
#' @param landlocked String. Options: \code{landlocked = c("yes","no")}. Default
#' setting for \code{landlocked = "no"}.
#' @return Numeric. Location factor
#' @source DNO Common Network Asset Indices Methodology (CNAIM),
#' Health & Criticality - Version 1.1, 2017:
#'\url{https://www.ofgem.gov.uk/sites/default/files/docs/2021/04/dno_common_network_asset_indices_methodology_v2.1_final_01-04-2021.pdf}
#' @export
#' @examples
#'  # Location factor for a non-landlocked submarine cable
#'location_factor_sub(topography = "Default",
#'                    sitution = "Default",
#'                    wind_wave = "Default",
#'                    intensity = "Default",
#'                    landlocked = "no")


location_factor_sub <- function(topography = "Default",
                                sitution = "Default",
                                wind_wave = "Default",
                                intensity = "Default",
                                landlocked = "no") {

  Topography = Situation = Rating = Intensity = NULL
  # Typography
  submarin_cable_topog_factor <- gb_ref$submarin_cable_topog_factor %>%
    dplyr::filter(Topography == topography)

  sea_score <- submarin_cable_topog_factor$`Score (Sea)`
  land_score <- submarin_cable_topog_factor$`Score (Land locked)`


  # Sitution
  submarin_cable_sitution_factor <- gb_ref$submarin_cable_sitution_factor %>%
    dplyr::filter(Situation == sitution)

  sit_score <- submarin_cable_sitution_factor$Score


  # Wave/wind

  if(wind_wave == "Default") {
    submarin_cable_wind_wave <- gb_ref$submarin_cable_wind_wave %>%
      dplyr::filter(is.na(Rating))

  } else {
    submarin_cable_wind_wave <- gb_ref$submarin_cable_wind_wave %>%
      dplyr::filter(Rating == wind_wave)
  }


  ww_score <- submarin_cable_wind_wave$Score


  # Combined energy factor
  combined_wave_ct_energy_factor <- gb_ref$combined_wave_ct_energy_factor %>%
    dplyr::filter(Intensity == intensity)


  comb_sea_score <- combined_wave_ct_energy_factor$`Scoring (Sea)`
  comb_land_score <- combined_wave_ct_energy_factor$`Scoring (Landlocked)`


  # INC
  inc_sub <- gb_ref$increment_constants$`Submarine Cables`

  # Submarine Cable Route Topography Factor, Situation Factor, Wind/Wave
  # Factor, Combined Wave & Current Energy Factor is greater than 1

  if (landlocked == "yes") {

    typ_score <- land_score
    comb_score <- comb_land_score

  } else {
    typ_score <- sea_score
    comb_score <- comb_sea_score


  }

  thresshold <- max(typ_score, sit_score, ww_score, comb_score)


  if (thresshold > 1) {

    count_f <- sum(c(typ_score, sit_score, ww_score, comb_score) > 1)

    location_factor_asset <-
      thresshold + ((count_f - 1) * inc_sub)

  } else {

    min_factors <- min(typ_score, sit_score, ww_score, comb_score)
    location_factor_asset <- min_factors

  }

  return(location_factor_asset)
}
