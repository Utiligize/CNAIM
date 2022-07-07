#' @title Calculates risk and converts to matrix coordinates
#' @description This function calculates monetary risk, given probability of
#' failure and consequence of failure inputs, as well as the desired risk matrix
#' dimensions.
#' @param matrix_dimensions A data frame with the dimensions of the desired risk
#' matrix
#' @param id A string that describes the asset
#' @param pof The probability of failure of the asset
#' @param cof The consequences of failure of the asset
#' @param asset_type The asset type to be calculated for class
#' @param pof_limit Specific limit for the risk matrix
#' @param cof_limit Specific limit for the risk matrix
#' @param asset_type The asset type to be calculated for class
#' @export
#'
risk_calculation <- function(matrix_dimensions,id,pof,cof,asset_type, pof_limits, cof_limits){

  `Asset Register Category` = `Total - (GBP)` = NULL
  # due to NSE notes in R CMD check

  reference_cof <- gb_ref$reference_costs_of_failure %>%
    dplyr::filter(`Asset Register Category` == asset_type) %>%
    dplyr::select(`Total - (GBP)`) %>%
    dplyr::pull()

  for (i in 1:length(pof_limits)) {

    if(pof < pof_limits[i] && i==1) {
      pof_pct <- ((pof - 0.5) / (pof_limits[i]-0.5)) * (100/length(pof_limits))
      break

    } else if (pof < pof_limits[i] ) {
      pof_pct <- ((pof - pof_limits[i-1]) / (pof_limits[i]-pof_limits[i-1])) * (100/length(pof_limits)) + (100/length(pof_limits))*(i-1)
      break

    } else if (pof > 15)  {
      pof_pct <- 100
    }

  }

  for (i in 1:length(cof_limits)) {

    if(cof < cof_limits[i] && i==1) {
      cof_pct <- ((cof - 0) / (cof_limits[i]- 0)) * (100/length(cof_limits))
      break

    } else if (cof < cof_limits[i] ) {
      cof_pct <- ((cof - cof_limits[i-1]) / (cof_limits[i]-cof_limits[i-1])) * (100/length(cof_limits)) + (100/length(cof_limits))*(i-1)
      break

    } else if (cof > 250)  {
      cof_pct <- 100
    }

  }

  # pof_pct <- pof # Criteria for being changed
  # Upper band is cof*4

  # if (pof_pct > 15) pof_pct <- 15
  # if (cof_pct > 250) cof_pct <- 250

  dots_vector = data.frame(id = id,
                           point_x = pof_pct,
                           point_y = cof_pct)


  return(dots_vector)
}

