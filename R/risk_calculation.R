#' @title Calculates risk and converts to matrix coordinates
#' @description This function calculates monetary risk, given probability of
#' failure and consequence of failure inputs, as well as the desired risk matrix
#' dimensions.
#' @param matrix_dimensions A data frame with the dimensions of the desired risk
#' matrix
#' @param id A string that describes the asset
#' @param pof The probability of failure of the asset
#' @param cof The consequences of failure of the asset
#' @param asset_type The asset type to be calculated for
#' class
#' @export
#'
risk_calculation <- function(matrix_dimensions,id,pof,cof,asset_type){
  reference_cof <- gb_ref$reference_costs_of_failure %>%
    dplyr::filter(`Asset Register Category` == asset_type) %>%
    dplyr::select(`Total - (GBP)`) %>%
    dplyr::pull()

  pof_pct <- 100*pof/0.4 # Criteria for being changed
  cof_pct <- 100*cof/(reference_cof*4) # Upper band is cof*4

  if (pof_pct > 100) pof_pct <- 100
  if (cof_pct > 100) cof_pct <- 100

  dots_vector = data.frame(id = 1:length(pof_pct),
                           point_x = pof_pct,
                           point_y = cof_pct)

  return(dots_vector)
}
