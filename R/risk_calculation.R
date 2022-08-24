#' @title Calculates risk and converts to matrix coordinates
#' @description This function calculates risk matrix coordinates
#' dimensions.
#' @param matrix_dimensions A data frame with the dimensions of the desired risk
#' matrix.
#' @param id An integer that identifies the asset
#' @param chs The Current Health Score (CHS) of the asset
#' @param cof The Consequence of Failure of the asset
#' @param asset_type The asset type to be calculated for class
#' @param hi_bands Specific Health Index (HI) bands for risk matrix. Default
#' values are the same as defined in the CNAIM v2.1 standard
#' @param ci_bands Specific Criticality Index (CI) bands for the risk matrix.
#' Default values are the same as defined in the CNAIM v.2.1 standard.
#' @param asset_type The asset type to be calculated for class
#' @source DNO Common Network Asset Indices Methodology (CNAIM),
#' Health & Criticality - Version 2.1, 2021:
#' \url{https://www.ofgem.gov.uk/sites/default/files/docs/2021/04/dno_common_network_asset_indices_methodology_v2.1_final_01-04-2021.pdf}
#' @export
#' @examples
#' # Calculate risk matrix coordinates for an asset
#' # 1. Make the risk matrix structure
#' matrix_structure <- risk_matrix_structure(5,4,NA)
#'
#' # 2. Calculate risk matrix coordinates
#' risk_calculation(matrix_dimensions = matrix_structure,
#' id = 1,
#' chs = 4,
#' cof = 15000,
#' asset_type = "6.6/11kV Transformer (GM)")
#'
#'

risk_calculation <- function(matrix_dimensions,id,chs,cof,asset_type,
                             hi_bands = NULL, ci_bands = NULL){

  `Asset Register Category` = `Total - (GBP)` = NULL
  # due to NSE notes in R CMD check

  reference_cof <- CNAIM:::gb_ref$reference_costs_of_failure %>%
    dplyr::filter(`Asset Register Category` == asset_type) %>%
    dplyr::select(`Total - (GBP)`) %>%
    dplyr::pull()

  if (is.null(hi_bands)){hi_bands <- c(3, 5, 5.5, 6.5, 8, 15)}
  if (is.null(ci_bands)){ci_bands <- c(75, 125, 200, Inf)}
  ci <- (cof/reference_cof)*100

  for (i in 1:length(hi_bands)) {

    if(chs < hi_bands[i] && i==1) {
      pof_pct <- ((chs - 0.5) / (hi_bands[i]-0.5)) * (100/length(hi_bands))
      break

    } else if (chs < hi_bands[i] ) {
      pof_pct <- ((chs - hi_bands[i-1]) / (hi_bands[i]-hi_bands[i-1])) *
        (100/length(hi_bands)) + (100/length(hi_bands))*(i-1)
      break

    } else if (chs > 15)  {
      pof_pct <- 100
    }

  }



  for (i in 1:length(ci_bands)) {

    if(ci < ci_bands[i] && i==1) {
      cof_pct <- ((ci - 0) / (ci_bands[i]- 0)) * (100/length(ci_bands))
      break

    } else if (ci < ci_bands[i] ) {
      cof_pct <- ((ci - ci_bands[i-1]) / (ci_bands[i]-ci_bands[i-1])) *
        (100/length(ci_bands)) + (100/length(ci_bands))*(i-1)
      break

    } else if (ci > 250)  {
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

