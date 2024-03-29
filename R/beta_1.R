#' @title Initial Ageing Rate
#' @description This function calculates the initial ageing rate for an
#' electric network asset. See section 6.1.5 on page 36 in CNAIM (2021).
#' @param expected_life_years Numeric. The output returned by the
#' function \code{\link{expected_life}}().
#' @return Numeric. Initial ageing rate for an electric network asset.
#' @source DNO Common Network Asset Indices Methodology (CNAIM),
#' Health & Criticality - Version 2.1, 2021:
#'\url{https://www.ofgem.gov.uk/sites/default/files/docs/2021/04/dno_common_network_asset_indices_methodology_v2.1_final_01-04-2021.pdf}
#' @export
#' @examples
#'beta_1(expected_life_years = 10)
#'

beta_1 <- function(expected_life_years) {
  # the Health Score of a new asset
  h_new <- 0.5
  # the Health Score of the asset when it reaches its Expected Life
  h_expected_life <- 5.5
  return(log(h_expected_life / h_new) / expected_life_years)
}
