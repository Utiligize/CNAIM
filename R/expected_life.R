#' @title Expected Life
#' @description This function calculates the expected life of
#' an electric network asset measured in years
#' when it would be expected to first observe significant deterioration.
#' The expected life is derived based on the assets normal expected life,
#'  duty factor and location factor.
#' See section 6.1.4 on page 32 in CNAIM (2017).
#' @param normal_expected_life Numeric. The number of years a new asset is
#'  expected to normally last. I.e. technical life time.
#'  See page 103, table 20 in CNAIM (2017).
#' @param duty_factor Numeric. E.g. the output returned by the function
#'  \code{\link{duty_factor_transformer_11_20kv}()}.
#' @param location_factor Numeric. The output returned by the function
#'  \code{\link{location_factor}()}.
#' @return Numeric. Expected life.
#' @source DNO Common Network Asset Indices Methodology (CNAIM),
#' Health & Criticality - Version 1.1, 2017:
#'\url{https://www.ofgem.gov.uk/sites/default/files/docs/2021/04/dno_common_network_asset_indices_methodology_v2.1_final_01-04-2021.pdf}
#' @export
#' @examples
#' # An asset e.g. a transformer with an expcted life of 50 years
#' expected_life(normal_expected_life = 50,
#'               duty_factor = 1,
#'               location_factor = 1)
#'

expected_life <- function(normal_expected_life,
                          duty_factor,
                          location_factor) {
  return(normal_expected_life / (duty_factor * location_factor))
}
