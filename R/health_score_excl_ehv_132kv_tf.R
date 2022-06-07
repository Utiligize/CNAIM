#' @title Health Score Factor for all Assets Categories excl.
#' EHV and 132kV Transformers
#' @description This function calculates the health score
#' factor for all asset categories exclusive the assets EHV
#' and 132kV transformers. For EHV and 132kV transformers see
#' \code{\link{mmi}}(). The function combines observed and measured
#' condition factors using the simplified maximum and
#' multiple increment (MMI) technique to
#' construct the health score factor (cf. CNAIM, 2021, page 56, table 9).
#' @param observed_condition_factor Numeric.
#' @param measured_condition_factor Numeric.
#' @return Numeric. Health score factor.
#' @source DNO Common Network Asset Indices Methodology (CNAIM),
#' Health & Criticality - Version 2.1, 2021:
#'\url{https://www.ofgem.gov.uk/sites/default/files/docs/2021/04/dno_common_network_asset_indices_methodology_v2.1_final_01-04-2021.pdf}
#' @export
#' @examples
#' # An asset with an observed condition factor of 1 and a measured condition
#' # factor of 0.33
#'  health_score_excl_ehv_132kv_tf(observed_condition_factor = 1,
#'  measured_condition_factor = 0.33)


health_score_excl_ehv_132kv_tf <- function(observed_condition_factor,
                                           measured_condition_factor) {
  factor_divider <- 1.5
  factors <- c(observed_condition_factor, measured_condition_factor)
  a <- max(factors)
  b <- min(factors)

  if (a > 1 && b > 1) {
    return(a + ((b - 1) / factor_divider))
  } else if (a > 1 && b <= 1) {
    return(a)
  } else {
    return(b + ((a - 1) / factor_divider))
  }
}
