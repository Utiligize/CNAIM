#' @title Maximum and Multiple Increment (MMI) Technique
#' @description This function returns a combined factor using a maximum and
#'  multiple increment (MMI) technique
#'  (cf. CNAIM, 2017. page 50, section 6.7.2). The function can be used
#' to derive the health score factor for EHV and 132kV transformers.
#' For deriviation of the health score factor for all other assets see
#' \code{\link{health_score_excl_ehv_132kv_tf}}.
#' To derive the health score factor for EHV and 132kV transformers one needs
#'  to use \code{mmi}() to derive the health score factor for
#'  the main transformer and for the tapchanger respectively.
#'  The constants \code{factor_divider_1}, \code{factor_divider_2}
#'   and \code{max_no_combined_factors} are all available in the lookup
#'    table 10 and 11 on page 53 and 54 in CNAIM (2017).
#' For an in deph desciption see also section 6.8 on page 53 in CNAIM (2017).
#' The \code{mmi}() can also be used in the derivation of observed and measured
#'  condition factors for all assets, using measured and observed
#'  input factors.
#' The constants \code{factor_divider_1}, \code{factor_divider_2} and
#' \code{max_no_combined_factors} can be found in table 13 on page 59
#' (observed condition factors) and in table 15 on page 63
#' (measured condition factors).
#' @param factors Numeric vector. Factors to me combined.
#' @param factor_divider_1 Numeric. Constant that specifies the degree
#'  to which additional “good” or “bad” factors are able further drive
#'   the combined factor.
#' @param factor_divider_2 Numeric. Constant that specifies the degree
#'  to which additional “good” or “bad” factors are able further drive
#'   the combined factor.
#' @param max_no_combined_factors Numeric. Specifies how many factors
#'  are able to simultaneously affect the combined factor.
#' @return Numeric. Combined factor.
#' @source DNO Common Network Asset Indices Methodology (CNAIM),
#'  Health & Criticality - Version 1.1, 2017:
#'\url{https://www.ofgem.gov.uk/sites/default/files/docs/2021/04/dno_common_network_asset_indices_methodology_v2.1_final_01-04-2021.pdf}
#' @export
#' @examples
#' mmi(factors = c(1,
#'1.5),
#'factor_divider_1 = 1.5,
#'factor_divider_2 = 1.5,
#'max_no_combined_factors = 1)

mmi <- function(factors,
                factor_divider_1,
                factor_divider_2,
                max_no_combined_factors) {

  if (any(factors > 1)) {
    factors_sorted <- sort(factors, decreasing = T)
    var_1 <- factors_sorted[1]

    remaining_factors <- factors_sorted[-1]
    remaining_factors <- remaining_factors - 1
    remaining_factors <- remaining_factors[which(remaining_factors > 0)]
    cumsum_remaining_factors <- cumsum(remaining_factors)

    if (max_no_combined_factors - 1 < 1) {
      var_2 <- 0
    } else if (length(cumsum_remaining_factors) == 0) {
      var_2 <- 0
    } else {
      var_2 <- cumsum_remaining_factors[max_no_combined_factors - 1]
      if (is.na(var_2)) {
        var_2 <- max(cumsum_remaining_factors)
      }
    }
    var_3 <- var_2 / factor_divider_1
  } else {
    factors_sorted <- sort(factors)
    var_1 <- factors_sorted[1]
    var_2 <- dplyr::if_else(is.na(factors_sorted[2]), 0, factors_sorted[2])
    var_3 <- (var_2 - 1) / factor_divider_2
  }

  # combined factor
  return(var_1 + var_3)
}
