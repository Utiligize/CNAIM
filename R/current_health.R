#' @title Current Health score
#' @description This function calculates the current health score for a
#' given electric network asset (cf. CNAIM, 2017. Page 21, section 4.3.2).
#' @param initial_health_score Numeric. The output from the function
#'  \code{\link{initial_health}}().
#' @param health_score_factor Numeric. E.g. output from the function
#' \code{\link{health_score_excl_ehv_132kv_tf}}().
#' @param health_score_cap Numeric. Specifies the maximum value of current
#' health score. The cap is used in situations where a good result from a
#' condition inspection or measurement implies that the health score should
#' be no more than the specified value. The cap is derived as the minimum
#' of the observed condition cap and the measured condition cap.
#' Measured and observed condition caps are found in lookup tables depending
#' in the asset category, when determine the observed and measured
#' condition factors. A setting of \code{"Default"} sets the
#' \code{health_score_cap} to 10.
#' @param health_score_collar Numeric. Specifies the minimum value of
#' Current Health Score.
#' The collar is used in situations where a poor result from a condition
#' inspection or measurement implies that the health score should be at
#' least the specified value.
#' The collar is derived as the minimum of the observed condition collar
#' and the measured condition collar. Measured and observed condition
#' collars are found in lookup tables depending in the asset category,
#' when determine the observed and measured condition factors.
#' A setting of \code{"Default"} sets the \code{health_score_collar} to 0.5.
#' @param reliability_factor Numeric. \code{reliability_factor}
#' shall have a value between 0.6 and 1.5. A setting of \code{"Default"}
#'  sets the \code{reliability_factor} to 1.
#' See section 6.14 on page 69 in CNAIM (2017).
#' @return Numeric. The Current health score.
#' @source DNO Common Network Asset Indices Methodology (CNAIM),
#' Health & Criticality - Version 1.1, 2017:
#'\url{https://www.ofgem.gov.uk/system/files/docs/2017/05/dno_common_network_asset_indices_methodology_v1.1.pdf}
#' @export
#' @examples
#'current_health(initial_health_score = 0.5,
#'               health_score_factor = 0.33,
#'               health_score_cap = 10,
#'               health_score_collar = 0.5,
#'               reliability_factor = 1)


current_health <- function(initial_health_score,
                           health_score_factor,
                           health_score_cap = "Default",
                           health_score_collar = "Default",
                           reliability_factor = "Default") {

  if (health_score_cap == "Default") health_score_cap <- 10
  if (health_score_collar == "Default") health_score_collar <- 0.5
  if (reliability_factor == "Default") reliability_factor <- 1
  reliability_factor <- as.numeric(reliability_factor)

  # Eq. 5
  current_health_score <- (initial_health_score * health_score_factor *
                           reliability_factor)

  # Eq. 6
  current_health_score <- pmin(current_health_score, health_score_cap)

  # Eq. 7
  collar <- max(health_score_collar, reliability_factor)
  current_health_score <- pmax(current_health_score, collar)
  return(current_health_score)
}
