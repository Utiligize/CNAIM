#' Failure statistics dataset for 10,000 6.6/11kV transformers
#'
#' A dataset containing failure statistics for 10,000 6.6/11kV transformers
#' from the CNAIM standard, simulated over 100 years. The variables are as
#' follows:
#'
#' @format A data frame with 103,848 rows and 13 variables:
#' \describe{
#'   \item{utilisation_pct}{Utilization of a transformer in \%}
#'   \item{placement}{Is the transformer placed indoors or outdoors?}
#'   \item{altitude_m}{Altitude above sea level (m)}
#'   \item{distance_from_coast_km}{Distance from salt water (km)}
#'   \item{corrosion_category_index}{Corrosion zone the asset exists in}
#'   \item{partial_discharge}{Condition converted from TEV \%-measurement}
#'   \item{oil_acidity}{Oil acidity (mg KOH/g)}
#'   \item{temperature_reading}{Temperature condition band}
#'   \item{observed_condition}{Observed condition band}
#'   \item{age}{Age of transformer (years)}
#'   \item{pof}{Probability of failure (current and future) when the transformer failed}
#'   \item{transformer_id}{Id of transformer that died}
#'   \item{dead}{Monte carlo result showing if the transformer has died (TRUE)}
#' }
#' @source \url{http://www.cnaim.io/}
"transformer_11kv_faults"
