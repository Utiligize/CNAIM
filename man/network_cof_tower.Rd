% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/cof_towers.R
\name{network_cof_tower}
\alias{network_cof_tower}
\title{Network cost of Failure for Towers}
\source{
DNO Common Network Asset Indices Methodology (CNAIM),
Health & Criticality - Version 2.1, 2021:
\url{https://www.ofgem.gov.uk/sites/default/files/docs/2021/04/dno_common_network_asset_indices_methodology_v2.1_final_01-04-2021.pdf}
}
\usage{
network_cof_tower(
  tower_asset_category,
  actual_load_mva,
  secure = T,
  gb_ref_given = NULL
)
}
\arguments{
\item{tower_asset_category}{String The type of tower asset category
Options: \code{tower_asset_category = c("33kV Tower", "66kV Tower",
"132kV Tower")}.}

\item{actual_load_mva}{Numeric. The actual load on the asset}

\item{secure}{Boolean If the asset is in a secure network or not}

\item{gb_ref_given}{optional parameter to use custom reference values}
}
\value{
Numeric. Network cost of failure.
}
\description{
This function calculates network cost of failure for
all asset categories exclusive the assets EHV and 132kV transformers.
(cf. section 7.6, page 87, CNAIM, 2021). Network cost of failure
is used in the derivation of consequences of failure see \code{\link{cof}}().
}
\examples{
network_cof_tower(tower_asset_category = "33kV Tower",
actual_load_mva = 15)
}
