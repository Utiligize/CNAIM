% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/cof_sub_cables.R
\name{network_cof_ehv_sub_cable}
\alias{network_cof_ehv_sub_cable}
\title{Network cost of Failure for EHV /132 kV sub cables}
\source{
DNO Common Network Asset Indices Methodology (CNAIM),
Health & Criticality - Version 1.1, 2017:
\url{https://www.ofgem.gov.uk/system/files/docs/2017/05/dno_common_network_asset_indices_methodology_v1.1.pdf}
}
\usage{
network_cof_ehv_sub_cable(
  sub_cable_asset_category,
  actual_load_mva,
  secure = T
)
}
\arguments{
\item{sub_cable_asset_category}{String The type of sub cable asset category}

\item{actual_load_mva}{Numeric. The actual load on the asset}

\item{secure}{Boolean If the asset is in a secure network or not}
}
\value{
Numeric. Network cost of failure.
}
\description{
This function calculates network cost of failure for
all asset categories exclusive the assets EHV and 132kV transformers.
(cf. section 7.6, page 83, CNAIM, 2017). Network cost of failure
is used in the derivation of consequences of failure see \code{\link{cof}}().
}
\examples{
network_cof_ehv_sub_cable(sub_cable_asset_category = "EHV Sub Cable",
actual_load_mva = 15, secure = TRUE)
}
