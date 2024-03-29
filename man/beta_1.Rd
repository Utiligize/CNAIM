% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/beta_1.R
\name{beta_1}
\alias{beta_1}
\title{Initial Ageing Rate}
\source{
DNO Common Network Asset Indices Methodology (CNAIM),
Health & Criticality - Version 2.1, 2021:
\url{https://www.ofgem.gov.uk/sites/default/files/docs/2021/04/dno_common_network_asset_indices_methodology_v2.1_final_01-04-2021.pdf}
}
\usage{
beta_1(expected_life_years)
}
\arguments{
\item{expected_life_years}{Numeric. The output returned by the
function \code{\link{expected_life}}().}
}
\value{
Numeric. Initial ageing rate for an electric network asset.
}
\description{
This function calculates the initial ageing rate for an
electric network asset. See section 6.1.5 on page 36 in CNAIM (2021).
}
\examples{
beta_1(expected_life_years = 10)

}
