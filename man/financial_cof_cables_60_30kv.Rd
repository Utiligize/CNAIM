% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/cof_cables_60_30kv.R
\name{financial_cof_cables_60_30kv}
\alias{financial_cof_cables_60_30kv}
\title{Financial cost of Failure for 30-60 kV UG cables}
\source{
DNO Common Network Asset Indices Methodology (CNAIM),
Health & Criticality - Version 2.1, 2021:
\url{https://www.ofgem.gov.uk/sites/default/files/docs/2021/04/dno_common_network_asset_indices_methodology_v2.1_final_01-04-2021.pdf}
}
\usage{
financial_cof_cables_60_30kv(ehv_asset_category, gb_ref_given = NULL)
}
\arguments{
\item{ehv_asset_category}{Asset category for the analysis}

\item{gb_ref_given}{optional parameter to use custom reference values}
}
\value{
Numeric. Financial consequences of failure for EHV switchgear
}
\description{
This function calculates financial consequences of failure
(cf. section 7.3, page 79, CNAIM, 2021). Financial consequences
of failure is used in
the derivation of consequences of failure see \code{\link{cof}}().
\code{ehv_asset_category = c("30kV UG Cable (Gas)", "60kV UG Cable (Gas)",
"30kV UG Cable (Non Pressurised)", "60kV UG Cable (Non Pressurised)",
"30kV UG Cable (Oil)", "60kV UG Cable (Oil)")
}. The default setting is
\code{ehv_asset_category = "60kV UG Cable (Gas)"}.
}
\examples{
financial_cof_cables_60_30kv(ehv_asset_category = "30kV UG Cable (Oil)")
}
