% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/cof_transformer_30_60kv.R
\name{network_cof_transformer_30_60kv}
\alias{network_cof_transformer_30_60kv}
\title{Network cost of Failure for 30/10kV and 60/10kV Transformers}
\usage{
network_cof_transformer_30_60kv(
  tf_asset_category,
  actual_load_mva,
  secure = T,
  gb_ref_given = NULL
)
}
\arguments{
\item{tf_asset_category}{String The type of Tower
Options: \code{tf_asset_category = c("30kV Transformer (GM)",
"60kV Transformer (GM)")}.}

\item{actual_load_mva}{Numeric. The actual load on the asset}

\item{secure}{Boolean If the asset is in a secure network or not}

\item{gb_ref_given}{optional parameter to use custom reference values}
}
\value{
Numeric. Network cost of failure.
}
\description{
This function calculates network cost of failure for
Outputted in DKK.
}
\examples{
network_cof_transformer_30_60kv(tf_asset_category = "30kV Transformer (GM)",
actual_load_mva = 15)
}
