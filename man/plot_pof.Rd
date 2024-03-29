% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/plotplot.R
\name{plot_pof}
\alias{plot_pof}
\title{Plot of probability of failure}
\usage{
plot_pof(pof_function = "Default")
}
\arguments{
\item{pof_function}{String. Choosing an pof function,  Options:
\code{pof_function = c(pof_cables_04kv_pex, pof_cables_10kv_pex, pof_cables_10kv_oil,
pof_cables_60_30kv, pof_ohl_cond_50kv, pof_submarine_cables_10kv_oil,
pof_submarine_cables_10kv_pex, pof_submarine_cables_30_60kv_oil,
pof_submarine_cables_30_60kv_pex, pof_transformer_04_10kv, pof_building,
pof_serviceline, "Default")}.}
}
\description{
This function is plotting the probability of failure for an
electric network asset in a percentage.
}
\examples{
# probability of failure curve
}
