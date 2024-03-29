% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/cof_poles_ohl_support_50kv.R
\name{financial_cof_poles_ohl_support_50kv}
\alias{financial_cof_poles_ohl_support_50kv}
\title{Financial cost of Failure for Poles OHL Support 50kV}
\usage{
financial_cof_poles_ohl_support_50kv(
  pole_asset_category,
  type_financial_factor_criteria,
  access_factor_criteria,
  gb_ref_given = NULL
)
}
\arguments{
\item{pole_asset_category}{String The type of Pole asset category}

\item{type_financial_factor_criteria}{String. Type Financial factor criteria for Pole
Options: \code{type_financial_factor_criteria = c("Pole (supporting conductor only)",
"Pole (supporting plant or equipment)", "Small footprint steel masts")}.}

\item{access_factor_criteria}{String. Asses Financial factor criteria for Pole
setting.
Options: \code{access_factor_criteria = c("Type A", "Type B")}.}

\item{gb_ref_given}{optional parameter to use custom reference values}
}
\value{
Numeric. Financial consequences of failure for Poles
}
\description{
This function calculates financial consequences of failure
Financial consequences of failure is used in
the derivation of consequences of failure see \code{\link{cof}}().
Outputted in DKK.
}
\examples{
financial_cof_poles_ohl_support_50kv(
type_financial_factor_criteria = "Small footprint steel masts",
access_factor_criteria = "Type A")
}
