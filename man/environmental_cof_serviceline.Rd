% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/cof_serviceline.R
\name{environmental_cof_serviceline}
\alias{environmental_cof_serviceline}
\title{Environmental cost of Failure for Service Lines}
\usage{
environmental_cof_serviceline(prox_water, bunded, gb_ref_given = NULL)
}
\arguments{
\item{prox_water}{Numeric. Specify the proximity to a water course in meters.
A setting of \code{"Default"} will result in a proximity factor of 1. Thus
assume the proximity to a water course is between 80m and 120m}

\item{bunded}{String. Options: \code{bunded = c("Yes", "No", "Default")}.
A setting of \code{"Default"} will result in a bunding factor of 1.}

\item{gb_ref_given}{optional parameter to use custom reference values}
}
\description{
This function calculates environmental consequences of failure
Outputted in DKK
}
\examples{
environmental_cof_serviceline(prox_water = 95, bunded = "Yes")
}
