% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/cof_transformer_11kv.R
\name{cof_transformer_11kv}
\alias{cof_transformer_11kv}
\title{Consequences of Failure for a 6.6/11 kV transformer}
\source{
DNO Common Network Asset Indices Methodology (CNAIM),
Health & Criticality - Version 1.1, 2017:
\url{https://www.ofgem.gov.uk/system/files/docs/2017/05/dno_common_network_asset_indices_methodology_v1.1.pdf}
}
\usage{
cof_transformer_11kv(
  kva,
  type,
  type_risk,
  location_risk,
  prox_water,
  bunded,
  no_customers,
  kva_per_customer
)
}
\arguments{
\item{kva}{Numeric. The rated transformer capacity measured in kVA
for a 6.6/11 kV transformer. Rated capacity is used to derive the
type financial factor. For a general description of type financial factor see
section 7.3.3.1 on page 76 in CNAIM (2017). A setting of \code{"Default"}
will result in a type financial factor equal to 1
(cf. section D1.2.1, page 162, CNAIM, 2017).}

\item{type}{String. Relates to the accessibility of the transformer
Options: \code{type = c("Type A", "Type B", "Type C", "Default")}.
A setting of \code{"Type A"} - Normal access.
A setting of \code{"Type B"} - Constrained access or confined working space.
A setting of \code{"Type C"} - Underground substation.
A setting of \code{"Default"} - Normal access thus same as \code{"Type A"}
setting (cf. table 214, page 164, CNAIM, 2017).}

\item{type_risk}{String. Risk that the asset presents to the
public by its characteristics and particular situation. Options:
\code{type_risk = c("Low", "Medium", "High", "Default")}
(cf. table 218, page 168, CNAIM, 2017).
A setting of \code{"Default"} equals a setting of \code{"Medium"}.}

\item{location_risk}{String. Proximity to areas that may affect its
likelihood of trespass or interference. Options:
\code{location_risk = c("Low", "Medium", "High", "Default")}
(cf. table 218, page 168, CNAIM, 2017).
A setting of \code{"Default"} equals a setting of \code{"Medium"}.}

\item{prox_water}{Numeric. Specify the proximity to a water course in meters.
A setting of \code{"Default"} will result in a proximity factor of 1. Thus
assume the proximity to a water course is between 80m and 120m
(cf. table 223, page 172, CNAIM, 2017).}

\item{bunded}{String. Options: \code{bunded = c("Yes", "No", "Default")}.
A setting of \code{"Default"} will result in a bunding factor of 1.}

\item{no_customers}{Numeric. The numner of customers
fed by an individual asset.}

\item{kva_per_customer}{Numeric. If the asset have an exceptionally high
demand per customer type in kVA per customer. A setting of \code{"Default"}
results in a multiplication factor of 1 (cf. table 18, page 86, CNAIM, 2017).}
}
\value{
Numeric. Consequences of failure for a 6.6/11 kV transformer.
}
\description{
This function calculates consequences of failure
for a 6.6/11 kV transformer (cf.section 7, page 71, CNAIM, 2017).
}
\examples{
# Consequences of failure for a 6.6/11 kV transformer
cof_transformer_11kv(kva = 500, type = "Type C",
                     type_risk = "High", location_risk = "High",
                     prox_water = 50, bunded = "No",
                     no_customers = 500, kva_per_customer = 1)
}
