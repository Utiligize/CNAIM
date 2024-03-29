% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/pof_future_transformer_04_10kv.R
\name{pof_future_transformer_04_10kv}
\alias{pof_future_transformer_04_10kv}
\title{Future Probability of Failure for 0.4/10kV Transformers}
\usage{
pof_future_transformer_04_10kv(
  utilisation_pct = "Default",
  placement = "Default",
  altitude_m = "Default",
  distance_from_coast_km = "Default",
  corrosion_category_index = "Default",
  age,
  partial_discharge = "Default",
  temperature_reading = "Default",
  observed_condition = "Default",
  reliability_factor = "Default",
  moisture = "Default",
  acidity = "Default",
  bd_strength = "Default",
  k_value = 0.0077,
  c_value = 1.087,
  normal_expected_life = 55,
  simulation_end_year = 100,
  gb_ref_given = NULL
)
}
\arguments{
\item{utilisation_pct}{Numeric. The max percentage of utilisation
under normal operating conditions.}

\item{placement}{String. Specify if the asset is located outdoor or indoor.
A setting of \code{"Outdoor"} means the asset is
located in an outside environment,
and a setting of \code{"Indoor"} means the asset is located in an
 indoor environment. A setting of \code{"Default"} will result
  in either an indoor or an outdoor environment setting that depends
  on the specification of \code{asset_type}. See page 110-113,
  table 26 in CNAIM (2021) for default environments.}

\item{altitude_m}{Numeric. Specify the altitude location for
the asset measured in meters from sea level.\code{altitude_m}
is used to derive the altitude factor. See page 111,
table 23 in CNAIM (2021). A setting of \code{"Default"}
will set the altitude factor to 1 independent of \code{asset_type}.}

\item{distance_from_coast_km}{Numeric. Specify the distance from the
coast measured in kilometers. \code{distance_from_coast_km} is used
to derive the distance from coast factor See page 110,
table 22 in CNAIM (2021). A setting of \code{"Default"} will set the
 distance from coast factor to 1 independent of \code{asset_type}.}

\item{corrosion_category_index}{Integer.
Specify the corrosion index category, 1-5.}

\item{age}{Numeric. The current age in years.}

\item{partial_discharge}{String. Indicating the}

\item{temperature_reading}{String. Indicating the criticality.
Options for \code{temperature_reading}:
\code{temperature_reading = c("Normal", "Moderately High",
"Very High", "Default")}. See page 153, table 172 in CNAIM (2021).}

\item{observed_condition}{String. Indicating the observed condition of the
 transformer. Options for \code{observed_condition}:
\code{observed_condition = c("No deterioration", "Superficial/minor deterioration", "Slight deterioration",
 "Some Deterioration", "Substantial Deterioration", "Default")}. See page 130, table 81 in CNAIM (2021).}

\item{reliability_factor}{Numeric. \code{reliability_factor}
shall have a value between 0.6 and 1.5. A setting of \code{"Default"}
 sets the \code{reliability_factor} to 1.
See section 6.14 on page 73 in CNAIM (2021).}

\item{moisture}{Numeric. the amount of moisture given in (ppm) See page 162, table 203 in CNAIM (2021).}

\item{acidity}{String Acidity}

\item{bd_strength}{Numeric. the amount of breakdown strength given in (kV) See page 162, table 205 in CNAIM (2021).}

\item{k_value}{Numeric. \code{k_value = 0.0069} by default. This number is
given in a percentage. The default value is accordingly to the CNAIM standard
on p. 110.}

\item{c_value}{Numeric. \code{c_value = 1.087} by default.
The default value is accordingly to the CNAIM standard see page 110}

\item{normal_expected_life}{Numeric. \code{normal_expected_life = 60} by default.
The default value is accordingly to the CNAIM standard on page 107.}

\item{simulation_end_year}{Numeric. The last year of simulating probability
of failure. Default is 100.}

\item{gb_ref_given}{optional parameter to use custom reference values}
}
\value{
DataFrame. Future probability of failure
along with future health score
}
\description{
This function calculates the future
annual probability of failure for 0.4/10kV Transformers.
The function is a cubic curve that is based on
the first three terms of the Taylor series for an
exponential function.
}
\examples{
pof_future_transformer_04_10kv(utilisation_pct = "Default",
placement = "Default",
altitude_m = "Default",
distance_from_coast_km = "Default",
corrosion_category_index = "Default",
age = 20,
partial_discharge = "Default",
temperature_reading = "Default",
observed_condition = "Default",
reliability_factor = "Default",
moisture = "Default",
acidity = "Default",
bd_strength = "Default",
k_value = 0.0077,
c_value = 1.087,
normal_expected_life = 55,
simulation_end_year = 100)
}
