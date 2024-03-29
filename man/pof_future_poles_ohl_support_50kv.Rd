% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/pof_future_poles_ohl_support_50kv.R
\name{pof_future_poles_ohl_support_50kv}
\alias{pof_future_poles_ohl_support_50kv}
\title{Future Probability of Failure for Poles OHL support 50 kV}
\usage{
pof_future_poles_ohl_support_50kv(
  sub_division = "Wood",
  placement = "Default",
  altitude_m = "Default",
  distance_from_coast_km = "Default",
  corrosion_category_index = "Default",
  age,
  measured_condition_inputs,
  observed_condition_inputs,
  reliability_factor = "Default",
  k_value = 0.0285,
  c_value = 1.087,
  normal_expected_life = "Default",
  simulation_end_year = 100,
  gb_ref_given = NULL
)
}
\arguments{
\item{sub_division}{String Sub Division}

\item{placement}{String. Specify if the asset is located outdoor or indoor.}

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

\item{age}{Numeric. The current age in years of the conductor.}

\item{measured_condition_inputs}{Named list observed_conditions_input}

\item{observed_condition_inputs}{Named list observed_conditions_input
\code{conductor_samp = c("Low","Medium/Normal","High","Default")}.
See page 161, table 199 and 201 in CNAIM (2021).}

\item{reliability_factor}{Numeric. \code{reliability_factor}
shall have a value between 0.6 and 1.5. A setting of \code{"Default"}
 sets the \code{reliability_factor} to 1.
See section 6.14 on page 73 in CNAIM (2021).}

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
annual probability of failure per kilometer for a Poles OHL support 50 kV.
The function is a cubic curve that is based on
the first three terms of the Taylor series for an
exponential function.
}
\examples{
# Future annual probability of failure for Poles OHL support 50 kV
pof_future_poles_ohl_support_50kv(
sub_division = "Wood",
placement = "Default",
altitude_m = "Default",
distance_from_coast_km = "Default",
corrosion_category_index = "Default",
age = 10,
observed_condition_inputs =
list("visual_pole_cond" =
list("Condition Criteria: Pole Top Rot Present?" = "Default"),
"pole_leaning" = list("Condition Criteria: Pole Leaning?" = "Default"),
"bird_animal_damage" =
list("Condition Criteria: Bird/Animal Damage?" = "Default"),
"top_rot"  = list("Condition Criteria: Pole Top Rot Present?" = "Default")),
measured_condition_inputs =
list("pole_decay" =
list("Condition Criteria: Degree of Decay/Deterioration" = "Default")),
reliability_factor = "Default",
k_value = 0.0285,
c_value = 1.087,
normal_expected_life = "Default",
simulation_end_year = 100)
}
