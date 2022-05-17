#' @importFrom stats dweibull lm uniroot
#' @title Prediction function for Weibull model
#' @description This function uses the Weibull model parameters trained by the function \code{\link{train_weibull_model}}(), together
#' with the environmental factors for a specific transformer, and determines the probability of failure at a given age.
#' @param age Numeric. Age of transformer which should be used in the prediction.
#' @param environmental_factors Data frame. Must contain the following fields:
#' utilisation_pct: Numeric or "Default",
#' placement: "Indoor", "Outdoor" or "Default",
#' altitude_m: Numeric or "Default",
#' distance_from_coast_km: Numeric or "Default",
#' corrosion_category_index: Numeric or "Default",
#' partial_discharge: "Low", "Medium", "High (Not Confirmed)", "High (Confirmed)" or "Default",
#' oil_acidity: Numeric or "Default",
#' temperature_reading: "Normal", "Moderately High", "Very High" or "Default",
#' observed_condition: "No deterioration", "Superficial/minor deterioration", "Slight Deterioration", "Some deterioration", "Substantial deterioration" or "Default"
#' Default value if environmental_factors is not provided: data frame with value "Default" for all fields
#' @param weibull_model_parameters Data frame. The output returned by the function
#'  \code{\link{train_weibull_model}}().
#'  Default value if weibull_parameters is not provided: data frame with parameters trained on data set transformer_11kv_faults.rda
#' @return Numeric. Probability of failure at the given age.
#' @source \url{https://www.cnaim.io/docs/fault-analysis/}
#' @export
#' @examples
#' predict_weibull_model(age = 50)
#'

predict_weibull_model <- function(age,
                                  environmental_factors = data.frame(utilisation_pct = "Default",
                                                                     placement = "Default",
                                                                     altitude_m = "Default",
                                                                     distance_from_coast_km = "Default",
                                                                     corrosion_category_index = "Default",
                                                                     partial_discharge = "Default",
                                                                     oil_acidity = "Default",
                                                                     temperature_reading = "Default",
                                                                     observed_condition = "Default"),
                                  weibull_model_parameters = data.frame(shapes = c(3.597272, 2.528015, 2.273607, 2.101450, 2.048909),
                                                                        scales.intercept = c(100.17922,  45.54622,  73.63507,  29.99655,  31.19306),
                                                                        scales.1 = c(0.0028536801,  0.0014449054,  0.0011716558, -0.0003356626, -0.0017302242),
                                                                        scales.2 = c(-8.202209, -3.856043, -2.818854, -2.388243, -2.940468),
                                                                        scales.3 = c(-0.003023546, -0.001602048, -0.001348340, -0.001988660, -0.003149921),
                                                                        scales.4 = c(-0.040016081, -0.028129483, -0.017586604, -0.009426902, -0.021783120),
                                                                        scales.5 = c(-1.4776137, -0.6794045, -0.6000869, -0.3839049, -0.4445468),
                                                                        scales.6 = c(-0.811395564,  0.015705206, -9.815935489, -0.002548827, -0.085903822),
                                                                        scales.7 = c(-4.4776511, -0.3677058,  0.4590218, -0.6364809, -0.3314029),
                                                                        scales.8 = c(-1.5861982,  0.0000000, -0.1398528, -0.1721091,  0.0000000),
                                                                        scales.9 = c(-0.7914404, -0.2632199, -1.1882148,  0.0000000,  0.0000000))
) {

  EF <- environmental_factors
  WP <- weibull_model_parameters


  # need numerical representation of the data:
  EF_num <- data.frame(0)

  # 1. utilisation percentage
  if (EF$utilisation_pct == "Default") EF_num$utilisation_pct <- 85
  else EF_num$utilisation_pct <- EF$utilisation_pct

  # 2. placement
  EF_num$placement[EF$placement %in% c("Indoor", "Default")] <- 1
  EF_num$placement[EF$placement == "Outdoor"] <- 2

  # 3. altitude in meters
  if (EF$altitude_m == "Default") EF_num$altitude_m <- 150
  else EF_num$altitude_m <- EF$altitude_m

  # 4. distance from coast in km
  if (EF$distance_from_coast_km == "Default") EF_num$distance_from_coast_km <- 15
  else EF_num$distance_from_coast_km <- EF$distance_from_coast_km

  # 5. corrosion category index
  if (EF$corrosion_category_index == "Default") EF_num$corrosion_category_index <- 3
  else EF_num$corrosion_category_index <- as.numeric(EF$corrosion_category_index)

  # 6. partial discharge
  EF_num$partial_discharge[EF$partial_discharge %in% c("Low", "Default")] <- 1
  EF_num$partial_discharge[EF$partial_discharge == "Medium"] <- 2
  EF_num$partial_discharge[EF$partial_discharge == "High (Not Confirmed)"] <- 3
  EF_num$partial_discharge[EF$partial_discharge == "High (Confirmed)"] <- 4

  # 7. oil acidity
  if (EF$oil_acidity == "Default") EF_num$oil_acidity <- 0.25
  else EF_num$oil_acidity <- EF$oil_acidity

  # 8. temperature reading
  EF_num$temperature_reading[EF$temperature_reading %in% c("Normal", "Default")] <- 1
  EF_num$temperature_reading[EF$temperature_reading == "Moderately High"] <- 2
  EF_num$temperature_reading[EF$temperature_reading == "Very High"] <- 3

  # 9. observed condition
  EF_num$observed_condition[EF$observed_condition ==  "No deterioration"] <- 1
  EF_num$observed_condition[EF$observed_condition %in%  c("Superficial/minor deterioration", "Default")] <- 2
  EF_num$observed_condition[EF$observed_condition ==  "Slight Deterioration"] <- 3
  EF_num$observed_condition[EF$observed_condition ==  "Some deterioration"] <- 4
  EF_num$observed_condition[EF$observed_condition ==  "Substantial deterioration"] <- 5



  # find which part of the partition the data point is in::

  if (EF_num$partial_discharge %in% c(3,4)) {
    idx <- 3
  } else {
    if (EF_num$temperature_reading == 3) {
      if (EF_num$observed_condition == 5) {
        idx <- 5
      } else {
        idx <- 2
      }
    } else {
      if (EF_num$observed_condition == 5) {
        idx <- 4
      } else {
        idx <- 1
      }
    }
  }


  # calculate scale parameter from multilinear fit, then use the Weibull distribution with the trained scale
  # and shape parameters to find the probability of failure:

  scale = WP$scales.intercept[idx]
  + WP$scales.1[idx] * EF_num$utilisation_pct
  + WP$scales.2[idx] * EF_num$placement
  + WP$scales.3[idx] * EF_num$altitude_m
  + WP$scales.4[idx] * EF_num$distance_from_coast_km
  + WP$scales.5[idx] * EF_num$corrosion_category_index
  + WP$scales.6[idx] * EF_num$partial_discharge
  + WP$scales.7[idx] * EF_num$oil_acidity
  + WP$scales.8[idx] * EF_num$temperature_reading
  + WP$scales.9[idx] * EF_num$observed_condition

  pof <- dweibull(age, WP$shapes[idx], scale)

  return(pof)
}

#' @title Training function for Weibull model
#' @description This function uses transformer fault statistics data to train a Weibull model: Based on the environmental
#' factors determining a transformer's expected lifetime, the set of all data points is first partitioned into five parts.
#' Then a multilinear estimate for the expected lifetime of a transformer is trained for each part separately, and the
#' corresponding Weibull shape and scale parameters for the five parts are estimated. The function returns the shape and scale
#' parameters needed for the function \code{\link{predict_weibull_model}}().
#' @param transformer_faults_data Data frame. Contains past data on transformer faults, together with environmental factors.
#' Must contain the following fields:
#' utilisation_pct: Numeric or "Default",
#' placement: "Indoor", "Outdoor" or "Default",
#' altitude_m: Numeric or "Default",
#' distance_from_coast_km: Numeric or "Default",
#' corrosion_category_index: Numeric or "Default",
#' partial_discharge: "Low", "Medium", "High (Not Confirmed)", "High (Confirmed)" or "Default",
#' oil_acidity: Numeric or "Default",
#' temperature_reading: "Normal", "Moderately High", "Very High" or "Default",
#' observed_condition: "No deterioration", "Superficial/minor deterioration", "Slight Deterioration", "Some deterioration", "Substantial deterioration" or "Default"
#' age: Numeric
#' @return Data frame. All shape and scale parameters needed for the function \code{\link{predict_weibull_model}}().
#' @source \url{https://www.cnaim.io/docs/fault-analysis/}
#' @export
#' @examples
#' train_weibull_model(transformer_faults_data = transformer_11kv_faults)
#'


train_weibull_model <- function(transformer_faults_data) {

  # need numerical representation of the data:
  TF <- transformer_faults_data

  # 1. utilisation percentage
  TF_num <- data.frame(utilisation_pct = TF$utilisation_pct)
  TF_num$utilisation_pct[TF$utilisation_pct == "Default"] <- 85

  # 2. placement
  TF_num$placement[TF$placement %in% c("Indoor", "Default")] <- 1
  TF_num$placement[TF$placement == "Outdoor"] <- 2

  # 3. altitude in meters
  TF_num$altitude_m <- TF$altitude_m
  TF_num$altitude_m[TF$altitude_m == "Default"] <- 150

  # 4. distance from coast in km
  TF_num$distance_from_coast_km <- TF$distance_from_coast_km
  TF_num$distance_from_coast_km[TF$distance_from_coast_km == "Default"] <- 15

  # 5. corrosion category index
  TF_num$corrosion_category_index <- as.numeric(TF$corrosion_category_index)
  TF_num$corrosion_category_index[TF$corrosion_category_index == "Default"] <- 3

  # 6. partial discharge
  TF_num$partial_discharge[TF$partial_discharge %in% c("Low", "Default")] <- 1
  TF_num$partial_discharge[TF$partial_discharge == "Medium"] <- 2
  TF_num$partial_discharge[TF$partial_discharge == "High (Not Confirmed)"] <- 3
  TF_num$partial_discharge[TF$partial_discharge == "High (Confirmed)"] <- 4

  # 7. oil acidity
  TF_num$oil_acidity <- TF$oil_acidity
  TF_num$oil_acidity[TF$oil_acidity == "Default"] <- 0.25

  # 8. temperature reading
  TF_num$temperature_reading[TF$temperature_reading %in% c("Normal", "Default")] <- 1
  TF_num$temperature_reading[TF$temperature_reading == "Moderately High"] <- 2
  TF_num$temperature_reading[TF$temperature_reading == "Very High"] <- 3

  # 9. observed condition
  TF_num$observed_condition[TF$observed_condition ==  "No deterioration"] <- 1
  TF_num$observed_condition[TF$observed_condition %in%  c("Superficial/minor deterioration", "Default")] <- 2
  TF_num$observed_condition[TF$observed_condition ==  "Slight Deterioration"] <- 3
  TF_num$observed_condition[TF$observed_condition ==  "Some deterioration"] <- 4
  TF_num$observed_condition[TF$observed_condition ==  "Substantial deterioration"] <- 5

  # 10. age
  TF_num$age <- TF$age


  # partitioning the data into 5 bins according to the most relevant environmental factors:

  H1_data <- TF_num[! TF_num$partial_discharge %in% c(3,4),]
  H1_data <- H1_data[! H1_data$temperature_reading %in% c(3), ]
  H1_data <- H1_data[! H1_data$observed_condition %in% c(5), ]

  H2_data <- TF_num[! TF_num$partial_discharge %in% c(3,4),]
  H2_data <- H2_data[H2_data$temperature_reading %in% c(3), ]
  H2_data <- H2_data[!H2_data$observed_condition %in% c(5), ]

  H3_data <- TF_num[TF_num$partial_discharge %in% c(3,4),]

  H4_data <- TF_num[! TF_num$partial_discharge %in% c(3,4),]
  H4_data <- H4_data[! H4_data$temperature_reading %in% c(3), ]
  H4_data <- H4_data[ H4_data$observed_condition %in% c(5), ]

  H5_data <- TF_num[! TF_num$partial_discharge %in% c(3,4),]
  H5_data <- H5_data[H5_data$temperature_reading %in% c(3), ]
  H5_data <- H5_data[H5_data$observed_condition %in% c(5), ]


  # multilinear regression on each part in the partition, to find the Weibull shape and scale parameters:

  counter <- 1
  coeff_age <- matrix(0,5,10)
  shapes <- matrix(0,5,1)
  scales <- matrix(0,5,10)
  for (h in list(H1_data, H2_data, H3_data, H4_data, H5_data))
  {
    # find multilinear model for expected lifetime:
    lm_age <- lm(formula = age ~ utilisation_pct + placement + altitude_m + distance_from_coast_km
                 + corrosion_category_index + partial_discharge + oil_acidity + temperature_reading
                 + observed_condition, data = h)

    coeff_age[counter, ] <- lm_age$coefficients
    coeff_age[counter, is.na(coeff_age[counter, ])] <- 0

    # find shape and scale parameters of Weibull distribution by comparing the sample variance with the model variance:
    C <- mean(lm_age$residuals^2)/mean(lm_age$fitted.values^2)
    shapes[counter] <- uniroot(function(x) {gamma(1 + 2/x)/(gamma(1 + 1/x))^2 - 1 - C}, c(1, 10))$root
    scales[counter, ] <- coeff_age[counter, ]/gamma(1 + 1 /shapes[counter])

    counter <- counter + 1
  }

  results <- data.frame(shapes = shapes, scales.intercept = scales[ , 1], scales = scales[ , 2:length(scales[1, ])])

  return(results)
}
