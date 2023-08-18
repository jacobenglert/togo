# Program Name: impute.R
# Author:       Jacob Englert
# Date:         18 September 2019
# Purpose:      Impute values for missing observations in cleaned data

# Load Required Packages --------------------------------------------------
library(tidyverse)

# Load clean data ---------------------------------------------------------
clean <- read_csv(here::here('Data','Clean','clean.csv'))

# Impute using the final models from MAT 375 ------------------------------

# Maximum Temperatures
max_data <- clean |>
  filter(Type == 'Maximum') |>
  mutate(Temperature = ifelse(ExcelAvg == 'Yes' | Outlier == 'Yes', NA, Temperature))

max_model <- lm(formula = Temperature ~ DecYear +
                  sin(DecYear*2*pi) + cos(DecYear*2*pi) +
                  sin(DecYear*2*pi/(1/2)) + cos(DecYear*2*pi/(1/2)) +
                  sin(DecYear*2*pi/(1/3)) + cos(DecYear*2*pi/(1/3)) +
                  sin(DecYear*2*pi/(1/4)) + cos(DecYear*2*pi/(1/4)) +
                  sin(DecYear*2*pi/13) + cos(DecYear*2*pi/13) +
                  Latitude +
                  Longitude +
                  I(Latitude*Longitude) +
                  I(Latitude**2) +
                  I(Longitude**2) +
                  Elevation +
                  ENSO +
                  SST,
                data = max_data)
summary(max_model)

# Use model to predict for entire dataset
max_pred <- predict(max_model, max_data)

# Impute
max_idx <- which(is.na(max_data$Temperature))
max_data$Temperature[max_idx] <- max_pred[max_idx]
MaxImp <- vector("character", length(max_data$Temperature))
MaxImp[max_idx] <- "Yes"
MaxImp[-max_idx] <- "No"
max_data$Imputed <- MaxImp

# Minimum Temperatures
min_data <- clean %>%
  filter(Type == 'Minimum') %>%
  mutate(Temperature = ifelse(ExcelAvg == 'Yes' | Outlier == 'Yes', NA, Temperature))

min_model <- lm(formula = Temperature ~ DecYear +
                  sin(DecYear*2*pi) + cos(DecYear*2*pi) +
                  sin(DecYear*2*pi/(1/2)) + cos(DecYear*2*pi/(1/2)) +
                  sin(DecYear*2*pi/(1/3)) + cos(DecYear*2*pi/(1/3)) +
                  sin(DecYear*2*pi/13) + cos(DecYear*2*pi/13) +
                  sin(DecYear*2*pi/20) + cos(DecYear*2*pi/20) +
                  Latitude +
                  Longitude +
                  I(Latitude*Longitude) +
                  I(Latitude**2) +
                  I(Longitude**2) +
                  Elevation +
                  ENSO +
                  SST,
                data = min_data)
summary(min_model)

# Use model to predict for entire dataset
min_pred <- predict(min_model, min_data)

# Impute
min_idx <- which(is.na(min_data$Temperature))
min_data$Temperature[min_idx] <- min_pred[min_idx]
MinImp <- vector("character", length(min_data$Temperature))
MinImp[min_idx] <- "Yes"
MinImp[-min_idx] <- "No"
min_data$Imputed <- MinImp

# Combine imputed datasets and export
imputed <- bind_rows(min_data, max_data) %>%
  arrange(Location, DecYear, Type)

# Export Imputed Data -----------------------------------------------------
write_csv(imputed, here::here('Data','Clean','imputed.csv'))
