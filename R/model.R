# Program Name: model.R
# Author:       Jacob Englert
# Date:         24 September 2019
# Purpose:      Using imputed data from impute.R and selected periods from
#               ssa.R, create global linear regression models for both minimum
#               and maximum temperatures and take a look at diagnostics

# Load Packages -----------------------------------------------------------
library(tidyverse)

# Import Data -------------------------------------------------------------
togo <- read_csv(here::here('Data','Clean','imputed.csv'))

# Minimum Temperature Model -----------------------------------------------

min_data <- filter(togo, Type == 'Minimum')

# Fit Random Intercept Model
min_model <- lme4::lmer(formula = Temperature ~ (1 | Location)
                         + DecYear
                         + sin(DecYear*2*pi/(1/3)) + cos(DecYear*2*pi/(1/3)) # Tri-Annual
                         + sin(DecYear*2*pi/(1/2)) + cos(DecYear*2*pi/(1/2)) # Bi-Annual
                         + sin(DecYear*2*pi) + cos(DecYear*2*pi) # Annual
                         + sin(DecYear*2*pi/(3.75)) + cos(DecYear*2*pi/(3.75)) # ENSO
                         + sin(DecYear*2*pi/(4.67)) + cos(DecYear*2*pi/(4.67)) # ENSO
                         + sin(DecYear*2*pi/(21)) + cos(DecYear*2*pi/(21)) # IPO, HC
                         + Longitude 
                         + Latitude 
                         + Elevation
                         + I(Latitude**2)
                         + I(Longitude**2) 
                         + I(Latitude*Longitude),
                         data = min_data)
summary(min_model)

# Show Model
min_data$fitted <- predict(min_model, min_data)
min_data |>
  ggplot(aes(x = DecYear)) +
  geom_point(aes(y = Temperature), alpha = 0.4) +
  geom_line(aes(y = fitted), color = 'blue', alpha = 0.8) + 
  scale_x_continuous(breaks = seq(1960, 2015, 10)) +
  labs(title = 'Minimum Temperature Model',
       x = 'Year',
       y = expression(paste("Temperature (", degree, "C)"))) +
  theme_bw() +
  facet_wrap(~Location, scales = 'free')
ggsave(here::here('Figures','min_model.png'))

# Check for outliers
car::outlierTest(min_model) # Bonferonni p-value for most extreme obs

# Assess Normality
min_res <- min_data$Temperature - min_data$fitted
qqnorm(min_res)

data.frame(residual = min_res) |>
  ggplot(aes(x = residual)) +
  geom_histogram(aes(y = after_stat(density)), binwidth = 0.3, color = 'black', fill = 'white') +
  # geom_density(aes(y = after_stat(density)), color = 'red') +
  stat_function(fun = dnorm,
                color = "red",
                args = list(mean = mean(min_res),
                            sd = sd(min_res))) +
  labs(title = 'Histogram of Residuls',
       subtitle = 'Minimum Temperature Model',
       x = 'Residual',
       y = 'Density') +
  theme_bw()
ggsave(here::here('Figures','min_model_resid.png'))

# car::qqPlot(min_model, main = "QQ Plot") #qq plot for studentized resid 
# # Evaluate Homoscedasticity
# car::ncvTest(min_model) # non-constant error variance test
# car::spreadLevelPlot(min_model) # plot studentized residuals vs. fitted values 
# 
# # Test for Autocorrelated Errors
# car::durbinWatsonTest(min_model)


# Maximum Temperature Model -----------------------------------------------
max_data <- filter(togo, Type == 'Maximum')
max_model <- lme4::lmer(Temperature ~ (1 | Location)
               + DecYear
               + sin(DecYear*2*pi/(1/4)) + cos(DecYear*2*pi/(1/4)) # Quad-Annual
               + sin(DecYear*2*pi/(1/3)) + cos(DecYear*2*pi/(1/3)) # Tri-Annual
               + sin(DecYear*2*pi/(1/2)) + cos(DecYear*2*pi/(1/2)) # Bi-Annual
               + sin(DecYear*2*pi) + cos(DecYear*2*pi) # Annual
               + sin(DecYear*2*pi/9.83) + cos(DecYear*2*pi/9.83) # PDO
               + sin(DecYear*2*pi/13) + cos(DecYear*2*pi/13) # PDO?
               + sin(DecYear*2*pi/25.42) + cos(DecYear*2*pi/25.42) # IPO?
               + Longitude 
               + Latitude 
               + Elevation
               + I(Latitude^2)
               + I(Longitude^2) 
               + I(Latitude*Longitude)
               , data = max_data)
summary(max_model)

# Show Model
max_data$fitted <- predict(max_model, max_data)
max_data |>
  ggplot(aes(x = DecYear)) +
  geom_point(aes(y = Temperature), alpha = 0.4) +
  geom_line(aes(y = fitted), color = 'red', alpha = 0.8) + 
  scale_x_continuous(breaks = seq(1960, 2015, 10)) +
  labs(title = 'Maximum Temperature Model',
       x = 'Year',
       y = expression(paste("Temperature (", degree, "C)"))) +
  theme_bw() +
  facet_wrap(~Location, scales = 'free')
ggsave(here::here('Figures','max_model.png'))

# Check for outliers
car::outlierTest(max_model) # Bonferonni p-value for most extreme obs

# Assess Normality
max_res <- max_data$Temperature - max_data$fitted
qqnorm(max_res)

data.frame(residual = max_res) |>
  ggplot(aes(x = residual)) +
  geom_histogram(aes(y = after_stat(density)), binwidth = 0.3, color = 'black', fill = 'white') +
  # geom_density(aes(y = after_stat(density)), color = 'red') +
  stat_function(fun = dnorm,
                color = "red",
                args = list(mean = mean(max_res),
                            sd = sd(max_res))) +
  labs(title = 'Histogram of Residuls',
       subtitle = 'Minimum Temperature Model',
       x = 'Residual',
       y = 'Density') +
  theme_bw()
ggsave(here::here('Figures','max_model_resid.png'))

# # Evaluate homoscedasticity
# ncvTest(max_model) # non-constant error variance test
# spreadLevelPlot(max_model) # Plot Studentized Residuals vs. Fitted Values
# 
# # Test for Autocorrelated Errors
# durbinWatsonTest(max_model)

# 3D Graphic of the effect of latitude and longitude on temperature -------

write_rds(max_model, here::here('Output','max_model.rds'))
write_rds(min_model, here::here('Output','min_model.rds'))

# Maximum temperature model
max_geo_coef <- coef(summary(max_model))[c('Longitude','Latitude', 
                                           'I(Latitude^2)','I(Longitude^2)',
                                           'I(Latitude * Longitude)'),
                                         'Estimate']

max_geo <- function(long, lat){
  cbind(long, lat, lat^2, long^2, long*lat) %*% max_geo_coef
}

rgl::open3d()
rgl::plot3d(max_geo, 
            col = colorRampPalette(c("blue", "white", "red")), 
            xlab = "Longitude", ylab = "Latitude", zlab = "GeoMaxTrend", 
            xlim = c(0,2), ylim = c(6,12),
            aspect = c(1, 1, 0.5))

# Minimum temperature model
min_geo_coef <- coef(summary(min_model))[c('Longitude','Latitude', 
                                           'I(Latitude^2)','I(Longitude^2)',
                                           'I(Latitude * Longitude)'),
                                         'Estimate']
min_geo <- function(long, lat){
  cbind(long, lat, lat^2, long^2, long*lat) %*% min_geo_coef
}
rgl::open3d()
rgl::plot3d(min_geo, 
            col = colorRampPalette(c("blue", "white", "red")), 
            xlab = "longitude", ylab = "latitude", zlab = "GeoMinTrend", 
            xlim = c(0,2), ylim = c(6,12),
            aspect = c(1, 1, 0.5))
