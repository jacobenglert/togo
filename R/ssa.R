# Program Name: ssa.R
# Author:       Jacob Englert
# Date:         18 September 2019
# Purpose:      Run SSA manually on each city

# Load Packages -----------------------------------------------------------
library(tidyverse)
library(Rssa)

# Import Data -------------------------------------------------------------
togo_data <- read_csv(here::here('Data','Clean','imputed.csv'))

# Create SSA Function -----------------------------------------------------
SSA <- function(city, var, n){
  
  # Subset data and create time-series object
  data <- filter(togo_data, Location == city, Type == var)
  temp <- ts(data$Temperature, frequency = 12, start = c(min(data$Year), 1))
  len <- length(temp) / 2
  text <- ifelse(var == "Maximum", '(Maxima)', '(Minima)')
  
  # Create SSA decomposition and reconstruction objects
  s_temp <- ssa(x = temp)
  r_temp <- reconstruct(s_temp, groups = list(Trend = 1,
                                              TrendPlusSeasonality = 1:n))
  d_temp <- ssa(x = temp - r_temp$Trend) # Decomposition on detrended series

  # Scree Plots (For entire decomposition & detrended series)
  #  - Retrieve singular values from SVD (SSA computes 50 by default)
  sigmas <- data.frame(Index = 1:50, 
                       s_sigma = s_temp$sigma, 
                       d_sigma = d_temp$sigma)
  
  #  - Scree Plot #1: Including Trend
  ggplot(sigmas, aes(x = Index, y = s_sigma)) +
    geom_line() +
    geom_point() +
    scale_y_log10() +
    labs(title = 'Scree Plot (including Trend)',
         subtitle = paste(city, text),
         x = 'Index',
         y = 'Singular Value') +
    theme_bw()
  image <- paste(city, var, "scree_trend", sep = "_")
  ggsave(paste0('Figures/', image, ".png"))
  
  #  - Scree Plot #2: Excluding Trend
  ggplot(sigmas, aes(x = Index, y = d_sigma)) +
    geom_line() +
    geom_point() +
    scale_y_log10() +
    labs(title = 'Scree Plot (excluding Trend)',
         subtitle = paste(city, text),
         x = 'Index',
         y = 'Singular Value') +
    theme_bw()
  image <- paste(city, var, "scree_notrend", sep = "_")
  ggsave(paste0('Figures/', image, ".png"))
  
  
  # W-correlation Analysis
  #  - Create the w-correlation matrix
  wcor <- data.frame(Value = matrix(wcor(d_temp, groups = 1:50))) %>%
    mutate(eVec1 = rep(paste('U', rep(1:50), sep = ''), each = 50),
           eVec2 = rep(paste('U', rep(1:50), sep = ''), 50),
           eVec1Num = parse_number(eVec1),
           eVec2Num = parse_number(eVec2))
  wcor$eVec1 <- factor(wcor$eVec1, levels = unique(wcor$eVec1))
  wcor$eVec2 <- factor(wcor$eVec2, levels = unique(wcor$eVec1))
  
  #  - Plot the w-correlation matrix for the first n left singular vectors
  #     (excluding the first, which corresponds to trend)
  wcor %>% filter(eVec1Num <= (n-1) & eVec2Num <= (n-1)) %>%
    ggplot(mapping = aes(x = eVec1, y = eVec2, fill = Value)) +
      geom_tile(show.legend = FALSE) +
      scale_fill_gradient(low = 'white', high = 'black') +
      coord_equal() +
      labs(title = 'W-correlation Matrix',
           subtitle = paste(city, text),
           x = '', y = '') +
      theme_bw()
  image <- paste(city, var, "wcor", sep = "_")
  ggsave(paste0('Figures/', image, ".png"))
  
  
  # Visualizing Left Singular Vectors
  #  - Get U vectors
  Us <- data.frame(matrix(data = d_temp$U, ncol = 50), Index = 1:len) %>%
    gather(key = 'eVec', value = 'Value', X1:X50) %>%
    mutate(VecNum = parse_number(eVec),
           eVecC = paste('U', VecNum, sep = ''))
  Us$eVec <- factor(Us$eVecC, levels = unique(Us$eVecC))
  
  
  #  - Plot first n Left Singular Vectors (excluding the first, which
  #     correpsonds to trend)
  Us %>% 
    filter(VecNum <= (n-1)) %>%
    ggplot(mapping = aes(x = Index, y = Value)) +
      geom_line(color = 'blue') +
      facet_wrap(~ eVec, ncol = 4) +
      labs(title = 'Left Singular Vectors',
           subtitle = paste(city, text)) +
      theme_bw() +
      theme(axis.ticks = element_blank(),
            axis.text = element_blank(),
            axis.title = element_blank())
  image <- paste(city, var, "vectors", sep = "_")
  ggsave(paste0('Figures/', image, ".png"))
  
  #  - Perform Cartesian join on U vectors and keep only those pairings of
  #     interest to simplify computations. 
  PairedUs <- Us %>%
    full_join(Us, by = c("Index" = "Index"), relationship = 'many-to-many') %>%
    mutate(Pairing = paste(eVecC.x, ' & ', eVecC.y)) %>%
    filter(VecNum.y - VecNum.x == 1) %>%
    left_join(wcor, by = c("VecNum.x" = "eVec1Num", "VecNum.y" = "eVec2Num")) %>%
    rename(Wcor = Value)
  PairedUs$Pairing <- factor(PairedUs$Pairing, levels = unique(PairedUs$Pairing))
  
  #  - Create Paired plot
  PairedUs %>% 
    filter(VecNum.x <= (n-1)) %>%
    ggplot(mapping = aes(x = Value.y, y = Value.x)) + 
      geom_path(aes(color = Wcor)) + # Coloration by correlations
      facet_wrap(~ Pairing, ncol = 4) +
      labs(title = 'Plot of Paired Left Singular Vectors',
           subtitle = paste(city, text)) +
      coord_equal() + 
      theme_bw() +
      theme(axis.ticks = element_blank(),
            axis.text = element_blank(),
            axis.title = element_blank())
  image <- paste(city, var, "paired_vectors", sep = "_")
  ggsave(paste0('Figures/', image, ".png"))
  
  
  # Display the decomposed time series
  # - Gather Trend, Seasonality, and Original series into a data frame
  decomp <- data.frame(Trend = r_temp$Trend, 
                       Seasonality = r_temp$TrendPlusSeasonality,
                       Original = temp,
                       DecYear = data$DecYear) %>%
    gather(key = 'type', value = 'Temperature', c(Trend,Seasonality,Original))
  
  # - Plot the decomposed series
  decomp %>%
    ggplot(mapping = aes(x = DecYear, y = Temperature, color = type)) +
      geom_line() +
      labs(title = 'Reconstructed Series',
           subtitle = paste(city, text),
           x = 'Year',
           y = expression(paste('Temperature (', degree, 'C)'))) +
      scale_y_continuous() +
      scale_x_continuous(breaks = seq(1960, 2015, 5)) +
      theme_bw() +
      theme(legend.title = element_blank())
  image <- paste(city, var, "decomposed", sep = "_")
  ggsave(paste0('Figures/', image, ".png"))
  
  
  # Print period estimates
  # pe <- parestimate(d_temp, groups = list(c(1:8)))
  # pedf <- data.frame(PeriodsInMonths = unique(round(abs(pe$periods),2))) %>%
  #   mutate(PeriodsInYears = round(PeriodsInMonths/12,1))
  # image <- paste(city, var, "periods", sep = "_")
  # kable(pedf) %>%
  #   kable_styling() %>%
  #   save_kable(file = paste0('Figures/', image, ".html"), self_contained = T)
  
  # Forecast into 2100
  #   - Use vforecast function to create a forecast
  #     (Does not perform well if trend components included in signal)
  f_temp <- vforecast(s_temp, groups = list(Trend = 1, Signal = 1:n),
                      len = 1008, only.new = TRUE)
  
  #   - Combine observed data and forecasts into a data frame
  f_data <- data.frame(
    Temperature = c(temp, f_temp$Signal, f_temp$Trend),
    Type = c(rep('Observed', length(temp)), 
             rep('Projected Seasonality', 1008),
             rep('Projected Trend', 1008)),
    Year = c(rep(min(data$Year):2015, each = 12), rep(2016:2099, 2, each = 12)),
    MonthNum = 1:12) %>%
    mutate(DecYear = Year + (MonthNum - 0.5)/12) %>%
    select(Temperature, Type, DecYear)
  
  #   - Plot the forecast
  ggplot(f_data, aes(x = DecYear, y = Temperature, color = Type)) +
    geom_line() +
    scale_x_continuous(breaks = seq(round(min(data$Year), -1), 2100, 20)) +
    labs(title = paste("Projection into 2100 for", city),
         x = 'Year',
         y = expression(paste("Temperature (", degree, "C)"))) +
    theme_bw() +
    theme(legend.title = element_blank())
  image <- paste(city, var, "forecast", sep = "_")
  ggsave(paste0('Figures/', image, ".png"))
}

# Perform SSA on Minimum Temperatures -------------------------------------
SSA(city = 'Atakpame', var = 'Minimum', n = 13) # 3
SSA(city = 'Dapaong', var = 'Minimum', n = 13) # 11
SSA(city = 'Kara', var = 'Minimum', n = 13) # 9
SSA(city = 'Kouma-Konda', var = 'Minimum', n = 13) # 7
SSA(city = 'Lome', var = 'Minimum', n = 13) # 9
SSA(city = 'Mango', var = 'Minimum', n = 13) # 7
SSA(city = 'Niamtougou', var = 'Minimum', n = 13) # 5
SSA(city = 'Sokode', var = 'Minimum', n = 13) # 9
SSA(city = 'Sotouboua', var = 'Minimum', n = 13) # 5
SSA(city = 'Tabligbo', var = 'Minimum', n = 13) # 7 or 9?

# Perform SSA on Maximum Temperatures -------------------------------------
SSA(city = 'Atakpame', var = 'Maximum', n = 13) # 9
SSA(city = 'Dapaong', var = 'Maximum', n = 13) # 7
SSA(city = 'Kara', var = 'Maximum', n = 13) # 7
SSA(city = 'Kouma-Konda', var = 'Maximum', n = 13) # 15?
SSA(city = 'Lome', var = 'Maximum', n = 13) # 7
SSA(city = 'Mango', var = 'Maximum', n = 13) # 7
SSA(city = 'Niamtougou', var = 'Maximum', n = 13) # 7
SSA(city = 'Sokode', var = 'Maximum', n = 13) # 9
SSA(city = 'Sotouboua', var = 'Maximum', n = 13) # 5 or 9?
SSA(city = 'Tabligbo', var = 'Maximum', n = 13) # 5 or 9?
