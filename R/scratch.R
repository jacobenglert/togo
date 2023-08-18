###########################################################################
# Program Name: SSA
# Author: Jacob Englert
# Date: 04 August 2023
# Purpose: Implement SSA from scratch
###########################################################################


# Load Data ---------------------------------------------------------------
library(tidyverse)
togo_data <- read_csv('Data/imputed.csv', show_col_types = FALSE)
sokode_min <- filter(togo_data, Location == 'Sokode', Type == 'Minimum') |>
  mutate(Date = as_date(paste0(Year, '-', Month, '-15')))
temp <- sokode_min$Temperature
plot(temp)

sokode_min |>
  select(Date, Temperature) |>
  write_csv('sokode_min.csv')

sokode_min |>
  ggplot(aes(x = Date, y = Temperature)) +
  geom_point() +
  geom_line() +
  geom_smooth() +
  scale_x_date(date_breaks = '10 years', date_labels = "%Y") +
  theme_bw()
#ggsave('Figures/site/soko_min.png', width = 6.5, height = 4.5)

# Create Trajectory Matrix ------------------------------------------------

N <- length(temp)             # Length of original time series
L <- 240 # length(temp) / 2   # Lag window length
K <- N - L + 1       
d <- 1:L

# Create the trajectory matrix
a <- sapply(1:K, \(i) temp[i:(i + L - 1)])
A <- matrix(a, nrow = L, ncol = K)


# Perform the Decomposition -----------------------------------------------

# SVD
A_decomposed <- svd(A)
U <- A_decomposed$u
V <- A_decomposed$v
D <- diag(A_decomposed$d)
# A = U %*% D %*% t(V)

# Compute relative singular values
# lambda_rel <- diag(D) / sum(diag(D))

# Obtain the individual components of the SVD (no grouping)
X <- list()
for(i in d){
  X[[i]] <- D[i,i] * U[,i,drop = FALSE] %*% t(V)[i,,drop=FALSE]
}
# test <- Reduce('+', X)
# test == A
# 
# S = A %*% t(A)
# round(sqrt(eigen(S)$values), 5) == round(diag(D), 5)
# Eigentriple Grouping ----------------------------------------------------

# Plot the singular values
scree_plot(D, trend = TRUE) # with trend
#ggsave('Figures/site/scree.png', width = 6, height = 4)
scree_plot(D) # without trend

# Examine Left Singular Vectors
plot_left_singular_vectors(U, D, n_vectors = 16, trend = TRUE) # with trend
# ggsave('Figures/site/vectors_w_trend.png', width = 8, height = 7)
plot_left_singular_vectors(U, D, n_vectors = 12) # without trend
# ggsave('Figures/site/vectors_no_trend.png', width = 8, height = 7)

# Examine Paired Left Singular Vectors
plot_left_singular_vectors(U, n_vectors = 17, trend = TRUE, type = 'paired') # with trend
#ggsave('Figures/site/paired_vectors_w_trend.png', width = 8, height = 7)
plot_left_singular_vectors(U, n_vectors = 12, type = 'paired') # without trend


# Reconstruction ----------------------------------------------------------

# Split eigentriples according to grouping analysis
trend_id <- c(1, 10) #c(1,8)
season_id <-  c(2:9, 11:12) # c(2:3, 4:5, 6:6, 9:10)
noise_id <- d[!(d %in% c(trend_id, season_id))]

# Create vector indicating which group each component belongs to
groups <- numeric(L)
groups[trend_id] <- 1; groups[season_id] <- 2; groups[noise_id] <- 3

# Examine summed Left Singular Vectors for each group
par(mfrow = c(3, 1))
for(i in unique(groups)) plot(rowSums(U[,groups == i]), type = 'l')

# Perform the reconstruction with diagonal averaging
Y_k <- lapply(unique(groups), \(j) hankelize(Reduce('+', X[groups == j])))
for(i in unique(groups)) plot(Y_k[[i]], type = 'l')

data.frame(Index = 1:N,
           Value = do.call(c, Y_k),
           Type = rep(c('Trend','Seasonality','Noise'), each = N)) |>
  ggplot(aes(x = Index, y = Value, color = Type)) +
  geom_line(show.legend = FALSE) +
  theme_bw() +
  facet_wrap(~factor(Type, levels = c('Trend','Seasonality','Noise')),
             scales = 'free', ncol = 1) +
  labs(y = 'Reconstructed Value')
# ggsave('Figures/site/reconstructed.png', width = 6, height = 5)

# Test noise for normality
par(mfrow = c(1,1))
hist(Y_k[[3]])
shapiro.test(Y_k[[3]])

# View pretty plot
par(mfrow = c(1,1))
plot(temp)
lines(Y_k[[1]], col = 'purple')
lines(Y_k[[1]] + Y_k[[2]], col = 'forestgreen')



# Generate Forecasts ------------------------------------------------------


# LRR

len <- 100
preds <- list()
for(i in unique(groups)){
  
  # Compute LRR parameters
  idx <- L
  lpf <- U[, groups == i, drop = FALSE] %*% t(U[idx, groups == i, drop = FALSE])
  params <- lpf[-idx] / (1 - lpf[idx])
  
  preds[[i]] <- c(Y_k[[i]], rep(NA, len))
  for(j in 1:len){
    preds[[i]][N + j] <- sum(preds[[i]][(N+j-(L-1)):(N+j-1)] * params)
  }
  
  preds[[i]] <- preds[[i]][(N+1):(N+len)]
  
}

par(mfrow = c(3,1))
for(i in unique(groups)) plot(preds[[i]])

data.frame(Index = 1:(N+100),
           obs = c(temp, rep(NA, 100)),
           Trend = c(Y_k[[1]], preds[[1]])) |>
  mutate(Seasonality = Trend + c(Y_k[[2]], preds[[2]])) |>
  pivot_longer(cols = Trend:Seasonality, names_to = 'Component', values_to = 'Value') |>
  mutate(Type = ifelse(Index > N, 'Predicted', 'Model')) |>
  ggplot(aes(x = Index)) +
  geom_point(aes(y = obs)) +
  geom_line(aes(y = Value, color = Component, lty = Type)) +
  scale_color_manual(values = scales::hue_pal()(3)[2:3]) +
  theme_bw() +
  theme(legend.position = 'top') +
  labs(x = 'Index', 
       y = 'Temperature',
       color = 'Component',
       lty = 'Type')
ggsave('Figures/site/prediction.png', width = 7, height = 6)

x <- Y_k
wcor(Y_k, L)
wcor <- function(x, L){
  
  # Convert to matrix if provided list
  if(is.list(x)) x <- do.call(cbind, x)
  
  n_series <- ncol(x)
  N <- nrow(x)
  wcors <- matrix(NA, nrow = n_series, ncol = n_series)
  
  weights <- pmin(1:N, L, N - (1:N))
  wvars <- sapply(1:n_series, \(i) crossprod(weights * x[,i], x[,i]))
  
  for(i in 1:n_series){
    for(j in 1:n_series){
      wcov <- crossprod(weights * x[,i], x[,j])
      wcors[i,j] <- wcov / (sqrt(wvars[i]) * sqrt(wvars[j]))
    }
  }
  
  return(wcors)
}

plot_left_singular_vectors <- function(U, D = NULL, n_vectors = 12, type = 'original',
                                       trend = FALSE){
  
  if(!trend) cols_want <- 2:(n_vectors+1) #U <- U[,-1]
  else cols_want <- 1:n_vectors
  
  firsts <- U |>
    as.data.frame() |>
    select(!!!vars(cols_want)) |>
    mutate(Index = row_number()) |>
    pivot_longer(cols = -Index, names_to = 'Vector1', values_to = 'Value1')
  
  if(!is.null(D)){
    lambda <- diag(D)[cols_want] / sum(diag(D)[cols_want])
    firsts <- firsts |>
      mutate(lambda = rep(lambda, times = nrow(U)),
             Vector1 = paste0(Vector1, ' (', round(lambda*100, 2), '%)'))
  }
  
  firsts$Vector1 <- factor(firsts$Vector1, levels = unique(firsts$Vector1))
  
  if(type == 'original'){
    
    firsts$Vector1 <- gsub('V', 'U', firsts$Vector1)
    firsts$Vector1 <- factor(firsts$Vector1, levels = unique(firsts$Vector1))
    
    firsts |>
      ggplot(aes(x = Index, y = Value1)) +
      geom_line() +
      facet_wrap(~Vector1, scales = 'free') +
      theme_bw() +
      labs(y = 'Value')
    
  } else if(type == 'paired'){
    
    seconds <- U |>
      as.data.frame() |>
      select(!!!vars(cols_want)) |>
      mutate(Index = row_number()) |>
      pivot_longer(cols = -Index, names_to = 'Vector2', values_to = 'Value2')
    seconds$Vector2 <- factor(seconds$Vector2, levels = unique(seconds$Vector2))
    
    pairs <- firsts |>
      left_join(seconds, by = join_by(Index), relationship = 'many-to-many') |>
      filter(as.numeric(Vector1) == (as.numeric(Vector2) - 1)) |>
      mutate(VectorPair = paste0(Vector1, ' & ', Vector2),
             VectorPair = gsub('V', 'U', VectorPair))
    pairs$VectorPair <- factor(pairs$VectorPair, levels = unique(pairs$VectorPair))
    
    pairs |>  
      ggplot(aes(x = Value1, y = Value2)) +
      geom_path() +
      facet_wrap(~VectorPair, scales = 'free') +
      theme_bw() +
      theme(axis.ticks = element_blank(),
            axis.text = element_blank(),
            axis.title = element_blank())
    
  }
}

scree_plot <- function(D, n = min(50, nrow(D)), trend = FALSE){
  
  d <- diag(D)
  
  if(!trend) d <- d[2:(n+1)]
  else d <- d[1:n]
  
  data.frame(d) |>
    mutate(Index = row_number()) |>
    ggplot(aes(x = Index, y = log(d))) +
    geom_point() +
    geom_line() +
    theme_bw() +
    labs(y = 'Log Singular Value')
}

hankelize <- function(X){
  n_rows <- nrow(X)
  n_cols <- ncol(X)
  N <- n_rows + n_cols - 1
  y_sum <- numeric(N)
  y_n <- numeric(N)
  
  for(i in 1:n_rows){
    for(j in 1:n_cols){
      y_sum[i + j - 1] <- y_sum[i + j - 1] + X[i,j]
      y_n[i + j - 1] <- y_n[i + j - 1] + 1
    }
  }
  
  return(y_sum / y_n)
}
