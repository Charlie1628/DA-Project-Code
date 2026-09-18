install.packages("tidyverse")
install.packages("car")
library(readxl)
library(dplyr)
library(tidyr)
library(car)

quality_ds <- read_xlsx("PLDS.xlsx", sheet = 2)
value_ds <- read_xlsx("PLDS.xlsx", sheet = 3)
reputation_ds <- read_xlsx("PLDS.xlsx", sheet = 4)
consideration_ds <- read_xlsx("PLDS.xlsx", sheet = 5)

n <- nrow(quality_ds) - 4

markets <- c("AU","BR","CA","DE","ES","FR","IN","IT","JP","MX","SA","UK","US")

# Transform datasets from wide to long format

long_df <- do.call(rbind, lapply(markets, function(m) {
  data.frame(
    date = quality_ds$Date[1:n],
    market = m,
    quality = quality_ds[[paste(m, "Raw")]][1:n],
    value = value_ds[[paste(m, "Raw")]][1:n],
    reputation = reputation_ds[[paste(m, "Raw")]][1:n],
    consideration = consideration_ds[[paste(m, "Raw")]][1:n]
  )
}))

# Average performance for every perceptual measure, for every market

quality_performance <- sapply(markets, function(p){
  mean(quality_ds[[paste(p, "Raw")]][1:n])
})
value_performance <- sapply(markets, function(p){
  mean(value_ds[[paste(p, "Raw")]][1:n])
})
reputation_performance <- sapply(markets, function(p){
  mean(reputation_ds[[paste(p, "Raw")]][1:n])
})

measure_performance_table <- matrix(data = c(markets, quality_performance, value_performance, reputation_performance), ncol = 4)
colnames(measure_performance_table) <- c("market", "quality", "value", "reputation")
measure_performance_table

# Pooled Pearson correlation

QVRC_correlation_table <- cor(long_df[, c("quality","value","reputation","consideration")], method = "pearson", use = "complete.obs")
QVRC_correlation_table

# Split-half testing pooled correlations

mid_date <- min(long_df$date) + (max(long_df$date) - min(long_df$date)) / 2

first_half <- long_df[long_df$date <= mid_date, ]
second_half <- long_df[long_df$date > mid_date, ]

QVRC_correlation_table1 <- cor(first_half[, c("quality","value","reputation","consideration")], method = "pearson", use = "complete.obs")
QVRC_correlation_table2 <- cor(second_half[, c("quality","value","reputation","consideration")], method = "pearson", use = "complete.obs")

QVRC_correlation_table1[ , c(-1, -2, -3)]
QVRC_correlation_table2[ , c(-1, -2, -3)]

# Pearson correlations at a market level

quality_cors <- sapply(markets, function(m){
  cor(quality_ds[[paste(m, "Raw")]][1:n], consideration_ds[[paste(m, "Raw")]][1:n], method = "pearson", use = "complete.obs")
})
value_cors <- sapply(markets, function(m){
  cor(value_ds[[paste(m, "Raw")]][1:n], consideration_ds[[paste(m, "Raw")]][1:n], method = "pearson", use = "complete.obs")
})
reputation_cors <- sapply(markets, function(m){
  cor(reputation_ds[[paste(m, "Raw")]][1:n], consideration_ds[[paste(m, "Raw")]][1:n], method = "pearson", use = "complete.obs")
})

market_level_correlation <- matrix(c(quality_cors, value_cors, reputation_cors), ncol = 3)
colnames(market_level_correlation) <- c("quality", "value", "reputation")
rownames(market_level_correlation) <- markets
market_level_correlation

# split-half testing market-level correlations

half <- floor(n / 2)

quality_cors1 <- sapply(markets, function(m){
  cor(quality_ds[[paste(m, "Raw")]][1:half], consideration_ds[[paste(m, "Raw")]][1:half], method = "pearson", use = "complete.obs")
})
quality_cors2 <- sapply(markets, function(m){
  cor(quality_ds[[paste(m, "Raw")]][(half+1):n], consideration_ds[[paste(m, "Raw")]][(half+1):n], method = "pearson", use = "complete.obs")
})

value_cors1 <- sapply(markets, function(m){
  cor(value_ds[[paste(m, "Raw")]][1:half], consideration_ds[[paste(m, "Raw")]][1:half], method = "pearson", use = "complete.obs")
})
value_cors2 <- sapply(markets, function(m){
  cor(value_ds[[paste(m, "Raw")]][(half+1):n], consideration_ds[[paste(m, "Raw")]][(half+1):n], method = "pearson", use = "complete.obs")
})

reputation_cors1 <- sapply(markets, function(m){
  cor(reputation_ds[[paste(m, "Raw")]][1:half], consideration_ds[[paste(m, "Raw")]][1:half], method = "pearson", use = "complete.obs")
})
reputation_cors2 <- sapply(markets, function(m){
  cor(reputation_ds[[paste(m, "Raw")]][(half+1):n], consideration_ds[[paste(m, "Raw")]][(half+1):n], method = "pearson", use = "complete.obs")
})

quality_cors1
quality_cors2
value_cors1
value_cors2
reputation_cors1
reputation_cors2

# Lag-testing market-level correlations for each perceptual measure

ln <- c(0, 7, 14, 30)

lag_cors_for_market <- function(mkt) {
  q_col <- quality_ds[[paste(mkt, "Raw")]]
  c_col <- consideration_ds[[paste(mkt, "Raw")]]
  
  sapply(ln, function(m) {
    cor(q_col[1:(n-m)], c_col[(1+m):n], method = "pearson", use = "complete.obs")
  })
}

quality_lag_matrix <- sapply(markets, lag_cors_for_market)
rownames(quality_lag_matrix) <- paste(ln)
quality_lag_matrix <- t(quality_lag_matrix)


lag_cors_for_market <- function(mkt) {
  v_col <- value_ds[[paste(mkt, "Raw")]]
  c_col <- consideration_ds[[paste(mkt, "Raw")]]
  
  sapply(ln, function(m) {
    cor(v_col[1:(n-m)], c_col[(1+m):n], method = "pearson", use = "complete.obs")
  })
}

value_lag_matrix <- sapply(markets, lag_cors_for_market)
rownames(value_lag_matrix) <- paste(ln)
value_lag_matrix <- t(value_lag_matrix)                         


lag_cors_for_market <- function(mkt) {
  r_col <- reputation_ds[[paste(mkt, "Raw")]]
  c_col <- consideration_ds[[paste(mkt, "Raw")]]
  
  sapply(ln, function(m) {
    cor(r_col[1:(n-m)], c_col[(1+m):n], method = "pearson", use = "complete.obs")
  })
}

reputation_lag_matrix <- sapply(markets, lag_cors_for_market)
rownames(reputation_lag_matrix) <- paste(ln)
reputation_lag_matrix <- t(reputation_lag_matrix)                         

quality_lag_matrix
value_lag_matrix
reputation_lag_matrix

# Consideration performance (current, past, change)

consideration_current_performance <- sapply(markets, function(m){
  mean(consideration_ds[[paste(m, "Raw")]][(n-29):n])
})

consideration_current_performance

consideration_30d_ago <- sapply(markets, function(m){
  mean(consideration_ds[[paste(m, "Raw")]][(n-59):(n-30)])
})

consideration_30d_ago

consideration_pct_change <- sapply(markets, function(m){
  prior <- mean(consideration_ds[[paste(m, "Raw")]][(n-59):(n-30)])
  (consideration_current_performance[m] - prior) / prior * 100
})

consideration_pct_change

# Consideration trend

day <- 1:60
trend_by_market <- function(m) {
 model <- lm(consideration_ds[[paste(m, "Raw")]][(n-59):n] ~ day)
 model$coefficients["day"] * 30
}

trend_model <- sapply(markets, trend_by_market)
trend_model

# Standardising predictors

long_df$quality_standardised <- as.numeric(scale(long_df$quality))   
long_df$value_standardised <- as.numeric(scale(long_df$value))
long_df$reputation_standardised <- as.numeric(scale(long_df$reputation))

# Main regression model + checks

driver_model <- lm(consideration ~ quality_standardised + value_standardised + reputation_standardised + factor(market), data = long_df)
summary(driver_model)

plot(driver_model, which = 1)
crPlots(driver_model)

vif_model <- lm(consideration ~ quality_standardised + value_standardised + reputation_standardised, data = long_df)
vif(vif_model)
