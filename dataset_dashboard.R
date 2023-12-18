# Library
library(tidyverse)
library(reshape2)

# Load dataset
dataset <- read.csv('C:\\Users\\franc\\Documents\\GitHub\\rproject\\Dataset\\winequality.csv')

# Transform
wine_dataset <- dataset %>%
  mutate_all(~ ifelse(is.na(.), 0, .))

# Correlation for heatmap
red_cor <- cor(subset(wine_dataset, type == "red")[, c("fixed.acidity", "volatile.acidity", "citric.acid", "residual.sugar",
                           "chlorides", "free.sulfur.dioxide", "total.sulfur.dioxide",
                           "density", "pH", "sulphates", "alcohol", "quality")])

white_cor <- cor(subset(wine_dataset, type == "white")[, c("fixed.acidity", "volatile.acidity", "citric.acid", "residual.sugar",
                                                       "chlorides", "free.sulfur.dioxide", "total.sulfur.dioxide",
                                                       "density", "pH", "sulphates", "alcohol", "quality")])



# Table correlation and p-value

var <- setdiff(names(wine_dataset), 'type')
redw_ds <- filter(wine_dataset, type == 'red')
whitew_ds <- filter(wine_dataset, type == 'white')

# Red wine

red_cor_func <- function(x) {
  result <- cor.test(x, redw_ds$quality, method = 'pearson')
  return(list(correlation = cor(x, redw_ds$quality), p_value = result$p.value))
}

red_cor_results <- lapply(redw_ds[var], red_cor_func)
df_red_cor <- do.call(rbind, red_cor_results)

# White wine

white_cor_func <- function(x) {
  result <- cor.test(x, whitew_ds$quality, method = 'pearson')
  return(list(correlation = cor(x, whitew_ds$quality), p_value = result$p.value))
}

white_cor_results <- lapply(whitew_ds[var], white_cor_func)
df_white_cor <- do.call(rbind, white_cor_results)