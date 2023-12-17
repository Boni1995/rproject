# Import libraries

library(tidyverse)
library(tibble)
library(dplyr)
library(ggplot2)
library(gridExtra)
library(reshape2)
library(grid)
library(broom)

# Check working directory and getting the path to import it next
getwd()
file.choose()

# Importing the wine dataset and printing only some rows
wine_dataset <- read.csv('C:\\Users\\franc\\Documents\\GitHub\\rproject\\Dataset\\winequality.csv')

# Exploring the dataset
names(wine_dataset) # Show column names

str(wine_dataset) # First attribute is char, the next 11 attributes are numeric, and the last one is integer

summary(wine_dataset) # Show extra information about elements inside dataset

nrow(wine_dataset) # Check number of rows

ncol(wine_dataset) # Check number of columns

dim(wine_dataset) # Check dimensions (rows and columns)

View(wine_dataset) # View of the DataFrame

head(wine_dataset) # Printing the first rows

tail(wine_dataset) # Printing the last rows

# Check if there is any NULL values
nulls <- colSums(is.na(wine_dataset))

nulls_summary <- data.frame(
  NullValues = nulls,
  Percentage = sprintf("%.2f%%",round(nulls/nrow(wine_dataset)*100,2)))

nulls_summary

# As NULL values are not representative, a second version of dataset is created, replacing the
# null with 0.
wine2_ds <- wine_dataset %>%
  mutate_all(~ ifelse(is.na(.), 0, .))

# Check new dataset
colSums(is.na(wine2_ds))
str(wine2_ds)

# Create a class for the dataset, so later I can extract red or white information
wine_ds_class <- function(data) {
  structure(list(data = data), class = "wine_ds")
}

# Create a method to load dataset of red and white wines separated.

load_redw <- function(wine_obj) {
  data <- wine_obj$data
  data %>%
    filter(type == "red")
}

load_whitew <- function(wine_obj) {
  data <- wine_obj$data
  data %>%
    filter(type == "white")
}

# Load both datasets
wine_data <- wine_ds_class(wine2_ds) # Using the class to load the dataset
redw_ds <- load_redw(wine_data) # Load only a dataset with red wine
whitew_ds <- load_whitew(wine_data) # Load only a dataset with white wine

# Print both again

head(redw_ds)
head(whitew_ds)

# Repeat summary of each new dataset

summary(redw_ds)
summary(whitew_ds)

# Chekc the amount of rows per dataset
data_per_type <- data.frame(
  Red_wine = nrow(redw_ds), # We have 1599 rows
  White_wine = nrow(whitew_ds)) # We have 4898 rows

data_per_type

# Show a bar plot with number of rows per dataset

barplot(as.numeric(data_per_type), col = c("#8B0000","#FFFFE0"), width = c(50, 50), las=2, space = 1,
        names.arg= c("Red wine", "White wine"), cex.names=0.75,
        main = "Size of sample by type of wine", ylab = "n° of wines in sample", xlab = "Wine Dataset",
        ylim = c(0, 5000))

# Show distribution of quality by each type 

rw <-ggplot(aes(x=quality), data =  redw_ds) +
  geom_histogram(color =I('black'),fill = I('#8B0000'), binwidth = 0.5) +
  ggtitle('Red wine sample by quality') +
  xlab('Quality') +
  ylab('n° of wines in sample') +
  scale_x_continuous(breaks = unique(redw_ds$quality))

ww <-ggplot(aes(x=quality), data =  whitew_ds) +
  geom_histogram(color =I('black'),fill = I('#FFFFE0'), binwidth = 0.5) +
  ggtitle('White wine sample by quality') +
  xlab('Quality') +
  ylab('n° of wines in sample') +
  scale_x_continuous(breaks = unique(redw_ds$quality))

grid.arrange(rw,ww,ncol=2)

#Red wines
# Heatmap to understand correlations between variables

red_cor <- cor(redw_ds[, c("fixed.acidity", "volatile.acidity", "citric.acid", "residual.sugar",
                              "chlorides", "free.sulfur.dioxide", "total.sulfur.dioxide",
                              "density", "pH", "sulphates", "alcohol", "quality")])
melted_red_cor <- melt(red_cor)


rw_heatmap <- ggplot(melted_red_cor, aes(x = Var1, y = Var2, fill = value)) +
  geom_tile() +
  scale_fill_gradient2(low = "white", mid = "orange", high = "#8B0000", midpoint = 0) +
  labs(title = "Correlation between red wine's variables",
       x = element_blank(),
       y = element_blank(),
       fill = "Correlation") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        plot.title = element_text(hjust = 0.5))

rw_heatmap

# Obtain correlation values to complement the heatmap
red_quality_corr <- subset(melted_red_cor, Var1 == "quality")
red_quality_corr[order(red_quality_corr$value), ]

# Line plot
red_alcohol <- ggplot(aes(x=quality,y=alcohol),
                      data = redw_ds)+
  stat_summary(geom = "line", fun = "median", aes(group = 1), color = "#8B0000", size = 1)+
  ggtitle('Alcohol by quality')+
  theme(plot.title = element_text(hjust = 0.5))

red_sulphates <- ggplot(aes(x=quality,y=sulphates),
                      data = redw_ds)+
  stat_summary(geom = "line", fun = "median", aes(group = 1), color = "#8B0000", size = 1)+
  ggtitle('Sulphates by quality')+
  theme(plot.title = element_text(hjust = 0.5))

red_citric_acid <- ggplot(aes(x=quality,y=citric.acid),
                      data = redw_ds)+
  stat_summary(geom = "line", fun = "median", aes(group = 1), color = "#8B0000", size = 1)+
  ggtitle('Citric acid by quality')+
  theme(plot.title = element_text(hjust = 0.5))

red_fixed_acidity <- ggplot(aes(x=quality,y=fixed.acidity),
                      data = redw_ds)+
  stat_summary(geom = "line", fun = "median", aes(group = 1), color = "#8B0000", size = 1)+
  ggtitle('Fixed acidity by quality')+
  theme(plot.title = element_text(hjust = 0.5))

grid.arrange(red_alcohol,red_sulphates, red_citric_acid, red_fixed_acidity,ncol=2)


# Heatmap to understand correlations between variables

white_cor <- cor(whitew_ds[, c("fixed.acidity", "volatile.acidity", "citric.acid", "residual.sugar",
                               "chlorides", "free.sulfur.dioxide", "total.sulfur.dioxide",
                               "density", "pH", "sulphates", "alcohol", "quality")])
melted_white_cor <- melt(white_cor)

ww_heatmap <- ggplot(melted_white_cor, aes(x = Var1, y = Var2, fill = value)) +
  geom_tile() +
  scale_fill_gradient2(low = "white", mid = "lightblue", high = "blue", midpoint = 0) +
  labs(title = "Correlation between white wine's variables",
       x = "Variable",
       y = "Variable",
       fill = "Correlation") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        plot.title = element_text(hjust = 0.5))

ww_heatmap

# Obtain correlation values to complement the heatmap

white_quality_corr <- subset(melted_white_cor, Var1 == "quality")
white_quality_corr[order(white_quality_corr$value), ]



# Line plot
white_alcohol <- ggplot(aes(x = quality,y=alcohol),
                      data = whitew_ds)+
  stat_summary(geom = "line", fun = "median", aes(group = 1), color = "#8B8B83", size = 1)+
  ggtitle('Alcohol by quality')+
  theme(plot.title = element_text(hjust = 0.5))

white_sulphates <- ggplot(aes(x = quality,y=sulphates),
                        data = whitew_ds)+
  stat_summary(geom = "line", fun = "median", aes(group = 1), color = "#8B8B83", size = 1)+
  ggtitle('Sulphates by quality')+
  theme(plot.title = element_text(hjust = 0.5))

white_pH <- ggplot(aes(x = quality,y=pH),
                          data = whitew_ds)+
  stat_summary(geom = "line", fun = "median", aes(group = 1), color = "#8B8B83", size = 1)+
  ggtitle('pH by quality')+
  theme(plot.title = element_text(hjust = 0.5))

white_free_sdiox <- ggplot(aes(x = quality,y=free.sulfur.dioxide),
                          data = whitew_ds)+
  stat_summary(geom = "line", fun = "median", aes(group = 1), color = "#8B8B83", size = 1)+
  ggtitle('Free sulfur dioxide by quality')+
  theme(plot.title = element_text(hjust = 0.5))

grid.arrange(white_alcohol,white_sulphates, white_pH, white_free_sdiox,ncol=2)


# pH to understand the distribution
red_ph <- ggplot(aes(x=pH),
                      data = redw_ds)+
  geom_histogram(color =I('black'),fill = I('#8B0000'))+
  ggtitle('pH distribution for red wines')+
  xlab('pH') +
  ylab('Frequence') +
  theme_minimal() +
  coord_cartesian(xlim = c(2.5, 4))+
  theme(plot.title = element_text(hjust = 0.5))

ww_ph <- ggplot(aes(x=pH),
                data = whitew_ds)+
  geom_histogram(color =I('black'),fill = I('#FFFFE0'))+
  ggtitle('pH distribution for white wines')+
  xlab('pH') +
  ylab('Frequence') +
  theme_minimal() +
  coord_cartesian(xlim = c(2.5, 4))

grid.arrange(red_ph,ww_ph,ncol=2)

# Statistic measures of pH in each type
ph_stats <- wine2_ds %>%
  group_by(type) %>%
  summarize(
    mean = mean(pH),
    median = median(pH),
    sd = sd(pH)
    )
ph_stats


# Correlation between alcohol and residual sugar

alcohol_sugar <- ggplot(wine2_ds, aes(x = residual.sugar, y = alcohol, color = type)) +
  geom_point() +
  ggtitle('Alcohol vs Residual Sugar') +
  xlab('Residual sugar') +
  ylab('Alcohol') +
  theme_minimal()+
  coord_cartesian(xlim = c(0, 33))+
  theme(plot.title = element_text(hjust = 0.5))

alcohol_sugar

# Correlation between alcohol and sulphates

alcohol_sulphates <- ggplot(wine2_ds, aes(x = sulphates, y = alcohol, color = type)) +
  geom_point() +
  ggtitle('Alcohol vs Sulphates') +
  xlab('Sulphates') +
  ylab('Alcohol') +
  theme_minimal()+
  theme(plot.title = element_text(hjust = 0.5))

alcohol_sulphates

# Correlation between total sulfur dioxide and free sulfur dioxide

sulfur_dioxide <- ggplot(wine2_ds, aes(x = total.sulfur.dioxide, y = free.sulfur.dioxide, color = type)) +
  geom_point() +
  ggtitle('Total vs Free Sulfur Dioxide ') +
  xlab('Total sulfur dioxide') +
  ylab('Free sulfur dioxide') +
  theme_minimal()+
  coord_cartesian(xlim = c(0, 380), ylim = c(0, 150))+
  theme(plot.title = element_text(hjust = 0.5))

sulfur_dioxide

# Correlation between chlorides, sulphates and pH
red_csp_corr <- pairs(~pH + sulphates + chlorides, data = redw_ds,
                     main = 'pH, sulphates and chlorides correlations in red wines')
red_csp_corr

white_csp_corr <- pairs(~pH + sulphates + chlorides, data = whitew_ds,
                      main = 'pH, sulphates and chlorides correlations in white wines')
white_csp_corr


# Correlation and test
red_var <- setdiff(names(redw_ds), 'type')


red_cor_func <- function(x) {
  result <- cor.test(x, redw_ds$quality, method = 'pearson')
  return(list(corr = cor(x, redw_ds$quality), p_value = result$p.value))
}

red_cor_results <- lapply(redw_ds[red_var], red_cor_func)

df_red_cor <- do.call(rbind, correlation_results)

df_red_cor

