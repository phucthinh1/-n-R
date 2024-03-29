# Steps to draw a Heat map
# Step 1: Import package
library(dplyr)
library(tidyverse)
library(ggplot2)

library(corrplot)

# Step 2: Load Data 
data <- read.csv('D:/HOCMAY/stockx.csv')

print(head(data, 10))

# Step 3: Draw Heatmap chart
numeric_columns <- df %>% select_if(is.numeric) %>% colnames()
correlation_matrix <- cor(df[numeric_columns])

options(repr.plot.width=20, repr.plot.height=9)  
corrplot(correlation_matrix, method="color", col=colorRampPalette(c("navy", "white", "darkred"))(20), addCoef.col="black", number.cex=0.7, tl.col="black")
title("Biểu đồ tương quan giữa các biến", cex.main=2, font.main=2)

