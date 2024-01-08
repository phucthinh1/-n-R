# BƯớc 1 : Nhập thư viện
library(dplyr)
library(tidyverse)
library(ggplot2)
library(caret)

#Bước 2 : Nhập dữ liệu
data <- read.csv('D:/HOCMAY/stockx.csv')

print(head(data, 10))

# Bước 3 : Chọn biến phụ thuộc và biến độc lập
features <- c('release_month', 'release_year', 'retail_price', 'volatility', 'number_of_sales', 'price_premium', 'avg_sale_price')
target <- 'last_sale'

set.seed(123)
trainIndex <- createDataPartition(data$last_sale, p = 0.7, 
                                  list = FALSE, 
                                  times = 1)

train_data <- data[trainIndex, ]
test_data <- data[-trainIndex, ]

# Bước 4 : chia dữ liệu thành tập huấn 
model <- lm(paste(target, "~ ."), data = train_data)

# Bước 5 : huấn luyện trên tập dự đoán
predictions <- predict(model, newdata = test_data)

# BƯớc 6 : tính toán độ chính xác
eval_metrics <- function(y_true, y_pred) {
  mae <- mean(abs(y_pred - y_true))
  rmse <- sqrt(mean((y_pred - y_true)^2))
  r2 <- cor(y_pred, y_true)^2
  
  return(c(MAE = mae, RMSE = rmse, R2 = r2))
}

metrics <- eval_metrics(test_data[[target]], predictions)
cat("Mean Absolute Error:", metrics["MAE"], "\n")
cat("Root Mean Squared Error:", metrics["RMSE"], "\n")
cat("R-squared:", metrics["R2"], "\n")

#Bước 7 : Vẽ mô hình
gg <- ggplot(plot_data, aes(x = Actual, y = Predicted)) +
  geom_point(color = 'green', size = 3, alpha = 0.7) +
  geom_abline(intercept = 0, slope = 1, linetype = "dashed", color = "blue", size = 1) +
  labs(x = 'Giá trị thực tế', y = 'Giá trị dự đoán', 
       title = 'Biểu đồ QQ-plot cho mô hình hồi quy tuyến tính') +
  theme_minimal()

plot(gg)


