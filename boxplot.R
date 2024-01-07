library(readr)
library(dplyr)
library(ggplot2)
library(RColorBrewer)

doc <- read_csv('D:/HOCMAY/stockx.csv')

sosanh <- doc %>%
  select(max_12_month_trade_rage, max_all_trade_range) %>%
  rename(`Chỉ năm 2023` = max_12_month_trade_rage, `Toàn thời gian` = max_all_trade_range)

Q1 <- sosanh %>%
  summarise_all(quantile, probs = 0.25) %>%
  as.numeric()

Q3 <- sosanh %>%
  summarise_all(quantile, probs = 0.75) %>%
  as.numeric()

IQR <- Q3 - Q1
lower_bound <- Q1 - 1.5 * IQR
upper_bound <- Q3 + 1.5 * IQR

sosanh_no_outliers <- sosanh %>%
  filter(
    `Chỉ năm 2023` >= lower_bound[1] & `Chỉ năm 2023` <= upper_bound[1] &
      `Toàn thời gian` >= lower_bound[2] & `Giá cao nhất trước 2023` <= upper_bound[2]
  ) %>%
  drop_na()

# Vẽ biểu đồ boxplot cho 'Chỉ năm 2023' và 'Toàn thời gian'
ggplot() +
  geom_boxplot(data = sosanh_no_outliers, aes(x = factor(1), y = `Chỉ năm 2023`), fill = brewer.pal(10, "Set3")[1]) +
  geom_boxplot(data = sosanh_no_outliers, aes(x = factor(2), y = `Toàn thời gian`), fill = brewer.pal(10, "Set3")[2]) +
  labs(x = 'Loại giá', y = 'Giá Trị (USD)', title = 'Biểu đồ thống kê chênh lệch giá bán giày cao nhất của năm 2023 và giá bán cao nhất của thời điểm trước 2023') +
  scale_x_discrete(labels = c('Chỉ năm 2023', 'Toàn thời gian')) +
  theme_minimal()
