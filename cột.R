library(readr)
library(dplyr)
library(ggplot2)

doc <- read_csv('D:/HOCMAY/stockx.csv')

nike_count <- doc %>%
  group_by(shoes_name, release_year) %>%
  summarize(count = n(), .groups = "drop") %>%
  tidyr::pivot_wider(names_from = release_year, values_from = count, values_fill = 0)


sales_by_year <- colSums(nike_count[, -1])
years <- as.numeric(colnames(nike_count)[-1])

# Đoạn code của bạn ở trên

ggplot() +
  geom_line(aes(x = as.factor(years), y = sales_by_year), color = 'red', size = 1) +
  geom_point(aes(x = as.factor(years), y = sales_by_year), color = 'red', size = 3) +
  geom_path(aes(x = as.factor(years), y = sales_by_year), color = 'blue', size = 1, linetype = "dotted") +
  geom_bar(stat = 'identity', aes(x = as.factor(years), y = sales_by_year), fill = 'gray', width = 0.5) +
  labs(title = 'Tổng số giày bán được qua các năm', x = 'Năm', y = 'Tổng số giày bán được qua các năm') +
  scale_x_discrete(breaks = as.factor(years)) +
  theme_minimal() +
  theme(axis.text.x = element_text(hjust = 1)) +
  geom_text(aes(label = sales_by_year, x = as.factor(years), y = sales_by_year), vjust = -0.5, size = 4, color = 'black', position = position_dodge(width = 0.9))

