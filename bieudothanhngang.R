library(tidyverse)

# Load data
doc <- read.csv('D:/HOCMAY/stockx.csv')

# Subset data for the year 2023
year <- doc[doc$release_year == 2023,]

# Create a pivot table
brandtopyear <- year %>%
  group_by(brands) %>%
  summarise(avg_sale_price = sum(avg_sale_price)) %>%
  arrange(desc(avg_sale_price))
brandtopyear

# Create bar plot
ggplot(data = brandtopyear, aes(x = reorder(brands, avg_sale_price), y = avg_sale_price)) +
  geom_bar(stat = 'identity', fill = 'red') +
  labs(x = 'Thương hiệu', y = 'Tổng doanh thu', title = 'Tổng doanh thu bán giày theo từng thương hiệu năm 2023') +
  theme_minimal() +
  coord_flip() +
  scale_y_continuous(labels = scales::comma)


