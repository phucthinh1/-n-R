library(readr)
library(dplyr)
library(ggplot2)
library(stats)
library(stats4)
library(tidyr)
library(readxl)

# Đọc dữ liệu từ file CSV
doc <- read_excel('D:/HOCMAY/stockx.xlsx')
print(doc)


# In ra số giày Nike bán được
nike <- doc %>%
  filter(brand == 'Nike') %>%
  count(brand, release_year, name = 'nike_count')

# In ra bảng thống kê số lượng giày Nike theo năm
nike_count <- doc %>%
  count(brand, release_year) %>%
  spread(key = release_year, value = n, fill = 0)

print(nike_count)

# In ra tổng số giày Nike trong năm 2022
total_nike_2022 <- nike_count %>%
  filter(brand == 'Nike') %>%
  select(`2022`) %>%
  unlist() %>%
  as.numeric()

print(paste("Số lượng giày Nike trong năm 2022 là:", total_nike_2022))

# In ra tổng số giày trong năm 2022
print(paste("Số lượng giày trong năm 2022 là:", sum(nike_count$`2022`)))

# Số lượng giày trong năm 2023
all_2023 <- doc %>%
  filter(release_year == 2023)

print(paste("Số lượng giày trong năm 2023 là:", nrow(all_2023)))

# Thiết lập hạt giống cho số ngẫu nhiên
set.seed(100)

# Lấy ngẫu nhiên các chỉ số từ tập dữ liệu
a <- sample(1:nrow(all_2023), 1500)

# Tạo DataFrame trống để lưu mẫu
mau <- data.frame()

# Lặp qua các chỉ số và thêm hàng tương ứng vào DataFrame mẫu
for (i in a) {
  mau <- rbind(mau, all_2023[i, ])
}

print(paste("Ta thu thập ngẫu nhiên 1500 mẫu trên tập dữ liệu lớn:", nrow(mau)))
print(paste("Trong 1500 mẫu, ta có được:", nrow(mau[mau$brand=='Nike', ]), "dòng thỏa mãn điều kiện là giày Nike."))

# Lấy số lượng mẫu và số lượng giày Nike trong mẫu
n <- nrow(mau)
count <- nrow(mau[mau$brand=='Nike', ])

# Giả thuyết H0
alpha <- 0.24

# Thực hiện kiểm định z-score
result <- prop.test(count, n, p = alpha, alternative = "less")

# In giá trị z-score và p-value
print(paste("Z-score:", result$statistic))
print(paste("P-value:", result$p.value))

# Kiểm tra giả thuyết null
alpha <- 0.05
if (result$p.value < alpha) {
  print("Có bằng chứng để bác bỏ giả thuyết h0,tỉ lệ bán giày năm 2023 đã giảm")
} else {
  print("Chưa có bằng chứng để bác bỏ H0, tức là chưa có cơ sở thừa nhận tỉ lệ bán giày năm 2023 đã giảm")
}

