library(readr)
library(dplyr)
library(Hmisc)
library(stats)

# Đọc dữ liệu từ file CSV
data <- read_csv('D:/HOCMAY/1500stockX.csv')

# Lấy mẫu ngẫu nhiên 1400 mẫu
set.seed(12345) # Để đảm bảo kết quả tái lặp được
mau <- sample_n(data, 1400, replace = FALSE)

# Số người bán Adidas
so_nguoi_ban_adidas <- mau %>% filter(brands == 'adidas') %>% nrow()
cat("Số người bán Adidas:", so_nguoi_ban_adidas, "\n")

# Số mẫu
so_mau <- nrow(mau)
cat("Số mẫu:", so_mau, "\n")

# Ước lượng tỉ lệ
khoang_tin_cay <- binconf(so_nguoi_ban_adidas, so_mau, method = "wilson")
cat("Khoảng tin cậy:", khoang_tin_cay[1], khoang_tin_cay[2])

