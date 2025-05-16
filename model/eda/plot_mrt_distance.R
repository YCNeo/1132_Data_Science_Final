# 載入必要的套件
library(ggplot2)
library(dplyr)
library(patchwork)

# 設定工作目錄
setwd("/Users/bbd/DS/1132_Data_Science_Final")

# 讀取資料
other_trades <- read.csv("dataset/other_trades_w_mrt.csv")
rent_trades <- read.csv("dataset/rent_trades_w_mrt.csv")

# 為資料集添加標籤
other_trades$type <- "Other Trades"
rent_trades$type <- "Rental Trades"

# 合併資料
combined_data <- rbind(
  other_trades %>% select(捷運站距離.公尺., type),
  rent_trades %>% select(捷運站距離.公尺., type)
)

# 繪製箱型圖
p1 <- ggplot(other_trades, aes(y = 捷運站距離.公尺.)) +
  geom_boxplot(fill = "lightblue", alpha = 0.7) +
  labs(title = "Other Trades - MRT Distance Distribution",
       y = "Distance (meters)") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5))

p2 <- ggplot(rent_trades, aes(y = 捷運站距離.公尺.)) +
  geom_boxplot(fill = "lightgreen", alpha = 0.7) +
  labs(title = "Rental Trades - MRT Distance Distribution",
       y = "Distance (meters)") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5))

# 並排顯示兩個箱型圖
combined_plot <- p1 + p2
print(combined_plot)

# 新增直方圖和密度圖
hist1 <- ggplot(other_trades, aes(x = 捷運站距離.公尺.)) +
  geom_histogram(binwidth = 100, fill = "lightblue", color = "black", alpha = 0.7) +
  labs(title = "Other Trades - MRT Distance Histogram", x = "Distance (meters)", y = "Count") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5))

dens1 <- ggplot(other_trades, aes(x = 捷運站距離.公尺.)) +
  geom_density(fill = "lightblue", alpha = 0.5) +
  labs(title = "Other Trades - MRT Distance Density", x = "Distance (meters)", y = "Density") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5))

hist2 <- ggplot(rent_trades, aes(x = 捷運站距離.公尺.)) +
  geom_histogram(binwidth = 100, fill = "lightgreen", color = "black", alpha = 0.7) +
  labs(title = "Rental Trades - MRT Distance Histogram", x = "Distance (meters)", y = "Count") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5))

dens2 <- ggplot(rent_trades, aes(x = 捷運站距離.公尺.)) +
  geom_density(fill = "lightgreen", alpha = 0.5) +
  labs(title = "Rental Trades - MRT Distance Density", x = "Distance (meters)", y = "Density") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5))

# 並排顯示直方圖和密度圖
hist_density_plot <- (hist1 | dens1) / (hist2 | dens2)
print(hist_density_plot)

# 計算基本統計量
cat("\nOther Trades - MRT Distance Summary:\n")
print(summary(other_trades$捷運站距離.公尺.))
cat("\nRental Trades - MRT Distance Summary:\n")
print(summary(rent_trades$捷運站距離.公尺.))

# 計算缺失值
cat("\nMissing Value Statistics:\n")
cat("Other Trades - Missing count:", sum(is.na(other_trades$捷運站距離.公尺.)), "\n")
cat("Other Trades - Missing percent:", 
    round(sum(is.na(other_trades$捷運站距離.公尺.)) / nrow(other_trades) * 100, 2), "%\n")
cat("Rental Trades - Missing count:", sum(is.na(rent_trades$捷運站距離.公尺.)), "\n")
cat("Rental Trades - Missing percent:", 
    round(sum(is.na(rent_trades$捷運站距離.公尺.)) / nrow(rent_trades) * 100, 2), "%\n")

# 儲存箱型圖
pdf("eda/mrt_distance_boxplots.pdf", width = 12, height = 6)
print(combined_plot)
dev.off()

# 儲存直方圖和密度圖
pdf("eda/mrt_distance_hist_density.pdf", width = 12, height = 10)
print(hist_density_plot)
dev.off() 