# 安裝並載入必要的套件
if (!require("ggplot2")) install.packages("ggplot2")
if (!require("gridExtra")) install.packages("gridExtra")
if (!require("moments")) install.packages("moments")

library(ggplot2)
library(gridExtra)
library(moments)

# 讀取資料
rent_trades <- read.csv(
  file        = "dataset/rent_trades_w_mrt.csv",
  header      = TRUE,
  stringsAsFactors = FALSE,
  fileEncoding = "UTF-8"
)

# 繪製原始租金的分布圖
p1 <- ggplot(rent_trades, aes(x = 單價元平方公尺)) +
  geom_histogram(aes(y = ..density..), bins = 30, fill = "lightblue", color = "black") +
  geom_density(color = "red", linewidth = 1) +
  labs(
    title = "租金分布圖 (原始數據)",
    x = "租金 (元/平方公尺)",
    y = "密度"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, size = 14),
    axis.title = element_text(size = 12),
    axis.text = element_text(size = 10)
  )

# 繪製對數轉換後租金的分布圖
p2 <- ggplot(rent_trades, aes(x = log(單價元平方公尺, base = 10))) +
  geom_histogram(aes(y = ..density..), bins = 30, fill = "lightgreen", color = "black") +
  geom_density(color = "red", linewidth = 1) +
  labs(
    title = "租金分布圖 (對數轉換後)",
    x = "log(租金) (元/平方公尺)",
    y = "密度"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, size = 14),
    axis.title = element_text(size = 12),
    axis.text = element_text(size = 10)
  )

# 將兩個圖並排顯示並保存
pdf("model/rent_distribution.pdf", width = 12, height = 6)
grid.arrange(p1, p2, ncol = 2)
dev.off()

# 輸出基本統計量
cat("\n原始租金的基本統計量：\n")
print(summary(rent_trades$單價元平方公尺))

cat("\n對數轉換後租金的基本統計量：\n")
print(summary(log(rent_trades$單價元平方公尺, base = 10)))

# 計算偏態係數
cat("\n原始租金的偏態係數：", skewness(rent_trades$單價元平方公尺), "\n")
cat("對數轉換後租金的偏態係數：", skewness(log(rent_trades$單價元平方公尺, base = 10)), "\n") 