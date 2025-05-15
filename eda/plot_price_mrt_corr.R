# 載入必要套件
library(ggplot2)

setwd("/Users/bbd/DS/1132_Data_Science_Final")

plot_corr <- function(file_path, out_pdf, title) {
  df <- read.csv(file_path)
  # 移除缺失值
  df <- df[!is.na(df$單價元平方公尺) & !is.na(df$捷運站距離.公尺.), ]
  # 計算皮爾森相關係數
  corr <- cor(df$單價元平方公尺, df$捷運站距離.公尺., method = "pearson")
  # 畫散布圖並加回歸線
  p <- ggplot(df, aes(x = 捷運站距離.公尺., y = 單價元平方公尺)) +
    geom_point(alpha = 0.5, color = "#0072B2") +
    geom_smooth(method = "lm", se = TRUE, color = "red", linetype = "dashed") +
    labs(title = title,
         x = "MRT Distance (meters)",
         y = "Unit Price (NTD/sqm)",
         subtitle = paste0("Pearson correlation: ", round(corr, 3))) +
    theme_minimal() +
    theme(plot.title = element_text(hjust = 0.5),
          plot.subtitle = element_text(hjust = 0.5))
  print(p)
  pdf(out_pdf, width = 8, height = 6)
  print(p)
  dev.off()
  cat(title, "correlation:", round(corr, 3), "\n")
}

plot_corr("eda/rent_trades_w_mrt.csv", "dataset/price_mrt_scatter_rent.pdf", "Rental Trades: Unit Price vs. MRT Distance")
plot_corr("eda/other_trades_w_mrt.csv", "dataset/price_mrt_scatter_other.pdf", "Other Trades: Unit Price vs. MRT Distance") 