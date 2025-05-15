# 載入必要套件
library(dplyr)

# 設定工作目錄
setwd("/Users/bbd/DS/1132_Data_Science_Final")

# 處理檔案的 function（將距離取 log）
do_log_transform <- function(file_path) {
  df <- read.csv(file_path)
  
  # 新增對數轉換欄位（處理 NA）
  df$distance_to_log <- log1p(df$捷運站距離.公尺.)
  
  # 存回原檔案（如需另存新檔可修改 file_path）
  write.csv(df, file_path, row.names = FALSE)
  cat("Done:", file_path, "\n")
}

# 套用到兩個檔案
do_log_transform("dataset/rent_trades_w_mrt.csv")
do_log_transform("dataset/other_trades_w_mrt.csv")
