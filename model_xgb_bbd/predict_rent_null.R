
predict_rent_xgb <- function(new_data) {
  # 1. 檢查 input 欄位是否正確
  required_cols <- c(
    "鄉鎮市區", "出租型態", "建物型態", "有無電梯", "租賃住宅服務",
    "總樓層數", "建物現況格局.房", "建物現況格局.廳", "建物現況格局.衛",
    "建物總面積平方公尺", "屋齡", 
    "附屬設備.冷氣", "附屬設備.熱水器", "附屬設備.洗衣機", "附屬設備.電視機",
    "附屬設備.瓦斯或天然氣", "附屬設備.網路",
    "捷運站距離.公尺.", "捷運線", "附近建物單位成交均價",
    "租賃年月日", "租賃天數"
  )
  if (!all(required_cols %in% names(new_data))) {
    missing_input_cols <- setdiff(required_cols, names(new_data))
    stop(paste0("❌ Input 'new_data' 缺少以下 conceptual input 欄位: ", paste(missing_input_cols, collapse = ", ")))
  }
  
  # 計算並回傳指定欄位的乘積
  calculated_value <- new_data$"附近建物單位成交均價" * new_data$"建物總面積平方公尺"
  return(calculated_value)
}


 test_row_example <- data.frame(
   鄉鎮市區 = "中正區",
   出租型態 = "整棟(戶)出租",
   建物型態 = "華廈(10層含以下有電梯)",
   有無電梯 = 1, # 數值 1
   租賃住宅服務 = "社會住宅包租轉租", # 此為範例值，若原始資料為NA，函數內部會嘗試轉換為 "空"
   總樓層數 = 7,
   建物現況格局.房 = 4,
   建物現況格局.廳 = 2,
   建物現況格局.衛 = 3,
   建物總面積平方公尺 = 232.72,
   屋齡 = 13,
   附屬設備.冷氣 = 1,
   附屬設備.熱水器 = 0,
   附屬設備.洗衣機 = 0,
   附屬設備.電視機 = 0,
   附屬設備.瓦斯或天然氣 = 0,
   附屬設備.網路 = 0,
   捷運站距離.公尺. = 241.31,
   捷運線 = "松山新店線",
   附近建物單位成交均價 = 407.64,
   租賃年月日 = "2023-08-16",
   租賃天數 = 737,
   stringsAsFactors = FALSE
 )

 predicted_price <- predict_rent_xgb(test_row_example)
 print(predicted_price)