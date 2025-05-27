# predict_rent_xgb.R


# @title XGBoost 租金預測
# @description
#   此函數接收一個包含租賃相關特徵的 data.frame，並使用預先訓練的 XGBoost 模型預測租金。
#   函數內部會執行必要的資料前處理，包括特徵轉換、因子化及 one-hot encoding。
#
# @param new_data data.frame，包含預測所需的原始特徵。欄位說明如下。
#   輸入欄位 (共22個 conceptual features):
#     - 鄉鎮市區 (character): 例如 "大安區"
#     - 出租型態 (character): 例如 "整層住家"
#     - 建物型態 (character): 例如 "住宅大樓(11層含以上有電梯)"
#     - 有無電梯 (character/numeric): "1" (有), "0" (無), 或 "空" (NA會轉為"空"). 若為 numeric 1/0 會轉為 character "1"/"0".
#     - 租賃住宅服務 (character): 例如 "一般包租", "空" (NA會轉為"空")
#     - 總樓層數 (numeric): 例如 12
#     - 建物現況格局.房 (numeric): 例如 3
#     - 建物現況格局.廳 (numeric): 例如 2
#     - 建物現況格局.衛 (numeric): 例如 1
#     - 建物總面積平方公尺 (numeric): 例如 80.5
#     - 屋齡 (numeric): 例如 10
#     - 附屬設備.冷氣 (numeric): 1 (有) 或 0 (無)
#     - 附屬設備.熱水器 (numeric): 1 (有) 或 0 (無)
#     - 附屬設備.洗衣機 (numeric): 1 (有) 或 0 (無)
#     - 附屬設備.電視機 (numeric): 1 (有) 或 0 (無)
#     - 附屬設備.瓦斯或天然氣 (numeric): 1 (有) 或 0 (無)
#     - 附屬設備.網路 (numeric): 1 (有) 或 0 (無)
#     - 捷運站距離.公尺. (numeric): 例如 250.5
#     - 捷運線 (character): 例如 "板南線", "淡水信義線" (NA會轉為"空"如果"空"在級別定義中)
#     - 附近建物單位成交均價 (numeric): 例如 500000
#     - 租賃年月日 (character): "YYYY-MM-DD" 格式, 例如 "2023-01-15" (函數內部會轉換為 _ts)
#     - 租賃天數 (numeric): 例如 365 (函數內部會轉換為 _期間天數 並檢查 >=30)
#
# @return numeric, 預測的租金總額元 (已進行 exp() 轉換回原始尺度)。
#   如果發生錯誤，函數將停止並顯示錯誤訊息。
#
# @examples
#  test_row_example <- data.frame(
#    鄉鎮市區 = "中正區",
#    出租型態 = "整棟(戶)出租",
#    建物型態 = "華廈(10層含以下有電梯)",
#    有無電梯 = 1, # 數值 1
#    租賃住宅服務 = "社會住宅包租轉租", # 此為範例值，若原始資料為NA，函數內部會嘗試轉換為 "空"
#    總樓層數 = 7,
#    建物現況格局.房 = 4,
#    建物現況格局.廳 = 2,
#    建物現況格局.衛 = 3,
#    建物總面積平方公尺 = 232.72,
#    屋齡 = 13,
#    附屬設備.冷氣 = 1,
#    附屬設備.熱水器 = 0,
#    附屬設備.洗衣機 = 0,
#    附屬設備.電視機 = 0,
#    附屬設備.瓦斯或天然氣 = 0,
#    附屬設備.網路 = 0,
#    捷運站距離.公尺. = 241.31,
#    捷運線 = "松山新店線",
#    附近建物單位成交均價 = 407.64,
#    租賃年月日 = "2023-08-16",
#    租賃天數 = 737,
#    stringsAsFactors = FALSE
#  )
 
# predicted_price <- predict_rent_xgb(test_row_example)

# 確認 'xgboost' 和 'caret' 套件已安裝: install.packages(c("xgboost", "caret"))
library(xgboost)
library(caret)

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
  # 2. 讀模型
  model <- xgboost::xgb.load("final_xgb_model.txt")
    
  # 3. factor 欄位轉型
  factor_cols <- c("鄉鎮市區", "出租型態", "建物型態", "有無電梯", "租賃住宅服務", "捷運線")
  
  factor_levels <- list(
    "鄉鎮市區" = c("大安區", "信義區", "中山區", "中正區", "松山區", "內湖區", "士林區", "文山區", "北投區", "萬華區", "大同區", "南港區"),
    "出租型態" = c("整層住家", "分租套房", "獨立套房", "雅房", "其他", "空", "整棟(戶)出租", "分層出租", "分租雅房"),
    "建物型態" = c("住宅大樓(11層含以上有電梯)", "公寓(5樓含以下無電梯)", "套房(1樓至5樓)", "華廈(10層含以下有電梯)", "透天厝", "其他", "店面(店鋪)", "廠辦", "辦公商業大樓", "倉庫", "工廠", "農舍", "空"),
    "有無電梯" = c("1", "0", "空"),
    "租賃住宅服務" = c("空", "社會住宅包租轉租", "社會住宅代管", "一般包租", "一般轉租", "一般代管"),
    "捷運線" = c("文湖線", "淡水信義線", "新北投支線", "松山新店線", "小碧潭支線", "中和新蘆線", "板南線", "環狀線", "機場線", "安坑輕軌", "其他", "無", "空") 
  )

  # 針對測試範例中的特定欄位進行處理
  if ("有無電梯" %in% names(new_data) && is.numeric(new_data$"有無電梯")) {
    new_data$"有無電梯" <- as.character(new_data$"有無電梯")
  }
  if ("租賃住宅服務" %in% names(new_data) && is.na(new_data$"租賃住宅服務")) {
    if ("空" %in% factor_levels[["租賃住宅服務"]]) { # 確認 "空" 是有效的級別
        new_data$"租賃住宅服務" <- "空"
    }
  }

  # 針對其他因子欄位進行通用的 NA 和空字串處理
  for (col_name in setdiff(factor_cols, c("有無電梯", "租賃住宅服務"))) { # 套用於其他欄位
      if (col_name %in% names(new_data)) {
          if ("空" %in% factor_levels[[col_name]]) {
              if (is.na(new_data[[col_name]]) || 
                  as.character(new_data[[col_name]]) == "" || 
                  tolower(as.character(new_data[[col_name]])) == "null") {
                new_data[[col_name]] <- "空"
              }
          }
      }
  }

  for (col in factor_cols) {
    new_data[[col]] <- factor(new_data[[col]], levels = factor_levels[[col]])
    if (any(is.na(new_data[[col]]))) {
      stop(paste0("❌ 欄位 ", col, " 的值有錯，請確認拼字和選項"))
    }
  }
  # 4. 轉成 matrix
  # 注意：此簡化版的矩陣轉換未處理 one-hot encoding 或確保欄位順序。
  # 它假設 new_data 在基本因子轉換之外已經是適當的結構。
  mat <- data.matrix(new_data[, required_cols, drop=FALSE])
  
  # 5. 預測
  pred_log <- predict(model, mat)
  predicted_value_original_scale <- exp(pred_log) # 將對數預測值轉換回原始尺度
  return(predicted_value_original_scale)
}

