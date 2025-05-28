library(dplyr)
library(lightgbm)
library(readr)

# @title XGBoost 租金預測
# @description
# 直接丟 data.frame 進來就行（欄位如下，型態用字串或數字都可以）
# input: data.frame，欄位順序如下
# test_row <- data.frame(
#   "鄉鎮市區" = "中山區",
#   "租賃年月日" = 18790,
#   "出租型態" = "獨立套房",
#   "總樓層數" = 8,
#   "建物型態" = "華廈(10層含以下有電梯)",
#   "租賃住宅服務" = "社會住宅代管",
#   "租賃天數" = 364,
#   "有無電梯" = 1,
#   "建物現況格局-房" = 1,
#   "建物現況格局-廳" = 1,
#   "建物現況格局-衛" = 1,
#   "建物總面積平方公尺" = 19.15,
#   "屋齡" = 12,
#   "附屬設備-冷氣" = 1,
#   "附屬設備-熱水器" = 1,
#   "附屬設備-洗衣機" = 0,
#   "附屬設備-電視機" = 1,
#   "附屬設備-瓦斯或天然氣" = 0,
#   "附屬設備-有線電視" = 0,
#   "附屬設備-網路" = 0,
#   "捷運站距離(公尺)" = 604.12,
#   "附近建物單位成交均價" = 467.4000,
#   "捷運線" = "松山新店線",
#   stringsAsFactors = FALSE,
#   check.names = FALSE
#   ...
# )
# output: 預測的租金（單位：元）
# predict_rent_xgb(test_row)

# test_row <- data.frame(
#   "建物總面積平方公尺" = 232.72,
#   "租賃住宅服務" = "未知", # NA 填未知
#   "建物型態" = "華廈(10層含以下有電梯)",
#   "屋齡" = 13,
#   "附近建物單位成交均價" = 407.64,
#   "鄉鎮市區" = "信義區",
#   "建物現況格局-房" = 4,
#   "捷運站距離(公尺)" = 241.31,
#   "租賃天數" = 737,
#   "出租型態" = "整棟(戶)出租",
#   "建物現況格局-衛" = 3,
#   "建物現況格局-廳" = 2,
#   "附屬設備-熱水器" = 1,
#   "租賃年月日" = 20001, # 轉數字，例如：19434
#   "總樓層數" = 7,
#   "附屬設備-網路" = 0,
#   "附屬設備-冷氣" = 1,
#   "捷運線" = "松山新店線",
#   "附屬設備-電視機" = 0,
#   "附屬設備-有線電視" = 0,
#   "附屬設備-瓦斯或天然氣" = 0,
#   "附屬設備-洗衣機" = 0,
#   "有無管理組織" = 0,
#   "交易筆棟數-建物" = 1,
#   "有無電梯" = 1,
#   "附屬設備-冰箱" = 0,
#   "有無附傢俱" = 0,
#   "建材分類" = "鋼筋混凝土造類",
#   "有無管理員" = 0,
#   "交易筆棟數-土地" = 0,
#   "建物現況格局-隔間" = 0,
#   "租賃層次(四類)" = "未知",  # 沒有資料就填 "未知"
#   stringsAsFactors = FALSE,
#   check.names = FALSE # 保留你自己指定的欄位名稱 TRUE 會把 dash（-）、括號、空白都換成點 .，有時還會補數字或底線
# )
test_row <- data.frame(
  "鄉鎮市區" = "士林區",
  "租賃年月日" = 19602,
  "出租型態" = "未知",
  "租賃層次(四類)" = "低樓層",
  "總樓層數" = 4,
  "建物型態" = "公寓(5樓含以下無電梯)",
  "交易筆棟數-土地" = 1,
  "交易筆棟數-建物" = 1,
  "租賃住宅服務" = "未知",
  "租賃天數" = 1826,
  "有無管理組織" = 0,
  "有無管理員" = 0,
  "有無附傢俱" = 0,
  "有無電梯" = 0,
  "建物現況格局-房" = 1,
  "建物現況格局-廳" = 1,
  "建物現況格局-衛" = 1,
  "建物現況格局-隔間" = 1,
  "建物總面積平方公尺" = 67.35,
  "屋齡" = 50,
  "建材分類" = "鋼筋混凝土造類",
  "附屬設備-冷氣" = 0,
  "附屬設備-熱水器" = 0,
  "附屬設備-洗衣機" = 0,
  "附屬設備-電視機" = 0,
  "附屬設備-冰箱" = 0,
  "附屬設備-瓦斯或天然氣" = 0,
  "附屬設備-有線電視" = 0,
  "附屬設備-網路" = 0,
  "捷運站距離(公尺)" = 215.07802,
  "附近建物單位成交均價" = 633.3333,
  "捷運線" = "淡水信義線",
  stringsAsFactors = FALSE,
  check.names = FALSE
)


predict_rent_lgbm <- function(input_row, model_path = "final_lgbm_model.txt",
                              feature_order_path = "lgbm_feature_order.txt",
                              factor_level_path = "factor_levels.rds") {

  # 1. 讀 feature 欄位順序（照訓練存下的）
  feature_order <- readLines(feature_order_path)
  input_row <- input_row[, feature_order, drop=FALSE]
  colnames(input_row) <- feature_order

  # 2. 讀 factor levels（照訓練存下的）
  factor_levels <- readRDS(factor_level_path)
  categorical_cols <- names(factor_levels)
  for (col in categorical_cols) {
    input_row[[col]] <- factor(as.character(input_row[[col]]), levels = factor_levels[[col]])
  }

  # 3. 檢查有沒有非法值（新 level 會被轉成 NA）
  for (col in categorical_cols) {
    if (any(is.na(input_row[[col]]))) {
      stop(sprintf("❌ 欄位 %s 的值有錯：%s\n可選值為：%s",
        col, 
        paste(input_row[[col]][is.na(input_row[[col]])], collapse = ", "),
        paste(factor_levels[[col]], collapse = ", ")
      ))
    }
  }

  # 4. 轉成數值矩陣
  test_mat <- data.matrix(input_row)
  colnames(test_mat) <- feature_order

  # 5. 載入模型 & 預測
  model <- lightgbm::lgb.load(model_path)
  pred_log <- predict(model, test_mat)
  pred <- exp(pred_log)  # 還原為租金
  return(pred)
}
predict_rent_lgbm(test_row)