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
# )
# output: 預測的租金（單位：元）
# predict_rent_xgb(test_row)

test_row <- data.frame(
  "鄉鎮市區" = "中山區",
  "租賃年月日" = 18790,
  "出租型態" = "獨立套房",
  "總樓層數" = 8,
  "建物型態" = "華廈(10層含以下有電梯)",
  "租賃住宅服務" = "社會住宅代管",
  "租賃天數" = 364,
  "有無電梯" = 1,
  "建物現況格局-房" = 1,
  "建物現況格局-廳" = 1,
  "建物現況格局-衛" = 1,
  "建物總面積平方公尺" = 19.15,
  "屋齡" = 12,
  "附屬設備-冷氣" = 1,
  "附屬設備-熱水器" = 1,
  "附屬設備-洗衣機" = 0,
  "附屬設備-電視機" = 1,
  "附屬設備-瓦斯或天然氣" = 0,
  "附屬設備-有線電視" = 0,
  "附屬設備-網路" = 0,
  "捷運站距離(公尺)" = 604.12,
  "附近建物單位成交均價" = 467.4000,
  "捷運線" = "松山新店線",
  stringsAsFactors = FALSE,
  check.names = FALSE  # 保留你自己指定的欄位名稱 TRUE 會把 dash（-）、括號、空白都換成點 .，有時還會補數字或底線
)

predict_rent_lgbm <- function(input_row, model_path = "final_lgbm_model.txt") {
  # 1. 定義所有需要的 feature 欄位（必須和模型訓練時順序/名稱完全一致）
  needed_cols <- c(
    "鄉鎮市區", "租賃年月日", "出租型態", "總樓層數", "建物型態", "租賃住宅服務",
    "租賃天數", "有無電梯", "建物現況格局-房",
    "建物現況格局-廳", "建物現況格局-衛", "建物總面積平方公尺",
    "屋齡", "附屬設備-冷氣", "附屬設備-熱水器",
    "附屬設備-洗衣機", "附屬設備-電視機", "附屬設備-瓦斯或天然氣",
    "附屬設備-有線電視", "附屬設備-網路", "捷運站距離(公尺)",
    "附近建物單位成交均價", "捷運線"
  )
   # 2. 定義所有分類欄位的合法 levels
  categorical_levels <- list(
    "鄉鎮市區" = c("士林區", "大同區", "大安區", "中山區", "中正區", "內湖區", "文山區", "北投區", "松山區", "信義區", "南港區", "萬華區"),
    "出租型態" = c("分租套房", "分租雅房", "分層出租", "未知", "整棟(戶)出租", "獨立套房"),
    "建物型態" = c("公寓(5樓含以下無電梯)", "住宅大樓(11層含以上有電梯)", "其他", "店面(店鋪)", "透天厝", "華廈(10層含以下有電梯)", "廠辦", "辦公商業大樓"),
    "租賃住宅服務" = c("一般代管", "一般包租", "一般轉租", "未知", "社會住宅代管", "社會住宅包租轉租"),
    "捷運線" = c("中和新蘆線", "文湖線", "松山新店線", "板南線", "淡水信義線", "新北投支線", "環狀線")
  )
  categorical_cols <- names(categorical_levels)

  # 3. 檢查 input_row 欄位是否齊全
  missing <- setdiff(needed_cols, names(input_row))
  if (length(missing) > 0) {
    stop(paste0("❌ 欄位缺失：", paste(missing, collapse = ", ")))
  }
  # 按照 needed_cols 排序 input_row 欄位（確保順序正確）
  input_row <- input_row[, needed_cols]

  # 4. 處理分類欄位：轉成 factor 並檢查是否合法值
  for (col in categorical_cols) {
    # 強制轉成 factor 並設定 levels
    input_row[[col]] <- factor(input_row[[col]], levels = categorical_levels[[col]])
    # 如果有 NA（代表輸入不合法），給出明確錯誤
    if (any(is.na(input_row[[col]]))) {
      stop(sprintf(
        "❌ 欄位 %s 的值有錯：%s\n可選值為：%s",
        col, 
        paste(input_row[[col]][is.na(input_row[[col]])], collapse = ", "),
        paste(categorical_levels[[col]], collapse = ", ")
      ))
    }
  }

  # 5. 載入 LightGBM 訓練好的模型
  model <- lightgbm::lgb.load(model_path)

  # 6. 轉換 input_row 成數值矩陣（feature 順序與模型一致）
  test_mat <- data.matrix(input_row)
  colnames(test_mat) <- needed_cols

  # 7. 使用模型進行預測（輸出為對數，需要轉回原本單位）
  pred_log <- predict(model, test_mat)
  pred <- exp(pred_log)  # 預設你訓練時是用 log(price)
  return(pred)
}
predict_rent_lgbm(test_row)