# 讀取檔案
rent_trades_raw <- read.csv(
  file        = "dataset/trades.csv",
  header      = TRUE,
  stringsAsFactors = FALSE,
  fileEncoding = "UTF-8"
)
cat("\n原始載入資料筆數：", nrow(rent_trades_raw), "筆\n")

# 載入必要的套件
library(dplyr)
library(caret)
library(lubridate)
library(xgboost)
library(Matrix)

# --- 全局定義 ---
# 用於 one-hot encoding 的類別欄位
global_categorical_cols <- c("鄉鎮市區", "出租型態", "建物型態", 
                              "有無電梯", "租賃住宅服務")

# XGBoost 模型參數 (如果不由CV調整，則保持全局)
global_best_params_fixed <- list(eta = 0.1, max_depth = 5, subsample = 0.9, colsample_bytree = 0.7)

# --- 函數定義 ---

# --- 函數：欄位篩選 ---
# 描述：此函數用於從資料框中移除不需要的欄位。
# 參數：
#   df: 資料框 (data.frame)
# 返回：篩選後的資料框
filter_columns <- function(df) {
  # 定義初始要排除的欄位列表
exclude_cols <- c("編號", "土地位置建物門牌", "都市土地使用分區", "source_file", 
                 "單價元平方公尺", "備註", "座標.x", "座標.y", "通車日期", "最近捷運站",
                   "主要建材", "主要用途", "租賃層次", "車位類別", "土地面積平方公尺", 
                   "轉乘站", "文湖線", "淡水信義線", "新北投支線", 
                   "松山新店線", "小碧潭支線", "中和新蘆線", "板南線", "環狀線","有無管理組織","附屬設備.冰箱",
                   "交易筆棟數.車位", "交易筆棟數.土地", "租賃層次.四類.","有無管理員","交易筆棟數.建物",  "建材分類","屋齡分類",
                   "主要用途分類", "附屬設備.有線電視", "建物現況格局.隔間",
                   "有無附傢俱")
  df <- df[, !names(df) %in% exclude_cols] # 移除定義在 exclude_cols 中的欄位

  # 定義額外要移除的欄位 (通常是前處理後不再需要的原始欄位，或是不參與模型的欄位)
cols_to_remove_additionally <- c(

  )

  # 實際執行額外欄位移除
  actual_cols_to_remove <- intersect(names(df), cols_to_remove_additionally)
if (length(actual_cols_to_remove) > 0) {
    df <- df[, !names(df) %in% actual_cols_to_remove]
  }

  return(df) # 返回處理後的資料框
}

# --- 函數：資料前處理 ---
# 描述：此函數執行主要的資料前處理步驟，包括日期處理、數值轉換、遺失值處理和 one-hot encoding。
# 參數：
#   df: 欄位篩選後的資料框
#   categorical_cols_def: 全局定義的類別欄位名稱向量
# 返回：完整前處理後的資料框
preprocess_data_custom <- function(df, categorical_cols_def) {
  # 1. 處理租賃年月日：轉換為 POSIXct 並再轉為數值型時間戳
  if("租賃年月日" %in% names(df)){
    df$parsed_date <- as.POSIXct(paste(df$租賃年月日, "12:00:00"), format="%Y-%m-%d %H:%M:%S", tz = "UTC")
    df$租賃年月日_ts <- as.numeric(df$parsed_date)
    df <- df[, !names(df) %in% c("租賃年月日", "parsed_date")] # 移除原始和中間日期欄位
  }

  # 2. 處理租賃期間天數：轉換為數值型並篩選掉 NA 及小於30天的記錄
  if("租賃天數" %in% names(df)){ # 檢查原始 "租賃天數" 欄位是否存在
    df$租賃期間天數 <- as.numeric(as.character(df$租賃天數)) # 轉換為數值
    df <- df[!is.na(df$租賃天數), ] # 移除原始欄位為NA的記錄
    if("租賃期間天數" %in% names(df)) { # 再次確認轉換後的欄位存在
        df <- df[!is.na(df$租賃期間天數), ] # 移除轉換後為NA的記錄
        df <- df[df$租賃期間天數 >= 30, ]   # 篩選租賃天數大於等於30天
    }
  } else {
    # 如果原始 "租賃天數" 欄位不存在，嘗試直接使用已存在的 "租賃期間天數" (若為數值型)
    if("租賃期間天數" %in% names(df) && is.numeric(df$租賃期間天數)){
        df <- df[!is.na(df$租賃期間天數), ] 
        df <- df[df$租賃期間天數 >= 30, ]
    } else {
        # 若兩種情況都不滿足，表示無法進行此步驟的處理。可選擇停止或發出警告。
        # stop("無法處理租賃期間：'租賃天數' 或有效的 '租賃期間天數' 欄位缺失。") 
    }
  }

  # 3. 處理建築完成年月：轉換為年份
  if ("建築完成年月" %in% names(df)) { # 檢查 "建築完成年月" 欄位是否存在
    # 僅處理非NA、非空字串、非 "null" 的值
    if(any(!is.na(df$建築完成年月) & df$建築完成年月 != "" & df$建築完成年月 != "null")){
        # 將 "YYYY-MM" 格式的字串補上 "-01" 使其變為完整日期，再轉換為 POSIXct，最後提取年份
        parsed_dates_building <- as.POSIXct(paste0(df$建築完成年月, "-01"), format="%Y-%m-%d", tz="UTC") # tz="UTC" 避免時區問題
        df$建築完成年月_yr <- as.numeric(format(parsed_dates_building, "%Y"))
    } else {
        df$建築完成年月_yr <- NA # 若原始欄位無有效值，則設為NA
    }
    # 移除原始 "建築完成年月" 欄位 (因為已經轉換為 _yr)
    df <- df[, !names(df) %in% c("建築完成年月")]
  }

  # 4. 為特定類別欄位處理 "空" 值：將 NA、空字串或 "null" 轉換為 "空" 類別
  cols_to_fill_empty <- intersect(c("出租型態", "租賃住宅服務"), names(df)) # "租賃住宅服務" 可能已被 filter_columns 移除
  for(col_name in cols_to_fill_empty){
    if(col_name %in% names(df)){ # 再次確認欄位存在
      df[[col_name]][is.na(df[[col_name]]) | df[[col_name]] == ""| df[[col_name]] == "NA" | df[[col_name]] == "null"] <- "空"
    }
  }
  
  # 5. 移除在特徵工程中使用完畢的原始欄位
  # 這些欄位已經被轉換成新的特徵 (如 _ts, _yr, 或用於篩選)
  cols_to_remove_after_feat_eng <- c("租賃天數") 
  # "建築完成年月" 已在步驟3中移除
  # "租賃年月日" 已在步驟1中移除
  actual_cols_to_remove_fe <- intersect(names(df), cols_to_remove_after_feat_eng)
  if (length(actual_cols_to_remove_fe) > 0) {
    df <- df[, !names(df) %in% actual_cols_to_remove_fe]
  }

  # 6. One-hot encoding：對定義的類別欄位進行獨熱編碼
  # 首先，依照使用者要求，使用預定義的級別進行明確的因子轉換

  # --- 使用者操作必要：請準確定義所有預期的因子級別 --- 
  # 以下為範例。您必須為您的特定資料集驗證並完成此列表。
  # 不正確或不完整的級別將導致腳本停止。
  factor_levels_definition <- list(
    "鄉鎮市區" = c("大安區", "信義區", "中山區", "中正區", "松山區", "內湖區", "士林區", "文山區", "北投區", "萬華區", "大同區", "南港區"), # 範例：台北市常見行政區。請驗證並補全。
    "出租型態" = c("整層住家", "分租套房", "獨立套房", "雅房", "其他", "空", "整棟(戶)出租", "分層出租", "分租雅房"), # 範例：常見類型。"空" 代表已處理的NA值。請驗證並補全。
    "建物型態" = c("住宅大樓(11層含以上有電梯)", "公寓(5樓含以下無電梯)", "套房(1樓至5樓)", "華廈(10層含以下有電梯)", "透天厝", "其他", "店面(店鋪)", "廠辦", "辦公商業大樓", "倉庫", "工廠", "農舍", "空"), # 範例/佔位符。請驗證並補全。
    "有無電梯" = c(1, 0, "空"), # 範例/佔位符。如果NA被映射到"空"，請包含它。請驗證並補全。
    "租賃住宅服務" = c("空", "社會住宅包租轉租", "社會住宅代管", "一般包租", "一般轉租", "一般代管")  # 範例。步驟4將NA映射到"空"。請驗證並補全。
  )
  # --- 使用者操作必要結束 ---

  # 識別資料框中存在且在 global_categorical_cols 中的類別欄位
  actual_categorical_cols_to_process <- intersect(categorical_cols_def, names(df))

  if (length(actual_categorical_cols_to_process) > 0) {
    for (col_name in actual_categorical_cols_to_process) {
      if (col_name %in% names(factor_levels_definition)) {
        # 此欄位已定義級別
        defined_levels <- factor_levels_definition[[col_name]]
        
        # 在因子轉換前，檢查資料中是否存在未在定義級別中的值
        # 這能提供關於哪些值有問題的更具體錯誤訊息。
        unique_values_in_col <- unique(df[[col_name]]) # 若原始資料有NA，此處會包含NA
        # 從 unique_values_in_col 中過濾掉 NA 以進行 setdiff 比較，因為 NA 不是值的「不匹配」，而是 NA 的存在。
        problematic_values <- setdiff(unique_values_in_col[!is.na(unique_values_in_col)], defined_levels)
        
        if (length(problematic_values) > 0) {
          stop(paste0("❌ 欄位 '", col_name, "' 中包含未定義於 factor_levels_definition 的值: [", 
                      paste(head(problematic_values, 5), collapse=", "), 
                      if(length(problematic_values)>5) "...", 
                      "]. 請檢查資料或更新 factor_levels_definition. 定義的級別為: [", 
                      paste(defined_levels, collapse=", "),"]" ))
        }

        # 使用預定義的級別將欄位轉換為因子
        df[[col_name]] <- factor(df[[col_name]], levels = defined_levels)
        
        # 依照使用者要求，如果在轉換後仍存在NA值，則停止腳本。
        # 這意味著原始的NA值必須被處理（例如，轉換為"空"並且"空"被包含在預定義級別中），
        # 或者如果NA不是一個可接受的級別，則此欄位的資料必須是完整的。
        if (any(is.na(df[[col_name]]))) {
          stop(paste0("❌ 欄位 '", col_name, "' 在使用預定義級別轉換為因子後仍包含NA值. ",
                      "這可能意味著原始資料中存在NA，且NA未被映射到一個明確的因子級別(例如，'空' 被包含在預定義級別中)，",
                      "或者在進行因子轉換之前，未能攔截到未在預定義級別中聲明的值 (此情況應已被前一個 stop 捕獲).",
                      "請檢查 '", col_name, "' 的原始資料以及 factor_levels_definition 中的級別定義. ",
                      "定義的級別為: [", paste(defined_levels, collapse=", "),"]" ))
        }
      } else {
        # 此類別欄位未在 factor_levels_definition 中定義級別
        warning(paste0("注意：欄位 '", col_name, "' 的因子水平未在 'factor_levels_definition' 中明確定義. ",
                       "將嘗試使用資料中存在的唯一值將其轉換為因子. ",
                       "建議為所有類別欄位定義明確的水平以確保可預測的行為及資料品質."))
        if (!is.factor(df[[col_name]])) {
          df[[col_name]] <- as.factor(df[[col_name]])
        }
      }
    }
  }
  
  # 使用 caret::dummyVars 進行 One-hot encoding
  # 'actual_categorical_cols_to_process' 中的欄位現在應該是具有適當級別的因子。
  valid_categorical_cols <- intersect(actual_categorical_cols_to_process, names(df)) # 應與 actual_categorical_cols_to_process 相同
  df_processed <- df 

  if (length(valid_categorical_cols) > 0) {
      numeric_and_other_cols_to_keep <- setdiff(names(df), valid_categorical_cols)
      df_numeric_part <- df[, numeric_and_other_cols_to_keep, drop = FALSE]
      
      # 此檢查現在有些多餘，因為我們處理了 'actual_categorical_cols_to_process'
      # 但為安全起見保留，確保只使用存在的欄位。
      existing_valid_cat_cols <- valid_categorical_cols[valid_categorical_cols %in% names(df)] 
      
      if(length(existing_valid_cat_cols) > 0) {
        # 先前的迴圈 `for(col in existing_valid_cat_cols) { if(!is.factor(df[[col]])) df[[col]] <- as.factor(df[[col]]) }`
        # 現在已由上面的新因子轉換邏輯有效處理。
        
        dummy_model <- caret::dummyVars(" ~ .", data = df[, existing_valid_cat_cols, drop = FALSE], fullRank = FALSE)
        dummy_data <- stats::predict(dummy_model, newdata = df[, existing_valid_cat_cols, drop = FALSE])
        
        # 合併數值部分和 one-hot 編碼後的類別部分
        if (is.vector(dummy_data)) { # 如果只有一個類別欄位且是二元的，結果可能是向量
            dummy_data_matrix <- as.matrix(dummy_data)
            df_processed <- cbind(df_numeric_part, dummy_data_matrix)
        } else if (is.matrix(dummy_data) || is.data.frame(dummy_data)){ # 一般情況下是矩陣或資料框
            df_processed <- cbind(df_numeric_part, dummy_data)
        } else {
            df_processed <- df_numeric_part # 發生意外情況，則只保留數值部分
        }
      } else {
        df_processed <- df_numeric_part # 如果沒有有效的類別欄位可供編碼
      }
  } else {
    # 如果沒有定義或找到任何類別欄位，df_processed 實質上就是 df
  }

  # 7. 最終的 NA 和特定字串 ("null", "", "NAN") 處理：對所有欄位進行檢查
  for (j in seq_along(df_processed)) {
    if(is.character(df_processed[[j]])) {
        df_processed[[j]][df_processed[[j]] == "null" | df_processed[[j]] == "" | toupper(df_processed[[j]]) == "NAN"] <- NA
    }
  }
  df_processed <- na.omit(df_processed) # 移除任何仍含有NA的列
  
  return(df_processed) # 返回完整處理後的資料框
}

# --- 函數：計算 MAPE (平均絕對百分比誤差) ---
# 描述：計算實際值與預測值之間的 MAPE。
# 參數：
#   actual: 實際值的數值向量
#   predicted: 預測值的數值向量
# 返回：MAPE 百分比值，或在無法計算時返回 NA
calculate_mape <- function(actual, predicted) {
  if (length(actual) != length(predicted)) {
    stop("計算 MAPE 時，實際值和預測值的向量長度必須相同。")
  }
  valid_indices <- abs(actual) > 1e-9 # 避免除以零或極小值，定義有效索引
  if (sum(valid_indices) == 0) return(NA) # 如果沒有有效的實際值，返回 NA
  
  actual_valid <- actual[valid_indices]
  predicted_valid <- predicted[valid_indices]
  
  mape <- mean(abs((actual_valid - predicted_valid) / actual_valid)) * 100
  return(mape)
}

# --- 函數：計算 MdAPE (中位數絕對百分比誤差) ---
# 描述：計算實際值與預測值之間的 MdAPE。
# 參數：
#   actual: 實際值的數值向量
#   predicted: 預測值的數值向量
# 返回：MdAPE 百分比值，或在無法計算時返回 NA
calculate_mdape <- function(actual, predicted) {
  if (length(actual) != length(predicted)) {
    stop("計算 MdAPE 時，實際值和預測值的向量長度必須相同。")
  }
  valid_indices <- abs(actual) > 1e-9 # 避免除以零或極小值
  if (sum(valid_indices) == 0) return(NA)
  
  actual_valid <- actual[valid_indices]
  predicted_valid <- predicted[valid_indices]
  
  mdape <- median(abs((actual_valid - predicted_valid) / actual_valid)) * 100
  return(mdape)
}

# --- 輔助函數：強制轉換為數值型並處理 NA ---
# 描述：將資料框中的所有欄位嘗試轉換為數值型，並將 NA 替換為 0。主要用於 XGBoost 輸入前的矩陣準備。
# 參數：
#   df: 資料框
# 返回：處理後的資料框 (所有欄位盡可能轉為數值型，NA 轉為 0)
force_numeric_and_handle_na_generic <- function(df) { 
  for (col_name in names(df)) {
    if (!is.numeric(df[[col_name]])) { # 如果欄位非數值型
      # 先轉為字元型再轉數值型，以抑制因子轉換時可能出現的警告
      df[[col_name]] <- suppressWarnings(as.numeric(as.character(df[[col_name]]))) 
    }
  }
  if (sum(is.na(df)) > 0) df[is.na(df)] <- 0 # 將所有 NA 值替換為 0
  return(df)
}

# --- 函數：執行交叉驗證 (Cross-Validation) ---
# 描述：對已處理的資料集執行 k 折交叉驗證，評估 XGBoost 模型性能。
# 參數：
#   processed_data: 完整前處理後的資料框
#   cat_cols: 全局定義的原始類別欄位名稱向量 (目前在此函數內未直接用於特徵顯示)
#   model_cv_params: XGBoost CV 用的參數列表
# 返回：無 (直接印出 CV 結果)
run_cross_validation <- function(processed_data, cat_cols, model_cv_params) {
  cat("\n--- 開始執行交叉驗證 (CV) ---\n")
  
  if (nrow(processed_data) > 1 && "總額元" %in% names(processed_data) && ncol(processed_data) > 1){
      X_cv_df <- processed_data[, setdiff(names(processed_data), c("總額元", "總額元_strata"))]
      y_cv_vec <- processed_data$總額元
      
      X_cv_df_numeric <- force_numeric_and_handle_na_generic(X_cv_df)
      X_cv_mat <- as.matrix(X_cv_df_numeric)
      
      if (ncol(X_cv_mat) > 0 && nrow(X_cv_mat) > 0 && length(y_cv_vec) == nrow(X_cv_mat)) { 
          
          # --- 印出 CV 訓練特徵 (概念性) ---
          cv_all_colnames <- colnames(X_cv_mat)
          conceptual_features_for_cv <- c()
          # 先加入原始類別欄位名稱 (如果其衍生的 one-hot 欄位存在)
          for (cat_col_original in cat_cols) { # cat_cols is global_categorical_cols passed in
            if (any(startsWith(cv_all_colnames, cat_col_original))) {
              conceptual_features_for_cv <- c(conceptual_features_for_cv, cat_col_original)
            }
          }
          # 再加入非 one-hot 編碼的欄位
          non_one_hot_cols_in_cv <- c()
          for (col_name in cv_all_colnames) {
            is_one_hot_part <- FALSE
            for (cat_col_original in cat_cols) {
              if (startsWith(col_name, cat_col_original)) {
                is_one_hot_part <- TRUE
                break
              }
            }
            if (!is_one_hot_part) {
              non_one_hot_cols_in_cv <- c(non_one_hot_cols_in_cv, col_name)
            }
          }
          conceptual_features_for_cv <- unique(c(conceptual_features_for_cv, non_one_hot_cols_in_cv))
          cat("\n[CV] 用於交叉驗證訓練的概念性特徵欄位 (共 ", length(conceptual_features_for_cv), " 個):")
          print(conceptual_features_for_cv)
          # ------

          set.seed(42) 
          folds_for_cv <- caret::createFolds(y_cv_vec, k = 10, list = TRUE, returnTrain = FALSE)
          
          # 初始化用於存儲每折性能指標的向量
          cv_rmse_all_data <- c(); cv_r2_all_data <- c(); cv_mape_all_data <- c(); cv_mdape_all_data <- c()
          
          # 遍歷每一折進行訓練和驗證
          cat("開始進行 ", length(folds_for_cv), " 折交叉驗證...\n")
          for (i in seq_along(folds_for_cv)) {
            cat("  處理第 ", i, " 折...\n")
            valid_idx_cv_fold <- folds_for_cv[[i]] # 當前折的驗證集索引
            train_idx_cv_fold <- setdiff(seq_len(nrow(X_cv_mat)), valid_idx_cv_fold) # 當前折的訓練集索引
            
            # 確保訓練集和驗證集都有足夠的資料
            if(length(train_idx_cv_fold) < 2 || length(valid_idx_cv_fold) == 0) {
                cat("    第 ", i, " 折資料不足，跳過。\n")
                next
            }

            # 準備當前折的 DMatrix (XGBoost 的內部資料結構)
            dtrain_cv_fold <- xgboost::xgb.DMatrix(data = X_cv_mat[train_idx_cv_fold, , drop = FALSE], label = y_cv_vec[train_idx_cv_fold])
            
            # 在當前折上訓練 XGBoost 模型
            # 注意：xgb.train 中的 seed 參數在 R 版本中常被忽略，set.seed() 更為重要
            model_in_cv <- xgboost::xgb.train(params = model_cv_params, data = dtrain_cv_fold, nrounds = 500, verbose = 0) 
            
            # 在當前折的驗證集上進行預測 (預測結果是 log 轉換後的)
            pred_in_cv_log <- stats::predict(model_in_cv, X_cv_mat[valid_idx_cv_fold, , drop = FALSE])
            
            # 將實際值和預測值轉換回原始尺度 (指數轉換)
            actual_cv_orig_scale <- exp(y_cv_vec[valid_idx_cv_fold])
            pred_cv_orig_scale <- exp(pred_in_cv_log)
          
            # 計算當前折的 RMSE (均方根誤差)
            rmse_fold <- sqrt(mean((actual_cv_orig_scale - pred_cv_orig_scale)^2))
            
            # 計算當前折的 R2 (決定係數)
            r2_fold <- NA # 初始化為 NA
            if (length(unique(actual_cv_orig_scale)) >= 2 && var(actual_cv_orig_scale, na.rm=TRUE) != 0) { # 避免常數實際值或零方差
              r2_fold <- 1 - sum((actual_cv_orig_scale - pred_cv_orig_scale)^2) / sum((actual_cv_orig_scale - mean(actual_cv_orig_scale))^2)
            }
            
            # 計算當前折的 MAPE
            mape_fold <- calculate_mape(actual_cv_orig_scale, pred_cv_orig_scale) 
            
            # 計算當前折的 MdAPE
            mdape_fold <- calculate_mdape(actual_cv_orig_scale, pred_cv_orig_scale)
            
            # 收集當前折的性能指標
            cv_rmse_all_data <- c(cv_rmse_all_data, rmse_fold)
            cv_r2_all_data <- c(cv_r2_all_data, r2_fold)
            cv_mape_all_data <- c(cv_mape_all_data, mape_fold) 
            cv_mdape_all_data <- c(cv_mdape_all_data, mdape_fold)
          }
          
          # 計算並印出所有折的平均性能指標
          cat(sprintf("\nCV 平均性能 (原始尺度): RMSE: %.2f, R2: %.4f, MAPE: %.2f%%, MdAPE: %.2f%%\n", 
                      mean(cv_rmse_all_data, na.rm=TRUE), 
                      mean(cv_r2_all_data, na.rm=TRUE),
                      mean(cv_mape_all_data, na.rm=TRUE),
                      mean(cv_mdape_all_data, na.rm=TRUE)))
    } else {
        cat("交叉驗證所需資料不足或特徵矩陣維度不正確，跳過 CV。\n")
    }
    } else {
    cat("用於交叉驗證的資料集為空或 '總額元' 欄位缺失，跳過 CV。\n")
  }
  cat("--- 交叉驗證 (CV) 執行完畢 ---\n")
}

# --- 函數：訓練並評估最終模型 ---
# 描述：使用完整的訓練集訓練 XGBoost 模型，並在測試集上評估其性能，最後儲存預測結果。
# 參數：
#   train_df: 訓練集資料框 (已前處理和 log 轉換目標變數)
#   test_df: 測試集資料框 (已前處理和 log 轉換目標變數)
#   cat_cols: 全局定義的原始類別欄位名稱向量 (目前在此函數內未直接用於特徵顯示)
#   model_final_params: XGBoost 最終模型訓練用的參數列表
# 返回：無 (直接印出最終模型性能並儲存 CSV)
train_evaluate_final_model <- function(train_df, test_df, cat_cols, model_final_params) {
  cat("\n--- 開始訓練並評估最終模型 ---\n")
  
  if (nrow(train_df) > 1 && nrow(test_df) > 0 && 
      "總額元" %in% names(train_df) && "總額元" %in% names(test_df) &&
      ncol(train_df) > 1 && ncol(test_df) > 1) {
      
      X_train_fm_df <- train_df[, setdiff(names(train_df), c("總額元", "總額元_strata"))]
      y_train_fm_vec <- train_df$總額元
      X_test_fm_df <- test_df[, setdiff(names(test_df), c("總額元", "總額元_strata"))]
      y_test_fm_vec <- test_df$總額元
      
      X_train_fm_df_numeric <- force_numeric_and_handle_na_generic(X_train_fm_df)
      X_test_fm_df_numeric  <- force_numeric_and_handle_na_generic(X_test_fm_df)
      
      if (ncol(X_train_fm_df_numeric) > 0 && nrow(X_train_fm_df_numeric) > 0 && 
          ncol(X_test_fm_df_numeric) > 0 && nrow(X_test_fm_df_numeric) > 0 &&
          length(y_train_fm_vec) == nrow(X_train_fm_df_numeric) && length(y_test_fm_vec) == nrow(X_test_fm_df_numeric)) {

          X_train_fm_mat <- as.matrix(X_train_fm_df_numeric)
          X_test_fm_mat  <- as.matrix(X_test_fm_df_numeric)
          
          if (nrow(X_train_fm_mat) < 2) {
            stop("最終模型訓練：訓練集資料筆數過少 (<2) 無法訓練 (轉換為矩陣後)。")
          }
          
          # --- 印出最終模型訓練特徵 (概念性) ---
          final_model_all_colnames <- colnames(X_train_fm_mat)
          conceptual_features_for_final_model <- c()
          # 先加入原始類別欄位名稱
          for (cat_col_original in cat_cols) { # cat_cols is global_categorical_cols passed in
            if (any(startsWith(final_model_all_colnames, cat_col_original))) {
              conceptual_features_for_final_model <- c(conceptual_features_for_final_model, cat_col_original)
            }
          }
          # 再加入非 one-hot 編碼的欄位
          non_one_hot_cols_in_final_model <- c()
          for (col_name in final_model_all_colnames) {
            is_one_hot_part <- FALSE
            for (cat_col_original in cat_cols) {
              if (startsWith(col_name, cat_col_original)) {
                is_one_hot_part <- TRUE
                break
              }
            }
            if (!is_one_hot_part) {
              non_one_hot_cols_in_final_model <- c(non_one_hot_cols_in_final_model, col_name)
            }
          }
          conceptual_features_for_final_model <- unique(c(conceptual_features_for_final_model, non_one_hot_cols_in_final_model))
          cat("\n[最終模型] 用於訓練的概念性特徵欄位 (共 ", length(conceptual_features_for_final_model), " 個):")
          print(conceptual_features_for_final_model)
          # ------

          set.seed(42)
          
          # 準備訓練用的 DMatrix
          dtrain_for_final_model <- xgboost::xgb.DMatrix(data = X_train_fm_mat, label = y_train_fm_vec)
          
          # 訓練最終的 XGBoost 模型
          # 注意：xgb.train 中的 seed 參數在 R 版本中常被忽略
          final_model_trained <- xgboost::xgb.train(params = model_final_params, data = dtrain_for_final_model, 
                                           nrounds = 500, verbose = 0)
          
          # --- 新增：儲存訓練好的模型 ---
          model_save_path <- "model/final_xgb_model.txt"
          tryCatch({
            xgboost::xgb.save(model = final_model_trained, fname = model_save_path)
            cat(sprintf("\n最終模型已成功儲存至: %s\n", model_save_path))
          }, error = function(e) {
            cat(sprintf("\n儲存最終模型檔案時發生錯誤: %s\n", e$message))
          })
          # --- 儲存模型結束 ---
          
          # 在測試集上進行預測 (如果測試集矩陣非空)
          if (nrow(X_test_fm_mat) > 0) { 
              # 預測結果是 log 轉換後的價格
              final_pred_on_test_set_log <- stats::predict(final_model_trained, X_test_fm_mat)
          
              # 將測試集的實際目標值和預測值都轉換回原始價格尺度
              actual_test_orig_scale <- exp(y_test_fm_vec)
              final_pred_on_test_set_orig_scale <- exp(final_pred_on_test_set_log)
          
              # 計算 RMSE
              baseline_test_rmse <- sqrt(mean((actual_test_orig_scale - final_pred_on_test_set_orig_scale)^2))
              
              # 計算 R2
              baseline_test_r2 <- NA
              if (length(unique(actual_test_orig_scale)) >= 2 && var(actual_test_orig_scale, na.rm=TRUE) != 0) {
                  baseline_test_r2 <- 1 - sum((actual_test_orig_scale - final_pred_on_test_set_orig_scale)^2) / sum((actual_test_orig_scale - mean(actual_test_orig_scale))^2)
              }
              
              # 計算 MAPE
              baseline_test_mape <- calculate_mape(actual_test_orig_scale, final_pred_on_test_set_orig_scale)
              
              # 計算 MdAPE
              baseline_test_mdape <- calculate_mdape(actual_test_orig_scale, final_pred_on_test_set_orig_scale)
              
              # 印出最終模型在測試集上的性能指標
              cat(sprintf("\n最終模型在測試集上的性能 (original scale):\n  RMSE: %.2f\n  R2  : %.4f\n  MAPE: %.2f%%\n  MdAPE: %.2f%%\n", 
                          baseline_test_rmse, baseline_test_r2, baseline_test_mape, baseline_test_mdape))
              
              # --- 準備輸出 CSV 檔案 ---
              # 使用原始測試集資料框 test_df (其中總額元已 log 轉換) 作為基礎
              output_subset_final <- data.frame(test_df) 
              
              # 新增實際價格 (原始尺度) 欄位
              output_subset_final$總額元_實際_原始尺度 <- exp(output_subset_final$總額元) # exp(log(原始價格)) = 原始價格
              
              # 新增模型預測價格 (原始尺度) 欄位
              output_subset_final$`模型預測的價格_原始尺度` <- final_pred_on_test_set_orig_scale
              
              # 新增預測差值 (原始尺度) 欄位
              output_subset_final$預測差值_原始尺度 <- output_subset_final$總額元_實際_原始尺度 - output_subset_final$`模型預測的價格_原始尺度`

              # 定義輸出 CSV 檔案的路徑和名稱
              # output_csv_path_test_only <- "model/xgb_predictions_with_details_TEST_SET_ONLY.csv" # 已註解，不再輸出此檔案
              
              # 嘗試寫入 CSV 檔案
              # tryCatch({ # 已註解，不再輸出此檔案
                # 選擇要寫入 CSV 的欄位 (可根據需求調整)
                # cols_to_write <- c("總額元_實際_原始尺度", "模型預測的價格_原始尺度", "預測差值_原始尺度")
                # 也可以加入一些原始特徵以供分析，例如：
                # potential_orig_features <- intersect(c("鄉鎮市區", "建物面積平方公尺", "租賃期間天數", "租賃年月日_ts"), names(test_df))
                # existing_cols_to_write <- intersect(c(cols_to_write, potential_orig_features), names(output_subset_final))
                existing_cols_to_write <- intersect(cols_to_write, names(output_subset_final)) # 簡化版：只輸出核心預測資訊
                
                # write.csv(output_subset_final[, existing_cols_to_write, drop=FALSE], 
                #           file = output_csv_path_test_only, row.names = FALSE, fileEncoding = "UTF-8") # 已註解
                # cat(sprintf("\n測試集預測詳細資料已儲存至: %s\n", output_csv_path_test_only)) # 已註解
              # }, error = function(e) { # 已註解
                # cat(sprintf("\n儲存 CSV 檔案時發生錯誤: %s\n", e$message)) # 已註解
              # }) # 已註解
          } else {
            cat ("\n測試集資料轉換後為空，跳過最終模型的預測與評估步驟。\n")
          }
      } else {
        cat("\n訓練集或測試集資料轉換後不足或維度不正確，跳過最終模型的訓練與評估步驟。\n")
      }
  } else {
    cat("\n訓練集或測試集為空，或 '總額元' 欄位缺失，無法進行最終模型的訓練與評估。\n")
  }
  cat("--- 最終模型訓練與評估完畢 ---\n")
}

# --- 主流程開始 ---

# 1. 讀取原始資料
cat("--- 步驟 1: 讀取原始資料 ---\n")
rent_trades_raw <- read.csv(
  file        = "dataset/trades.csv",
  header      = TRUE,
  stringsAsFactors = FALSE,
  fileEncoding = "UTF-8"
)
cat("原始載入資料筆數：", nrow(rent_trades_raw), "筆\n")

# 2. 欄位篩選
cat("\n--- 步驟 2: 執行欄位篩選 ---\n")
rent_trades_filtered <- filter_columns(rent_trades_raw)
cat("欄位篩選後剩餘欄位數：", ncol(rent_trades_filtered), "\n")
# print(head(rent_trades_filtered, 2)) # 可選：印出篩選後頭幾筆資料檢查

# 3. 資料前處理
cat("\n--- 步驟 3: 執行資料前處理 ---\n")
rent_trades_processed <- preprocess_data_custom(rent_trades_filtered, global_categorical_cols)
cat("資料前處理完成後，準備用於模型的資料筆數：", nrow(rent_trades_processed), "筆，欄位數：", ncol(rent_trades_processed), "\n")
# print(head(rent_trades_processed, 2)) # 可選：印出前處理後頭幾筆資料檢查

# --- 新增步驟：移除 "建築完成年月_yr" (因為已被指定不使用) ---
if ("建築完成年月_yr" %in% names(rent_trades_processed)) {
  rent_trades_processed <- rent_trades_processed[, !names(rent_trades_processed) %in% "建築完成年月_yr"]
  cat("已移除 '建築完成年月_yr' 欄位。\n")
}
# ------

# 4. 篩選目標變數 > 0 的記錄
cat("\n--- 步驟 4: 篩選目標變數 (總額元) 大於 0 的記錄 ---\n")
if ("總額元" %in% names(rent_trades_processed)) {
    original_rows_before_filter_positive <- nrow(rent_trades_processed)
    rent_trades_processed <- rent_trades_processed[rent_trades_processed$總額元 > 0, ]
    cat("移除總額元 <= 0 的記錄後，剩餘資料筆數：", nrow(rent_trades_processed), "筆 (移除了 ", original_rows_before_filter_positive - nrow(rent_trades_processed), " 筆)\n")
} else {
    stop("錯誤：欄位 '總額元' 在預處理後缺失，無法進行 > 0 的篩選。")
}

# 5. 對目標變數 "總額元" 進行 log 轉換
cat("\n--- 步驟 5: 對目標變數 (總額元) 進行 Log 轉換 ---\n")
if ("總額元" %in% names(rent_trades_processed) && is.numeric(rent_trades_processed$總額元)) {
    rent_trades_processed$總額元 <- log(rent_trades_processed$總額元)
    cat("目標變數 '總額元' 已成功進行 Log 轉換。\n")
    # print(head(rent_trades_processed$總額元, 5)) # 可選：檢查 log 轉換結果
} else {
    stop("錯誤：欄位 '總額元' 缺失或非數值型，無法進行 Log 轉換。")
}

cat("\n最終可用於模型訓練的資料筆數 ：", nrow(rent_trades_processed), "筆\n")

# 6. 訓練/測試集分割 (使用分層抽樣)
cat("\n--- 步驟 6: 分割訓練集與測試集 (依總額元分層抽樣) ---\n")
set.seed(123) # 設定隨機種子以確保分割的可重現性
train_ratio <- 0.8 # 設定訓練集比例

if (nrow(rent_trades_processed) < 2 || !("總額元" %in% names(rent_trades_processed))) {
    stop("資料筆數過少或目標變數 \'總額元\' 缺失，無法進行資料分割。")
}

# 對 log轉換後的 '總額元' 進行分箱以進行分層抽樣
# 使用五分位數 (5 個箱) 作為示例
num_bins <- 5 
# 檢查是否有足夠的唯一值來創建分箱，避免 quantile 出錯
if (length(unique(rent_trades_processed$總額元)) < num_bins) {
  warning(sprintf("總額元的唯一值數量 (%d) 少於指定的分箱數 (%d)，分層抽樣可能不準確或退化為隨機抽樣。", 
                  length(unique(rent_trades_processed$總額元)), num_bins))
  # 在這種情況下，退回使用原始總額元進行抽樣 (或採取其他策略)
  strata_column <- rent_trades_processed$總額元 
} else {
  quantiles_총액원 <- quantile(rent_trades_processed$總額元, probs = seq(0, 1, by = 1/num_bins), na.rm = TRUE)
  # 確保分位點是唯一的，以避免 cut 函數出錯
  quantiles_총액원_unique <- unique(quantiles_총액원)
  if (length(quantiles_총액원_unique) < 2) { # 如果所有值都相同或太少唯一值
      warning("總額元的值過於集中，無法有效分箱進行分層抽樣。將退化為隨機抽樣。")
      strata_column <- rent_trades_processed$總額元 # 或創建一個單一水平的因子
  } else {
    rent_trades_processed$總額元_strata <- cut(rent_trades_processed$總額元, 
                                             breaks = quantiles_총액원_unique, 
                                             labels = FALSE, 
                                             include.lowest = TRUE)
    # 檢查分箱結果，確保沒有 NA (除非原始總額元就有 NA，但前面已處理)
    if(any(is.na(rent_trades_processed$總額元_strata))){
        warning("分箱後 '總額元_strata' 產生 NA 值，請檢查分箱邏輯。分層抽樣可能受影響。")
        # 可以考慮將 NA 的 strata 設為一個特殊值，或退回普通抽樣
        # rent_trades_processed$總額元_strata[is.na(rent_trades_processed$總額元_strata)] <- -1 # 範例處理
        strata_column <- rent_trades_processed$總額元 # 退回
    } else {
        strata_column <- rent_trades_processed$總額元_strata
        cat(sprintf("已將 '總額元' (log轉換後) 分為 %d 個層級進行抽樣。\n", length(levels(as.factor(strata_column)))))
    }
  }
}

train_indices <- caret::createDataPartition(strata_column, # 使用分層抽樣的欄位
                                          p = train_ratio, 
                                          list = FALSE, 
                                          times = 1)
rent_trades_train <- rent_trades_processed[train_indices, ]
rent_trades_test  <- rent_trades_processed[-train_indices, ]

# 可以選擇性移除分層欄位，但它不會被選入模型特徵，所以影響不大
# rent_trades_train$總額元_strata <- NULL
# rent_trades_test$總額元_strata <- NULL

cat(sprintf("訓練集筆數: %d (%.1f%%), 測試集筆數: %d (%.1f%%)\\n", 
            nrow(rent_trades_train), train_ratio * 100,
            nrow(rent_trades_test), (1-train_ratio) * 100))

# 7. 執行交叉驗證
# CV 參數與最終模型參數可以相同，也可以不同
# 這裡我們使用全局定義的 global_best_params_fixed
cv_params_to_use <- global_best_params_fixed 
# 確保目標是回歸平方誤差，因為我們預測連續值
cv_params_to_use$objective <- "reg:squarederror" 
# 可以加入 nthread 參數，例如 nthread = parallel::detectCores() -1 或固定值
# cv_params_to_use$nthread <- 2 

run_cross_validation(rent_trades_processed, global_categorical_cols, cv_params_to_use)

# 8. 訓練並評估最終模型
# 最終模型參數也使用全局定義的
final_model_params_to_use <- global_best_params_fixed
final_model_params_to_use$objective <- "reg:squarederror"
# final_model_params_to_use$nthread <- 2

train_evaluate_final_model(rent_trades_train, rent_trades_test, global_categorical_cols, final_model_params_to_use)

cat("\n--- R 腳本執行完畢 ---\n") 