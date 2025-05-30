library(dplyr)
library(lightgbm)
library(caret)
library(ggplot2)
library(readr)

# --- 變數索引 ---
# df        ：完整資料（data.frame）
# train_df  ：訓練資料（含目標值，data.frame）
# test_df   ：測試資料（含目標值，data.frame）
# test_mat  ：測試資料的純數值特徵矩陣（不含目標值）
# -- k-fold CV 變數索引 ---
# train_data: 每一fold 的訓練資料（data.frame）
# valid_data: 每一fold 的驗證資料（data.frame）
# mat       ：每一fold 訓練資料的純數值特徵矩陣（不含目標值）
# valid_mat ：每一fold 驗證資料的純數值特徵矩陣（不含目標值）
# --- 模型參數 ---
# categorical_cols：類別欄位的名稱（character vector）
# to_remove：要移除的特徵名稱（character vector）
# --- 提醒 ---
# data.matrix()：會把資料表轉成純數值的矩陣（factor 會自動編碼成數字）
# 最終使用特徵為 mat, valid_mat, test_mat 的欄位名稱
# 可由 print(importance)來 查看特徵重要性跟有用到的特徵


# 1.指定要讀取的欄位
use_columns <- c(
  "鄉鎮市區", "總額元", "租賃年月日",
  "出租型態",  
  "租賃層次(四類)", "總樓層數", "建物型態", 
  "交易筆棟數-土地", "交易筆棟數-建物",
  "租賃住宅服務", "租賃天數",
  "有無管理組織", "有無管理員", "有無附傢俱","有無電梯", 
  "建物現況格局-房", "建物現況格局-廳","建物現況格局-衛", "建物現況格局-隔間", 
  "建物總面積平方公尺", 
  "屋齡",
  "建材分類",
  "附屬設備-冷氣", "附屬設備-熱水器", "附屬設備-洗衣機","附屬設備-電視機", "附屬設備-冰箱", "附屬設備-瓦斯或天然氣", "附屬設備-有線電視", "附屬設備-網路",
  "捷運站距離(公尺)",
  "文湖線", "淡水信義線", "新北投支線", "松山新店線", "小碧潭支線", 
  "中和新蘆線", "板南線", "環狀線", "附近建物單位成交均價"
) 

# 2. 經過測試，特徵全留下
to_remove <- c()
# 用 setdiff() 移除指定欄位
use_columns_clean <- setdiff(use_columns, to_remove)


# 3. 讀取資料並篩選欄位
full_data <- read_csv("dataset/rent_mrg.csv", show_col_types = FALSE)
df <- full_data %>% select(all_of(use_columns_clean))

# 4. 篩掉「天數太短」的資料（如 < 30 天），以及填補缺失值
df <- df %>% filter(租賃天數 >= 30)
df$出租型態[is.na(df$出租型態)] <- "未知"
df$租賃住宅服務[is.na(df$租賃住宅服務)] <- "未知"
df$租賃年月日 <- as.numeric(df$租賃年月日)

# 5. 將 one-hot 捷運欄位合併成一欄（方便分析）
df <- df %>%
  mutate(捷運線 = case_when(
    文湖線 == 1 ~ "文湖線",
    淡水信義線 == 1 ~ "淡水信義線",
    新北投支線 == 1 ~ "新北投支線",
    松山新店線 == 1 ~ "松山新店線",
    小碧潭支線 == 1 ~ "小碧潭支線",
    中和新蘆線 == 1 ~ "中和新蘆線",
    板南線 == 1 ~ "板南線",
    環狀線 == 1 ~ "環狀線",
    TRUE ~ "無捷運"
  )) %>%
  select(-c(文湖線, 淡水信義線, 新北投支線, 松山新店線, 小碧潭支線, 
            中和新蘆線, 板南線, 環狀線)) # 移除原 one-hot 欄位


# 6. 指定分類欄位 並篩掉沒用的features
categorical_cols <- c("鄉鎮市區", "出租型態", "租賃層次(四類)", "建物型態", "租賃住宅服務", "捷運線", "建材分類")
categorical_cols <- setdiff(categorical_cols, to_remove)
# 將指定欄位轉換為 factor（類別型資料）
df[categorical_cols] <- lapply(df[categorical_cols], factor)


# 7. train/test split，先分箱 stratify
set.seed(42)
df$price_bin <- cut(df$總額元,
                    breaks = quantile(df$總額元, probs = seq(0, 1, 0.2), na.rm = TRUE),
                    include.lowest = TRUE)
train_idx <- createDataPartition(df$price_bin, p = 0.8, list = FALSE)
train_df <- df[train_idx, ] %>% select(-price_bin)
test_df  <- df[-train_idx, ] %>% select(-price_bin)


# 8. 在 train_df 上做 K-fold Cross Validation (tune 超參數)
k <- 10
train_df$price_bin <- cut(train_df$總額元,
                          breaks = quantile(train_df$總額元, probs = seq(0, 1, length.out = k + 1), na.rm = TRUE),
                          include.lowest = TRUE)
folds <- createFolds(train_df$price_bin, k = k, returnTrain = TRUE)
train_df <- train_df %>% select(-c(price_bin))

# 9. 定義超參數搜尋空間
grid <- expand.grid(
  num_leaves = c(31),  # 可根據需要調整
  learning_rate = c(0.05),
  feature_fraction = c(0.8),
  min_data_in_leaf = c(20),  # 可根據需要調整
  nrounds = c(500)  # 可根據需要調整
)

# 初始化最佳參數與結果儲存
best_rmse <- Inf
best_params <- list()
results_k_fold_tv <- data.frame()


# 10. 逐組超參數做 K-fold CV
for (i in 1:nrow(grid)) {
  params <- as.list(grid[i, ])
  rmse_list <- c()
  mape_list <- c()
  medape_list <- c()
  sdpe_list <- c()

  for (f in 1:length(folds)) {
    train_data <- train_df[folds[[f]], ]
    valid_data <- train_df[-folds[[f]], ]

    # 轉成矩陣型態（除了 y 變數以外）
    mat <- data.matrix(train_data %>% select(-總額元))
    colnames(mat) <- setdiff(names(train_data), "總額元")
    
    valid_mat <- data.matrix(valid_data %>% select(-總額元))
    colnames(valid_mat) <- setdiff(names(valid_data), "總額元")

    # 訓練資料 y 取 log
    dtrain <- lgb.Dataset(data = mat,
                          label = log(train_data$總額元),
                          categorical_feature = categorical_cols)
    dvalid <- lgb.Dataset(data = valid_mat,
                          label = log(valid_data$總額元),
                          categorical_feature = categorical_cols)
    # 訓練模型
    model <- lgb.train(
      params = c(params, list(objective = "regression", metric = "rmse")),
      data = dtrain,
      nrounds = params$nrounds,
      valids = list(valid = dvalid),
      verbose = -1,
      early_stopping_rounds = 100
    )

    # 預測的是 log(price)，要轉回原本單位
    preds_logs <- predict(model, valid_mat)
    preds <- exp(preds_logs) # 還原為租金金額（元）

    # 計算誤差（跟原始租金比）
    rmse <- sqrt(mean((preds - valid_data$總額元)^2))
    mape <- mean(abs(preds - valid_data$總額元) / valid_data$總額元)
    medape <- median(abs(preds - valid_data$總額元) / valid_data$總額元)
    sdpe <- sd(abs(preds - valid_data$總額元) / valid_data$總額元)

    # 計算誤差（跟原始租金比）
    rmse_list <- c(rmse_list, rmse)
    mape_list <- c(mape_list, mape)
    medape_list <- c(medape_list, medape)
    sdpe_list <- c(sdpe_list, sdpe)
  }

  avg_rmse <- mean(rmse_list)
  avg_mape <- mean(mape_list)
  avg_medape <- mean(medape_list)
  avg_sdpe <- mean(sdpe_list)

  cat(sprintf("Grid %d → CV RMSE: %.2f | MAPE: %.4f | MEAPE: %4f | SDPE: %4f\n", i, avg_rmse, avg_mape, avg_medape, avg_sdpe))

  if (avg_rmse < best_rmse) {
    best_rmse <- avg_rmse
    best_params <- params
  }
}

cat("Best Parameters:\n")
print(best_params)


# 11. 在完整 train_df 上訓練最終模型
train_mat <- data.matrix(train_df %>% select(-總額元))
colnames(train_mat) <- setdiff(names(train_df), "總額元")
final_model <- lgb.train(
  params = c(best_params, list(objective = "regression", metric = "rmse")),
  data = lgb.Dataset(data = train_mat, 
                    label = log(train_df$總額元),
                    categorical_feature = categorical_cols),
  nrounds = best_params$nrounds,
  verbose = -1
)


# 12. 用 test_df 預測，計算各種誤差
test_mat <- data.matrix(test_df %>% select(-總額元))
colnames(test_mat) <- setdiff(names(test_df), "總額元")

test_preds_log <- predict(final_model, test_mat)
test_preds <- exp(test_preds_log)

test_rmse <- sqrt(mean((test_preds - test_df$總額元)^2))
test_mape <- mean(abs(test_preds - test_df$總額元) / test_df$總額元)
test_meape <- median(abs(test_preds - test_df$總額元) / test_df$總額元)
test_sdpe <- sd(abs(test_preds - test_df$總額元) / test_df$總額元)

cat("\n🎯 Final Test RMSE:", round(test_rmse, 2), "\n")
cat("🎯 Final Test MAPE:", round(test_mape, 4), "\n")
cat("🎯 Final Test MEAPE:", round(test_meape, 4), "\n")
cat("🎯 Final Test SDPE:", round(test_sdpe, 4), "\n")

# 13. 輸出預測結果至 csv
final_result <- test_df %>%
  mutate(Predicted = test_preds,
         AbsError = abs(Predicted - 總額元),
         MAPE = abs(Predicted - 總額元) / 總額元)
write.csv(final_result, "lgbm_model_result_on_all_features.csv", row.names = FALSE)  # 可用於後續分析

# 14. 取得 feature importance
importance <- lgb.importance(final_model, percentage = TRUE)
importance$Gain <- format(importance$Gain, scientific = FALSE, digits = 4)
importance$Cover <- format(importance$Cover, scientific = FALSE, digits = 4)
importance$Frequency <- format(importance$Frequency, scientific = FALSE, digits = 4)
print(importance)

# 15. 存下模型（範例: 存成 txt 格式）
lgb.save(final_model, "final_lgbm_model.txt")  # 這行會存成 .txt，可以用 lgb.load() 載回
# 16. 存下特徵順序
writeLines(colnames(train_mat), "lgbm_feature_order.txt")
# 17. 存下所有類別欄位的 level order
saveRDS(lapply(train_df[, categorical_cols], levels), "factor_levels.rds")

