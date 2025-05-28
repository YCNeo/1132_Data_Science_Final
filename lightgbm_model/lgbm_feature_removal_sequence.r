library(dplyr)
library(caret)
library(lightgbm)
library(readr)

run_removal_effect_on_test <- function(
  train_df,
  test_df,
  importance_df,
  best_params,
  categorical_cols,
  k_folds   = 5,
  max_remove= 10
) {
  print(match.call())
  # 1) 把特征按 Gain 从小到大排好
  ranked_feats <- importance_df %>%
    arrange(Gain) %>%
    pull(Feature)

  # 2) 先在 train_df 上做一次分箱 stratify，（后面 CV 用）
  train_df$bin <- cut(
    train_df$總額元,
    breaks = quantile(train_df$總額元, probs = seq(0,1, length.out = k_folds+1)),
    include.lowest = TRUE
  )
  folds <- createFolds(train_df$bin, k = k_folds, returnTrain = TRUE)
  train_df$bin <- NULL

  # 4) 结果容器
  res <- tibble(
    Num_Removed      = integer(),
    Removed_Features = character(),
    Train_RMSE       = numeric(),
    Test_RMSE        = numeric(),
    Test_MAPE        = numeric(),
    Test_MEAPE       = numeric(),
    Test_SDPE        = numeric()
  )

  set.seed(42)
  for (n in 0:max_remove) {
    print(n)
    # 5) 取出要剔除的 n 个最小 Gain features
    low_feats <- if (n==0) character(0) else ranked_feats[1:n]
    use_feats <- setdiff(names(train_df), c("總額元", low_feats))
    if (length(use_feats)==0) next

    # ——— 5.1 CV 计算平均 Train RMSE ———
    rmse_cv <- c()
    for (f in seq_along(folds)) {
      tr <- train_df[ folds[[f]],   ]
      va <- train_df[-folds[[f]],   ]
      mtr <- data.matrix(tr[ , use_feats])
      mva <- data.matrix(va[ , use_feats])
      cat_feats <- intersect(categorical_cols, use_feats)

      dtr <- lgb.Dataset(
        data = mtr,
        label= log(tr$總額元),
        categorical_feature = cat_feats
      )
      mdl <- lgb.train(
        params   = c(best_params, list(objective="regression", metric="rmse")),
        data     = dtr,
        nrounds  = best_params$nrounds,
        verbose  = -1
      )
      preds_va <- exp(predict(mdl, mva))
      rmse_cv   <- c(rmse_cv, sqrt(mean((preds_va - va$總額元)^2)))
    }
    train_rmse_mean <- mean(rmse_cv)

    # ——— 5.2 用全部 train_df 训练 final，再跑 test_df ———
    full_mat <- data.matrix(train_df[ , use_feats])
    dfull    <- lgb.Dataset(
      data = full_mat,
      label= log(train_df$總額元),
      categorical_feature = intersect(categorical_cols, use_feats)
    )
    mdl_full  <- lgb.train(
      params   = c(best_params, list(objective="regression", metric="rmse")),
      data     = dfull,
      nrounds  = best_params$nrounds,
      verbose  = -1
    )
    test_mat  <- data.matrix(test_df[ , use_feats])
    preds_te  <- exp(predict(mdl_full, test_mat))
    test_rmse <- sqrt(mean((preds_te - test_df$總額元)^2))
    test_mape <- mean(abs(preds_te - test_df$總額元) / test_df$總額元)
    test_meape<- median(abs(preds_te - test_df$總額元) / test_df$總額元)
    test_sdpe <- sd(abs(preds_te - test_df$總額元) / test_df$總額元)

    # 6) 存结果
    res <- add_row(
      res,
      Num_Removed      = n,
      Removed_Features = paste(low_feats, collapse = ", "),
      Train_RMSE       = train_rmse_mean,
      Test_RMSE        = test_rmse,
      Test_MAPE        = test_mape,
      Test_MEAPE       = test_meape,
      Test_SDPE        = test_sdpe
    )
  }

  return(res)
}

# 指定要讀取的欄位
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

# 讀取資料
full_data <- read_csv("rent_mrg.csv", show_col_types = FALSE)
# 篩選欄位
df <- full_data %>% select(all_of(use_columns))

## 篩掉「天數太短」的資料 
df <- df %>% filter(租賃天數 >= 30)

# 假設你原始資料叫 df
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

df$出租型態[is.na(df$出租型態)] <- "未知"
df$租賃住宅服務[is.na(df$租賃住宅服務)] <- "未知"
df$租賃年月日 <- as.numeric(df$租賃年月日)


# 指定分類欄位 並篩掉沒用的features
categorical_cols <- c("鄉鎮市區", "出租型態", "租賃層次(四類)", "建物型態", "租賃住宅服務", "捷運線", "建材分類")
# 將分類欄位轉為因子類型
df[categorical_cols] <- lapply(df[categorical_cols], factor)


# 
set.seed(42)
df$price_bin <- cut(df$總額元,
                    breaks = quantile(df$總額元, probs = seq(0, 1, 0.2), na.rm = TRUE),
                    include.lowest = TRUE)
train_idx <- createDataPartition(df$price_bin, p = 0.8, list = FALSE)
train_df <- df[train_idx, ] %>% select(-price_bin)
test_df  <- df[-train_idx, ] %>% select(-price_bin)


# 3️⃣ 在 train_df 上做 K-fold CV（tune 超參數）
k <- 10
train_df$price_bin <- cut(train_df$總額元,
                          breaks = quantile(train_df$總額元, probs = seq(0, 1, length.out = k + 1), na.rm = TRUE),
                          include.lowest = TRUE)
folds <- createFolds(train_df$price_bin, k = k, returnTrain = TRUE)
train_df <- train_df %>% select(-c(price_bin))

grid <- expand.grid(
  num_leaves = c(31),  # 可根據需要調整
  learning_rate = c(0.05),
  feature_fraction = c(0.8),
  min_data_in_leaf = c(20),  # 可根據需要調整
  nrounds = c(500)  # 可根據需要調整
)

best_rmse <- Inf
best_params <- list()
results_k_fold_tv <- data.frame()
print(colnames(train_df))

for (i in 1:nrow(grid)) {
  params <- as.list(grid[i, ])
  rmse_list <- c()
  mape_list <- c()
  medape_list <- c()
  sdpe_list <- c()

  for (f in 1:length(folds)) {
    train_data <- train_df[folds[[f]], ]
    valid_data <- train_df[-folds[[f]], ]


    # categorical_cols 為原始 factor 欄位名稱
    # 將分類欄位轉為數字編碼
    mat <- data.matrix(train_data %>% select(-總額元))
    colnames(mat) <- setdiff(names(train_data), "總額元")
    
    valid_mat <- data.matrix(valid_data %>% select(-總額元))
    colnames(valid_mat) <- setdiff(names(valid_data), "總額元")

    # 訓練資料用 log(price)
    dtrain <- lgb.Dataset(data = mat,
                          label = log(train_data$總額元),
                          categorical_feature = categorical_cols)
    # 驗證資料 label 可省略，因為我們自己計算誤差
    dvalid <- lgb.Dataset(data = valid_mat,
                          label = log(valid_data$總額元),
                          categorical_feature = categorical_cols)

    model <- lgb.train(
      params = c(params, list(objective = "regression", metric = "rmse")),
      data = dtrain,
      nrounds = params$nrounds,
      valids = list(valid = dvalid),
      verbose = -1,
      #early_stopping_rounds = 100
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

train_mat <- data.matrix(train_df %>% select(-總額元))
colnames(train_mat) <- setdiff(names(train_df), "總額元")

# 4.最終模型：在完整 train_df 上訓練，test_df 上預測
final_model <- lgb.train(
  params = c(best_params, list(objective = "regression", metric = "rmse")),
  data = lgb.Dataset(data = train_mat, 
                    label = log(train_df$總額元),
                    categorical_feature = categorical_cols),
  nrounds = best_params$nrounds,
  verbose = -1
)


# 預測 test_df
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

# 輸出預測結果
final_result <- test_df %>%
  mutate(Predicted = test_preds,
         AbsError = abs(Predicted - 總額元),
         MAPE = abs(Predicted - 總額元) / 總額元)
write.csv(final_result, "test_prediction_results.csv", row.names = FALSE)  # 可用於後續分析

# importance 是你前面 final model 訓練後得到的
importance <- lgb.importance(final_model, percentage = TRUE)

importance$Gain <- format(importance$Gain, scientific = FALSE, digits = 5)
importance$Cover <- format(importance$Cover, scientific = FALSE, digits = 5)
importance$Frequency <- format(importance$Frequency, scientific = FALSE, digits = 5)
print(importance)


print(best_params)
str(best_params)
res <- results <- run_removal_effect_on_test(
  train_df     = train_df,
  test_df      = test_df,
  importance_df = importance,
  best_params  = best_params,
  categorical_cols = c(
    "鄉鎮市區", "出租型態", "租賃層次(四類)", "建物型態", "租賃住宅服務", "捷運線", "建材分類"),  # 這是分類欄位
  k_folds     = 10,              # 你想用幾折交叉驗證
  max_remove   = 20             # 你想試前 1~20 支特徵
)
write.csv(res, "feature_removal_results.csv", row.names=FALSE, fileEncoding="UTF-8")