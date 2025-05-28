library(readr)
library(dplyr)
library(caret)
library(lightgbm)
library(stringr)
library(tibble)

#── 固定參數：bootstrap 次數、LightGBM 參數 ──
B      <- 100000
params <- list(
  num_leaves=31, learning_rate=0.05, feature_fraction=0.8,
  min_data_in_leaf=20, nrounds=500
)

#── 1) 讀資料 & 前處理（與之前完全相同） ──
full_data <- read_csv("rent_mrg.csv", show_col_types=FALSE)
use_columns <- c("鄉鎮市區","總額元","租賃年月日","出租型態",
                 "租賃層次(四類)","總樓層數","建物型態",
                 "交易筆棟數-土地","交易筆棟數-建物",
                 "租賃住宅服務","租賃天數","有無管理組織",
                 "有無管理員","有無附傢俱","有無電梯",
                 "建物現況格局-房","建物現況格局-廳",
                 "建物現況格局-衛","建物現況格局-隔間",
                 "建物總面積平方公尺","屋齡","建材分類",
                 "附屬設備-冷氣","附屬設備-熱水器","附屬設備-洗衣機",
                 "附屬設備-電視機","附屬設備-冰箱",
                 "附屬設備-瓦斯或天然氣","附屬設備-有線電視",
                 "附屬設備-網路","捷運站距離(公尺)",
                 "文湖線","淡水信義線","新北投支線","松山新店線",
                 "小碧潭支線","中和新蘆線","板南線","環狀線",
                 "附近建物單位成交均價")
df <- full_data %>%
  select(all_of(use_columns)) %>%
  filter(租賃天數 >= 30) %>%
  mutate(捷運線 = case_when(
    文湖線==1 ~ "文湖線", 淡水信義線==1 ~ "淡水信義線",
    新北投支線==1 ~ "新北投支線", 松山新店線==1 ~ "松山新店線",
    小碧潭支線==1 ~ "小碧潭支線", 中和新蘆線==1 ~ "中和新蘆線",
    板南線==1 ~ "板南線", 環狀線==1 ~ "環狀線", TRUE ~ "無捷運"
  )) %>%
  select(-c(文湖線,淡水信義線,新北投支線,松山新店線,
            小碧潭支線,中和新蘆線,板南線,環狀線))
df$出租型態[is.na(df$出租型態)] <- "未知"
df$租賃住宅服務[is.na(df$租賃住宅服務)] <- "未知"
df$租賃年月日 <- as.numeric(df$租賃年月日)
categorical_cols <- c("鄉鎮市區","出租型態","租賃層次(四類)",
                      "建物型態","租賃住宅服務","捷運線","建材分類")
df[categorical_cols] <- lapply(df[categorical_cols], factor)

#── 2) Stratified train/test split ──
set.seed(42)
df$price_bin <- cut(df$總額元,
                    breaks = quantile(df$總額元, probs = seq(0,1,0.2), na.rm=TRUE),
                    include.lowest=TRUE)
train_idx <- createDataPartition(df$price_bin, p=0.8, list=FALSE)
train_df  <- df[train_idx, ] %>% select(-price_bin)
test_df   <- df[-train_idx,] %>% select(-price_bin)

all_feats <- setdiff(names(train_df), "總額元")

#── 3) 讀入 removal_vs_test.csv，並依筆數跑檢定 ──
scenarios <- read_csv("feature_removal_results.csv", show_col_types=FALSE)

results <- scenarios %>%
  mutate(
    CI_lower = NA_real_,
    CI_upper = NA_real_,
    p_value  = NA_real_
  )

for (i in seq_len(nrow(scenarios))) {
  # 3.1 parse 要移除的特徵
  to_remove <- str_split(scenarios$Removed_Features[i], ",\\s*")[[1]]
  new_feats <- setdiff(all_feats, to_remove)
  print("要移除的特徵:")
  print(to_remove)
  print("新特徵清單:")
  print(new_feats)
  
  # 3.2 訓練 Base／New 模型
  # Base
  mat_b <- data.matrix(train_df[, all_feats]); colnames(mat_b)<-all_feats
  dtrain_b <- lgb.Dataset(mat_b, label=log(train_df$總額元),
                          categorical_feature=intersect(categorical_cols, all_feats))
  m_b <- lgb.train(params=c(params, list(objective="regression", metric="rmse")),
                   data=dtrain_b, nrounds=params$nrounds, verbose=-1)
  # New
  mat_n <- data.matrix(train_df[, new_feats]); colnames(mat_n)<-new_feats
  dtrain_n <- lgb.Dataset(mat_n, label=log(train_df$總額元),
                          categorical_feature=intersect(categorical_cols, new_feats))
  m_n <- lgb.train(params=c(params, list(objective="regression", metric="rmse")),
                   data=dtrain_n, nrounds=params$nrounds, verbose=-1)
  
  # 3.3 對測試集預測
  test_b <- exp(predict(m_b, data.matrix(test_df[, all_feats])))
  test_n <- exp(predict(m_n, data.matrix(test_df[, new_feats])))
  truth  <- test_df$總額元
  importance_b <- lgb.importance(m_b, percentage=TRUE)
  importance_n <- lgb.importance(m_n, percentage=TRUE)
  print("Base 模型特徵重要性:")
  print(importance_b)
  print("New 模型特徵重要性:")
  print(importance_n)
  
  # 3.4 bootstrap 差值
  diffs <- numeric(B)
  set.seed(42)
  for (b in seq_len(B)) {
    idx <- sample.int(length(truth), length(truth), replace=TRUE)
    rmse_b <- sqrt(mean((test_b[idx] - truth[idx])^2))
    rmse_n <- sqrt(mean((test_n[idx] - truth[idx])^2))
    diffs[b] <- rmse_b - rmse_n
  }
  ci    <- quantile(diffs, c(0.025, 0.975))
  pval  <- 2 * min(mean(diffs <= 0), mean(diffs >= 0))
  
  # 3.5 存回結果表
  results$CI_lower[i] <- ci[1]
  results$CI_upper[i] <- ci[2]
  results$p_value [i] <- pval
  
  cat(sprintf(
    "Scenario %2d (%2d feats removed): CI=[%.1f,%.1f], p=%.3f\n",
    i, scenarios$Num_Removed[i], ci[1], ci[2], pval
  ))
}

#── 4) 輸出完整結果 ──
write_csv(results, "feature_removal_significance_results.csv")
