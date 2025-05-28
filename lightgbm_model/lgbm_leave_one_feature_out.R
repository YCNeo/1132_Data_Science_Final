library(readr)
library(dplyr)
library(caret)
library(lightgbm)

# 1) 讀資料 & 前處理
full_data <- read_csv("../dataset/rent_mrg.csv", show_col_types = FALSE)
use_columns <- c(
  "鄉鎮市區","總額元","租賃年月日","出租型態",
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
  "附近建物單位成交均價"
)
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

# 2) Train/Test Split （price_bin Stratify）
set.seed(42)
df$price_bin <- cut(df$總額元,
                    breaks = quantile(df$總額元, probs=seq(0,1,0.2), na.rm=TRUE),
                    include.lowest=TRUE)
train_idx <- createDataPartition(df$price_bin, p=0.8, list=FALSE)
train_df <- df[train_idx, ] %>% select(-price_bin)
test_df  <- df[-train_idx,] %>% select(-price_bin)
all_feats <- setdiff(names(train_df), "總額元")

# 3) 設定參數
params <- list(
  num_leaves = 31,
  learning_rate = 0.05,
  feature_fraction = 0.8,
  min_data_in_leaf = 20,
  nrounds = 500
)

# 4) 訓練 base model (全特徵)
dparams_base <- c(params, list(objective="regression", metric="rmse"))
set.seed(42)
mbase <- lgb.train(
  params = dparams_base,
  data   = lgb.Dataset(data = data.matrix(train_df[, all_feats]), label = log(train_df$總額元), categorical_feature = intersect(categorical_cols, all_feats)),
  nrounds = params$nrounds,
  verbose = -1
)
preds_base <- exp(predict(mbase, data.matrix(test_df[, all_feats])))
truth      <- test_df$總額元
rmse_base  <- sqrt(mean((preds_base - truth)^2))

# 5) 依序移除每個特徵並記錄結果
results <- data.frame(
  Feature_Removed = character(),
  Test_RMSE = numeric(),
  RMSE_Diff = numeric(),
  CI_Lower = numeric(),
  CI_Upper = numeric(),
  p_value = numeric(),
  stringsAsFactors = FALSE
)

B <- 100000 # 若很慢可改成 5000 或 3000；如果你機器很強可調大

for (feat in all_feats) {
  new_feats <- setdiff(all_feats, feat)
  set.seed(42)
  mnew <- lgb.train(
    params = dparams_base,
    data   = lgb.Dataset(data = data.matrix(train_df[, new_feats]), label = log(train_df$總額元), categorical_feature = intersect(categorical_cols, new_feats)),
    nrounds = params$nrounds,
    verbose = -1
  )
  preds_new <- exp(predict(mnew, data.matrix(test_df[, new_feats])))
  rmse_new  <- sqrt(mean((preds_new - truth)^2))
  # Bootstrap
  diffs <- numeric(B)
  set.seed(42)
  for (b in seq_len(B)) {
    idx <- sample.int(length(truth), length(truth), replace = TRUE)
    diffs[b] <- sqrt(mean((preds_base[idx] - truth[idx])^2)) - sqrt(mean((preds_new[idx] - truth[idx])^2))
  }
  ci    <- quantile(diffs, c(0.025, 0.975))
  pval  <- 2 * min(mean(diffs <= 0), mean(diffs >= 0))
  # 加入結果表
  results <- rbind(
    results,
    data.frame(
      Feature_Removed = feat,
      Test_RMSE = rmse_new,
      RMSE_Diff = rmse_base - rmse_new,
      CI_Lower = ci[1],
      CI_Upper = ci[2],
      p_value = pval,
      stringsAsFactors = FALSE
    )
  )
  cat(sprintf("Remove %s → Test_RMSE: %.2f, RMSE_diff: %.2f, CI=[%.2f, %.2f], p=%.4f\n",
              feat, rmse_new, rmse_base - rmse_new, ci[1], ci[2], pval))
}

# 6) base model 也加進去，方便比較
results <- rbind(
  data.frame(
    Feature_Removed = "None (All features)",
    Test_RMSE = rmse_base,
    RMSE_Diff = 0,
    CI_Lower = NA,
    CI_Upper = NA,
    p_value = NA,
    stringsAsFactors = FALSE
  ),
  results
)

# 7) 依 Test_RMSE 由低到高排序
results <- results %>% arrange(Test_RMSE)

# 8) 輸出 csv
write.csv(results, "leave_one_feature_out_results.csv", row.names = FALSE)
