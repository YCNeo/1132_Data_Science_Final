# 讀取檔案
rent_trades <- read.csv(
  file        = "dataset/rent_trades_w_mrt.csv",      # 檔名
  header      = TRUE,            # 第一列是否為欄位名稱
  stringsAsFactors = FALSE,      # 字串不要轉為 factor
  fileEncoding = "UTF-8"         # 若有編碼問題，可指定
)

# 檢查前幾筆
head(rent_trades) 

# 隨機切分 1:4 的訓練集與測試集
set.seed(526)
n      <- nrow(rent_trades)
ntr    <- floor(0.8 * n)
train_idx <- sample(seq_len(n), size = ntr)

dtrain <- rent_trades[train_idx, ]
dtest  <- rent_trades[-train_idx, ]

cat("訓練：", nrow(dtrain), "測試：", nrow(dtest))

# Fit a linear regression model using log(單價元平方公尺, base=10) as the dependent variable
# (using the specified features as predictors)
model <- lm(log(單價元平方公尺, base=10) ~ 土地面積平方公尺 + 租賃年月日 + 總樓層數 + 建物總面積平方公尺 + 建物現況格局.房 + 建物現況格局.廳 + 建物現況格局.衛 + 文湖線 + 淡水信義線 + 新北投支線 + 松山新店線 + 小碧潭支線 + 中和新蘆線 + 板南線 + 環狀線 + distance_to_log, data=dtrain)

# Compute predictions on training and test sets
dtrain$predLogPrice <- predict(model, newdata=dtrain)
dtest$predLogPrice  <- predict(model, newdata=dtest)
library(ggplot2)

# Plot predicted log(單價元平方公尺) versus actual log(單價元平方公尺) (using ggplot2)
# (Example 7.2)
ggplot(data=dtest, aes(x=predLogPrice, y=log(單價元平方公尺, base=10))) +
  geom_point(alpha=0.2, color="black") +
  geom_smooth(aes(x=predLogPrice, y=log(單價元平方公尺, base=10)), color="black") +
  geom_line(aes(單價元平方公尺, y=log(單價元平方公尺, base=10)), color="blue", linetype=2) +
  scale_x_continuous(limits=c(floor(min(dtest$predLogPrice)), ceiling(max(dtest$predLogPrice)))) +
  scale_y_continuous(limits=c(floor(min(log(dtest$單價元平方公尺, base=10))), ceiling(max(log(dtest$單價元平方公尺, base=10)))))

# Plot residuals (predicted – actual log(單價元平方公尺)) versus predicted log(單價元平方公尺) (using ggplot2)
# (Example 7.3)
ggplot(data=dtest, aes(x=predLogPrice, y=predLogPrice - log(單價元平方公尺, base=10))) +
  geom_point(alpha=0.2, color="black") +
  geom_smooth(aes(x=predLogPrice, y=predLogPrice - log(單價元平方公尺, base=10)), color="black")

# Compute residuals summary (training and test)
summary(log(dtrain$單價元平方公尺, base=10) - dtrain$predLogPrice)
summary(log(dtest$單價元平方公尺, base=10) - dtest$predLogPrice)

# Compute R-squared (using the rsq function) (Example 7.4)
rsq <- function(y, f) { 1 - sum((y-f)^2) / sum((y-mean(y))^2) }
rsq_train <- rsq(log(dtrain$單價元平方公尺, base=10), dtrain$predLogPrice)
rsq_test  <- rsq(log(dtest$單價元平方公尺, base=10), dtest$predLogPrice)
print(paste("Training R-squared:", rsq_train))
print(paste("Test R-squared:", rsq_test))

# Compute RMSE (using the rmse function) (Example 7.5)
rmse <- function(y, f) { sqrt(mean((y-f)^2)) }
rmse_train <- rmse(log(dtrain$單價元平方公尺, base=10), dtrain$predLogPrice)
rmse_test  <- rmse(log(dtest$單價元平方公尺, base=10), dtest$predLogPrice)
print(paste("Training RMSE:", rmse_train))
print(paste("Test RMSE:", rmse_test))

# 將預測值和實際值轉換回原始單位（元/平方公尺）
dtest$predicted_price <- 10^dtest$predLogPrice
dtest$actual_price <- dtest$單價元平方公尺

# 選取十筆資料並顯示比較結果
comparison <- data.frame(
  實際租金 = round(dtest$actual_price, 2),
  預測租金 = round(dtest$predicted_price, 2),
  預測誤差 = round(dtest$predicted_price - dtest$actual_price, 2),
  誤差百分比 = round((dtest$predicted_price - dtest$actual_price) / dtest$actual_price * 100, 2)
)

# 顯示前十筆資料
print("測試集前十筆資料的預測結果：")
print(head(comparison, 10))

# (Optional) Print model coefficients and summary (informalexample 7.9, 7.10)
print("Model Coefficients:")
print(coefficients(model))
print("Model Summary:")
print(summary(model))
print(summary(model)$coefficients)

# (Optional) Compute residual error (informalexample 7.10)
df <- dim(dtrain)[1] - dim(summary(model)$coefficients)[1]
modelResidualError <- sqrt(sum(residuals(model)^2) / df)
print(paste("Model Residual Error:", modelResidualError)) 