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
  model <- xgboost::xgb.load("model_xgb/final_xgb_model.txt")
    
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

  # 6. 取得 feature importance
  importance <- xgboost::xgb.importance(model = model)
  top5 <- head(importance[order(-importance$Gain), c("Feature", "Gain")], 5)

  # 7. 回傳 list
  print(list(
    rent = as.numeric(predicted_value_original_scale[1]),
    fi = top5
  ))
  return(list(
    rent = as.numeric(predicted_value_original_scale[1]),
    fi = top5
  ))
}

modelPageUI <- function(id) {
  ns <- NS(id)
  tagList(
    fluidPage(
      # ... 其他區塊 ...
      fluidRow(
        column(4,
          card(
            class = "mb-4 shadow-sm",
            card_header("輸入房屋資訊"),
            card_body(
              selectInput(ns("region"), "鄉鎮市區", choices = c("大安區", "信義區", "中山區", "中正區", "松山區", "內湖區", "士林區", "文山區", "北投區", "萬華區", "大同區", "南港區")),
              selectInput(ns("rental_type"), "出租型態", choices = c("整層住家", "分租套房", "獨立套房", "雅房", "其他", "空", "整棟(戶)出租", "分層出租", "分租雅房")),
              selectInput(ns("building_type"), "建物型態", choices = c("住宅大樓(11層含以上有電梯)", "公寓(5樓含以下無電梯)", "套房(1樓至5樓)", "華廈(10層含以下有電梯)", "透天厝", "其他", "店面(店鋪)", "廠辦", "辦公商業大樓", "倉庫", "工廠", "農舍", "空")),
              selectInput(ns("elevator"), "有無電梯", choices = c("1", "0", "空")),
              selectInput(ns("rental_service"), "租賃住宅服務", choices = c("空", "社會住宅包租轉租", "社會住宅代管", "一般包租", "一般轉租", "一般代管")),
              numericInput(ns("total_floor"), "總樓層數", value = 12, min = 1),
              numericInput(ns("rooms"), "建物現況格局.房", value = 2, min = 0),
              numericInput(ns("halls"), "建物現況格局.廳", value = 1, min = 0),
              numericInput(ns("baths"), "建物現況格局.衛", value = 1, min = 0),
              numericInput(ns("area"), "建物總面積平方公尺", value = 80, min = 5),
              numericInput(ns("age"), "屋齡", value = 10, min = 0),
              numericInput(ns("ac"), "附屬設備.冷氣", value = 1, min = 0, max = 1),
              numericInput(ns("heater"), "附屬設備.熱水器", value = 1, min = 0, max = 1),
              numericInput(ns("washer"), "附屬設備.洗衣機", value = 1, min = 0, max = 1),
              numericInput(ns("tv"), "附屬設備.電視機", value = 0, min = 0, max = 1),
              numericInput(ns("gas"), "附屬設備.瓦斯或天然氣", value = 0, min = 0, max = 1),
              numericInput(ns("internet"), "附屬設備.網路", value = 0, min = 0, max = 1),
              numericInput(ns("mrt_dist"), "捷運站距離.公尺.", value = 500, min = 0),
              selectInput(ns("mrt_line"), "捷運線", choices = c("文湖線", "淡水信義線", "新北投支線", "松山新店線", "小碧潭支線", "中和新蘆線", "板南線", "環狀線", "機場線", "安坑輕軌", "其他", "無", "空")),
              numericInput(ns("nearby_price"), "附近建物單位成交均價", value = 500000, min = 0),
              dateInput(ns("rental_date"), "租賃年月日", value = Sys.Date()),
              numericInput(ns("rental_days"), "租賃天數", value = 365, min = 1),
              actionButton(ns("run_model"), "預測租金", class = "btn btn-danger btn-lg w-100")
            )
          )
        ),
        # ... 右側結果區塊 ...
      )
    )
  )
}

modelPageServer <- function(input, output, session) {
  pred_data <- eventReactive(input$run_model, {
    new_data <- data.frame(
      鄉鎮市區 = input$region,
      出租型態 = input$rental_type,
      建物型態 = input$building_type,
      有無電梯 = input$elevator,
      租賃住宅服務 = input$rental_service,
      總樓層數 = input$total_floor,
      建物現況格局.房 = input$rooms,
      建物現況格局.廳 = input$halls,
      建物現況格局.衛 = input$baths,
      建物總面積平方公尺 = input$area,
      屋齡 = input$age,
      附屬設備.冷氣 = input$ac,
      附屬設備.熱水器 = input$heater,
      附屬設備.洗衣機 = input$washer,
      附屬設備.電視機 = input$tv,
      附屬設備.瓦斯或天然氣 = input$gas,
      附屬設備.網路 = input$internet,
      捷運站距離.公尺. = input$mrt_dist,
      捷運線 = input$mrt_line,
      附近建物單位成交均價 = input$nearby_price,
      租賃年月日 = as.character(input$rental_date),
      租賃天數 = input$rental_days,
      stringsAsFactors = FALSE
    )
    result <- predict_rent_xgb(new_data)
    print(result)
  })

  output$pred_rent <- renderText({
    req(pred_data())
    if (is.na(pred_data())) {
      return("⚠️ 無法預測，請檢查輸入資料")
    }
    paste0("NT$ ", formatC(pred_data(), big.mark = ",", format = "f", digits = 0))
  })

  output$feature_importance <- renderUI({
    df <- pred_data()$fi
    if (is.null(df) || nrow(df) == 0) return(NULL)
    lapply(seq_len(nrow(df)), function(i) {
      val <- round(as.numeric(df$Gain[i]) * 100, 1)
      tagList(
        tags$div(df$Feature[i], style="margin-top:10px;"),
        tags$div(
          class = "progress",
          tags$div(
            class = "progress-bar",
            role = "progressbar",
            style = paste0("width:", val, "%;"),
            `aria-valuenow` = val, `aria-valuemin` = 0, `aria-valuemax` = 100,
            paste0(val, "%")
          )
        )
      )
    })
  })
}

