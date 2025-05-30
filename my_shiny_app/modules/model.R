## modules/model.R
# Model Page module: UI and Server definitions

# 載入模型 function
# 確認路徑正確，相對於 app.R
source("model_xgb/predict_rent_xgb.R", local = TRUE)
source("model_lgbm/predict_rent_lgbm.R", local = TRUE)

# UI function for Model page
modelPageUI <- function(id) {
  ns <- NS(id)
  tagList(
    fluidPage(
      # 1. 返回鍵 + 大標題
       # 這裡引用 www/custom.css
        tags$head(
        tags$link(rel = "stylesheet", href = "custom.css")
        ),
        tags$div(
            style = "display: flex;
                align-items: center;
                padding: 20px 20px;  /* 你要的內邊距 */
                gap: 20px;",
            # 返回鍵
            actionButton(
                "back_welcome",
                label = span(icon("arrow-left"),
                class = "icon-circle"),
                class = "btn-no-border"
            ),
            # 標題
            tags$h1("租金預測", class = "text-primary custom-title", style = "margin: 0;")
        ),

      # 2. 模型選擇 + 預測工具卡片 (左右兩欄)
      fluidRow(
        # 左側：模型選擇與輸入欄位
        column(4,
          card(
            class = "mb-4 shadow-sm",
            card_header("預測模型與輸入"),
            card_body(
              # 模型選擇 (用 radioButtons)
              radioButtons(ns("model_choice"), "選擇預測模型：",
                           choices = c("LGBM", "XGBoost"),
                           selected = "LGBM", inline = TRUE),
                           # 選擇顯示前幾大特徵的數量
              numericInput(ns("num_top_features"), "顯示影響前幾大特徵：", value = 8, min = 1, step = 1, width = "100%"),
              
              # 動態顯示輸入欄位
              # 預測按鈕
              actionButton(ns("run_model"), "開始預測", class = "btn-primary mt-3", width = "100%"),
              hr(), # 分隔線
              uiOutput(ns("input_panel"))
            )
          )
        ),

        # 右側：預測結果
        column(8,
          card(
            class = "mb-4 shadow-sm",
            style = "min-height: 600px;",
            card_header("預測結果"),
            card_body(
              # 預測月租金
              tags$div(style = "background:#f8f9fa; padding:20px; border-radius:8px; text-align:center; margin-bottom:20px;",
                tags$h5("預測月租金"),
                tags$h2(textOutput(ns("pred_rent")), style = "color:#d9534f;")
              ),
              # 特徵重要性顯示區域
              uiOutput(ns("importance_panel"))
              # 影響因素重要性 和 類似物件參考 這裡已移除
            )
          )
        )
      )
    )
  )
}

# Server function for Model page
modelPageServer <- function(input, output, session) {
  # 載入 lightgbm 套件以計算特徵重要性
  library(lightgbm)
  # 載入 ggplot2 用於視覺化
  library(ggplot2)
  library(dplyr)

  # 讀取 LGBM 的 factor levels
  # 確認 factor_levels.rds 在 model_lgbm 資料夾內
  factor_levels_lgbm <- tryCatch({
    readRDS("model_lgbm/factor_levels.rds")
  }, error = function(e) {
    warning("無法載入 LGBM factor_levels.rds 檔案: ", e$message)
    NULL # 載入失敗時回傳 NULL
  })

  # 動態產生輸入欄位 UI
  output$input_panel <- renderUI({
    ns <- session$ns # 確保在 renderUI 內部也使用模組的 namespace
    if (input$model_choice == "XGBoost") {
      # XGBoost 輸入欄位 (根據 predict_rent_xgb.R 的 required_cols 和 factor_levels)
      tagList(
        selectInput(ns("xgb_region"), "鄉鎮市區", choices = c("大安區", "信義區", "中山區", "中正區", "松山區", "內湖區", "士林區", "文山區", "北投區", "萬華區", "大同區", "南港區")),
        selectInput(ns("xgb_rental_type"), "出租型態", choices = c("分租套房", "獨立套房", "空", "整棟(戶)出租", "分層出租", "分租雅房")),
        selectInput(ns("xgb_building_type"), "建物型態", choices = c("住宅大樓(11層含以上有電梯)", "公寓(5樓含以下無電梯)", "華廈(10層含以下有電梯)", "透天厝", "其他", "店面(店鋪)", "廠辦", "辦公商業大樓")),
        selectInput(ns("xgb_elevator"), "有無電梯", choices = c("1", "0", "空")),
        selectInput(ns("xgb_rental_service"), "租賃住宅服務", choices = c("空", "社會住宅包租轉租", "社會住宅代管", "一般包租", "一般轉租", "一般代管")),
        numericInput(ns("xgb_total_floor"), "總樓層數", value = 12, min = 1),
        numericInput(ns("xgb_rooms"), "建物現況格局.房", value = 2, min = 0),
        numericInput(ns("xgb_halls"), "建物現況格局.廳", value = 1, min = 0),
        numericInput(ns("xgb_baths"), "建物現況格局.衛", value = 1, min = 0),
        numericInput(ns("xgb_area"), "建物總面積平方公尺", value = 80, min = 5),
        numericInput(ns("xgb_age"), "屋齡", value = 10, min = 0),
        numericInput(ns("xgb_ac"), "附屬設備.冷氣", value = 1, min = 0, max = 1),
        numericInput(ns("xgb_heater"), "附屬設備.熱水器", value = 1, min = 0, max = 1),
        numericInput(ns("xgb_washer"), "附屬設備.洗衣機", value = 1, min = 0, max = 1),
        numericInput(ns("xgb_tv"), "附屬設備.電視機", value = 0, min = 0, max = 1),
        numericInput(ns("xgb_gas"), "附屬設備.瓦斯或天然氣", value = 0, min = 0, max = 1),
        numericInput(ns("xgb_internet"), "附屬設備.網路", value = 0, min = 0, max = 1),
        numericInput(ns("xgb_mrt_dist"), "捷運站距離.公尺.", value = 500, min = 0),
        selectInput(ns("xgb_mrt_line"), "捷運線", choices = c("文湖線", "淡水信義線", "新北投支線", "松山新店線", "中和新蘆線", "板南線", "環狀線")),
        numericInput(ns("xgb_nearby_price"), "附近建物單位成交均價", value = 500000, min = 0),
        dateInput(ns("xgb_rental_date"), "租賃年月日", value = Sys.Date()),
        numericInput(ns("xgb_rental_days"), "租賃天數", value = 365, min = 1)
      )
    } else if (input$model_choice == "LGBM") {
      # LGBM 輸入欄位 (根據 predict_rent_lgbm.R 的 input 說明和 factor_levels)
      # 根據 factor_levels_lgbm 是否成功載入來決定 choices
      region_choices <- if (!is.null(factor_levels_lgbm)) factor_levels_lgbm$鄉鎮市區 else c("未知")
      rental_type_choices <- if (!is.null(factor_levels_lgbm)) factor_levels_lgbm$出租型態 else c("未知")
      floor_type_choices <- if (!is.null(factor_levels_lgbm)) factor_levels_lgbm$`租賃層次(四類)` else c("未知")
      building_type_choices <- if (!is.null(factor_levels_lgbm)) factor_levels_lgbm$建物型態 else c("未知")
      rental_service_choices <- if (!is.null(factor_levels_lgbm)) factor_levels_lgbm$租賃住宅服務 else c("未知")
      mrt_line_choices <- if (!is.null(factor_levels_lgbm)) factor_levels_lgbm$捷運線 else c("未知")
      material_choices <- if (!is.null(factor_levels_lgbm)) factor_levels_lgbm$建材分類 else c("未知")

      tagList(
        # 分類變數 (下拉選單)
        selectInput(ns("lgbm_region"), "鄉鎮市區", choices = region_choices),
        selectInput(ns("lgbm_rental_type"), "出租型態", choices = rental_type_choices),
        selectInput(ns("lgbm_floor_type"), "租賃層次", choices = floor_type_choices),
        selectInput(ns("lgbm_building_type"), "建物型態", choices = building_type_choices),
        selectInput(ns("lgbm_rental_service"), "租賃住宅服務", choices = rental_service_choices),
        selectInput(ns("lgbm_mrt_line"), "捷運線", choices = mrt_line_choices),
        selectInput(ns("lgbm_material"), "建材分類", choices = material_choices),
        # 數值變數 (Numeric Input)
        numericInput(ns("lgbm_total_floor"), "總樓層數", value = 12, min = 1),
        numericInput(ns("lgbm_rooms"), "建物現況格局-房", value = 2, min = 0),
        numericInput(ns("lgbm_halls"), "建物現況格局-廳", value = 1, min = 0),
        numericInput(ns("lgbm_baths"), "建物現況格局-衛", value = 1, min = 0),
        numericInput(ns("lgbm_partition"), "建物現況格局-隔間", value = 1, min = 0, max = 1), # 假設是 0/1
        numericInput(ns("lgbm_area"), "建物總面積平方公尺", value = 80, min = 5),
        numericInput(ns("lgbm_age"), "屋齡", value = 10, min = 0),
        numericInput(ns("lgbm_land_trans"), "交易筆棟數-土地", value = 0, min = 0, max = 0), # 假設是數字
        numericInput(ns("lgbm_building_trans"), "交易筆棟數-建物", value = 1, min = 0), # 假設是數字
        numericInput(ns("lgbm_manage_org"), "有無管理組織", value = 0, min = 0, max = 1), # 假設是 0/1
        numericInput(ns("lgbm_manager"), "有無管理員", value = 0, min = 0, max = 1), # 假設是 0/1
        numericInput(ns("lgbm_furniture"), "有無附傢俱", value = 0, min = 0, max = 1), # 假設是 0/1
        numericInput(ns("lgbm_elevator"), "有無電梯", value = 0, min = 0, max = 1), # 假設是 0/1
        numericInput(ns("lgbm_ac"), "附屬設備-冷氣", value = 1, min = 0, max = 1),
        numericInput(ns("lgbm_heater"), "附屬設備-熱水器", value = 1, min = 0, max = 1),
        numericInput(ns("lgbm_washer"), "附屬設備-洗衣機", value = 1, min = 0, max = 1),
        numericInput(ns("lgbm_tv"), "附屬設備-電視機", value = 0, min = 0, max = 1),
        numericInput(ns("lgbm_fridge"), "附屬設備-冰箱", value = 0, min = 0, max = 1), # LGBM 似乎有冰箱
        numericInput(ns("lgbm_gas"), "附屬設備-瓦斯或天然氣", value = 0, min = 0, max = 1),
        numericInput(ns("lgbm_cable"), "附屬設備-有線電視", value = 0, min = 0, max = 1), # LGBM 似乎有有線電視
        numericInput(ns("lgbm_internet"), "附屬設備-網路", value = 0, min = 0, max = 1),
        numericInput(ns("lgbm_mrt_dist"), "捷運站距離(公尺)", value = 500, min = 0),
        numericInput(ns("lgbm_nearby_price"), "附近建物單位成交均價", value = 500000, min = 0),
        dateInput(ns("lgbm_rental_date"), "租賃年月日", value = Sys.Date()),
        numericInput(ns("lgbm_rental_days"), "租賃天數", value = 365, min = 1)
      )
    }
  })

  # Reactive: 當按下預測按鈕執行模型
  pred_data <- eventReactive(input$run_model, {
    print("eventReactive triggered") # 確認 eventReactive 是否被觸發
    print(input$lgbm_region) # 檢查 lgbm_region 的值
    print(input$lgbm_rental_type) # 檢查 lgbm_rental_type 的值
    print(input$lgbm_floor_type) # 檢查 lgbm_floor_type 的值
    print(input$lgbm_building_type) # 檢查 lgbm_building_type 的值
    print(input$lgbm_rental_service) # 檢查 lgbm_rental_service 的值
    print(input$lgbm_mrt_line) # 檢查 lgbm_mrt_line 的值
    print(input$lgbm_material) # 檢查 lgbm_material 的值
    print(input$lgbm_total_floor) # 檢查 lgbm_total_floor 的值
    print(input$lgbm_rooms) # 檢查 lgbm_rooms 的值
    print(input$lgbm_halls) # 檢查 lgbm_halls 的值
    print(input$lgbm_baths) # 檢查 lgbm_baths 的值
    print(input$lgbm_partition) # 檢查 lgbm_partition 的值
    print(input$lgbm_area) # 檢查 lgbm_area 的值
    print(input$lgbm_age) # 檢查 lgbm_age 的值
    print(input$lgbm_land_trans) # 檢查 lgbm_land_trans 的值
    print(input$lgbm_building_trans) # 檢查 lgbm_building_trans 的值
    print(input$lgbm_manage_org) # 檢查 lgbm_manage_org 的值
    print(input$lgbm_manager) # 檢查 lgbm_manager 的值
    print(input$lgbm_furniture) # 檢查 lgbm_furniture 的值
    print(input$lgbm_ac) # 檢查 lgbm_ac 的值
    print(input$lgbm_heater) # 檢查 lgbm_heater 的值
    print(input$lgbm_washer) # 檢查 lgbm_washer 的值
    print(input$lgbm_tv) # 檢查 lgbm_tv 的值
    print(input$lgbm_fridge) # 檢查 lgbm_fridge 的值
    print(input$lgbm_gas) # 檢查 lgbm_gas 的值
    print(input$lgbm_cable) # 檢查 lgbm_cable 的值
    print(input$lgbm_internet) # 檢查 lgbm_internet 的值
    print(input$lgbm_mrt_dist) # 檢查 lgbm_mrt_dist 的值
    print(input$lgbm_nearby_price) # 檢查 lgbm_nearby_price 的值
    print(input$lgbm_rental_date) # 檢查 lgbm_rental_date 的值
    print(input$lgbm_rental_days) # 檢查 lgbm_rental_days 的值

    if (input$model_choice == "XGBoost") {
      # 組裝 XGBoost input data.frame
      new_data <- data.frame(
        鄉鎮市區 = input$xgb_region,
        出租型態 = input$xgb_rental_type,
        建物型態 = input$xgb_building_type,
        有無電梯 = input$xgb_elevator,
        租賃住宅服務 = input$xgb_rental_service,
        總樓層數 = input$xgb_total_floor,
        建物現況格局.房 = input$xgb_rooms,
        建物現況格局.廳 = input$xgb_halls,
        建物現況格局.衛 = input$xgb_baths,
        建物總面積平方公尺 = input$xgb_area,
        屋齡 = input$xgb_age,
        附屬設備.冷氣 = input$xgb_ac,
        附屬設備.熱水器 = input$xgb_heater,
        附屬設備.洗衣機 = input$xgb_washer,
        附屬設備.電視機 = input$xgb_tv,
        附屬設備.瓦斯或天然氣 = input$xgb_gas,
        附屬設備.網路 = input$xgb_internet,
        捷運站距離.公尺. = input$xgb_mrt_dist,
        捷運線 = input$xgb_mrt_line,
        附近建物單位成交均價 = input$xgb_nearby_price,
        租賃年月日 = as.character(input$xgb_rental_date),
        租賃天數 = input$xgb_rental_days,
        stringsAsFactors = FALSE
      )
      # 呼叫 XGBoost 模型
      pred <- predict_rent_xgb(new_data)
      round(as.numeric(pred[1])) # 確保回傳單一數值
    } else if (input$model_choice == "LGBM") {
       if (is.null(factor_levels_lgbm)) {
         warning("LGBM factor levels 未載入，無法進行預測。")
         return(NA) # 載入失敗則回傳 NA
       }
      # 組裝 LGBM input data.frame
      lgbm_input_data <- data.frame(
        "鄉鎮市區" = input$lgbm_region,
        "租賃年月日" = as.numeric(as.Date(input$lgbm_rental_date)),
        "出租型態" = input$lgbm_rental_type,
        "租賃層次(四類)" = input$lgbm_floor_type,
        "總樓層數" = as.numeric(input$lgbm_total_floor),
        "建物型態" = input$lgbm_building_type,
        "交易筆棟數-土地" = as.numeric(input$lgbm_land_trans),
        "交易筆棟數-建物" = as.numeric(input$lgbm_building_trans),
        "租賃住宅服務" = input$lgbm_rental_service,
        "租賃天數" = as.numeric(input$lgbm_rental_days),
        "有無管理組織" = as.numeric(input$lgbm_manage_org),
        "有無管理員" = as.numeric(input$lgbm_manager),
        "有無附傢俱" = as.numeric(input$lgbm_furniture),
        "有無電梯" = as.numeric(input$lgbm_elevator),
        "建物現況格局-房" = as.numeric(input$lgbm_rooms),
        "建物現況格局-廳" = as.numeric(input$lgbm_halls),
        "建物現況格局-衛" = as.numeric(input$lgbm_baths),
        "建物現況格局-隔間" = as.numeric(input$lgbm_partition),
        "建物總面積平方公尺" = as.numeric(input$lgbm_area),
        "屋齡" = as.numeric(input$lgbm_age),
        "建材分類" = input$lgbm_material,
        "附屬設備-冷氣" = as.numeric(input$lgbm_ac),
        "附屬設備-熱水器" = as.numeric(input$lgbm_heater),
        "附屬設備-洗衣機" = as.numeric(input$lgbm_washer),
        "附屬設備-電視機" = as.numeric(input$lgbm_tv),
        "附屬設備-冰箱" = as.numeric(input$lgbm_fridge),
        "附屬設備-瓦斯或天然氣" = as.numeric(input$lgbm_gas),
        "附屬設備-有線電視" = as.numeric(input$lgbm_cable),
        "附屬設備-網路" = as.numeric(input$lgbm_internet),
        "捷運站距離(公尺)" = as.numeric(input$lgbm_mrt_dist),
        "附近建物單位成交均價" = as.numeric(input$lgbm_nearby_price),
        "捷運線" = input$lgbm_mrt_line,
        stringsAsFactors = FALSE,
        check.names = FALSE
      )

      # 呼叫 LGBM 模型
      print(lgbm_input_data) # 檢查 lgbm_input_data 的內容
      pred <- predict_rent_lgbm(lgbm_input_data, 
                               model_path = "model_lgbm/final_lgbm_model.txt",
                               feature_order_path = "model_lgbm/lgbm_feature_order.txt",
                               factor_level_path = "model_lgbm/factor_levels.rds")
      round(as.numeric(pred[1])) # 確保回傳單一數值
    }
  })

  # 顯示預測月租金
  output$pred_rent <- renderText({
    req(pred_data())
    if (is.na(pred_data())) {
      return("⚠️ 無法預測，請檢查輸入資料")
    }
    paste0("NT$ ", formatC(pred_data(), big.mark = ",", format = "f", digits = 0))
  })

  # Reactive: 計算並顯示 LGBM 特徵重要性
  observeEvent(input$run_model, {
    # 確保使用模組的 namespace
    ns <- session$ns
    
    # 取得使用者輸入的前幾大特徵數量，並做基本驗證
    num_features <- req(input$num_top_features)
    if (!is.numeric(num_features) || num_features < 1 || num_features %% 1 != 0) {
      return(tags$p("⚠️ 請輸入有效的正整數來指定顯示前幾大特徵的數量。"))
    }
    
    # 根據選擇的模型動態顯示特徵重要性
    output$importance_panel <- renderUI({
      if (input$model_choice == "LGBM") {
        # ====== LGBM 特徵重要性計算與顯示 ======
        
        # 載入模型檔案
        lgbm_model <- tryCatch({
           # 使用 lgb.load 從檔案載入模型
           lightgbm::lgb.load("model_lgbm/final_lgbm_model.txt")
        }, error = function(e) {
           warning("無法載入 LGBM 模型檔案以計算特徵重要性: ", e$message)
           return(tags$p("⚠️ 無法載入 LGBM 模型，特徵重要性無法顯示。"))
        })
        
        if (is.null(lgbm_model)) {
           return(tags$p("⚠️ 無法載入 LGBM 模型，特徵重要性無法顯示。"))
        }
        
        # 計算特徵重要性 (使用 Gain)
        importance_data <- lightgbm::lgb.importance(lgbm_model, percentage = TRUE)
        
        if (is.null(importance_data) || nrow(importance_data) == 0) {
           return(tags$p("⚠️ 無法取得 LGBM 特徵重要性資料。"))
        }
        
        # 選取使用者指定數量的特徵
        # 確保不超過實際特徵數量
        num_to_show <- min(num_features, nrow(importance_data))
        top_importance <- head(importance_data, num_to_show)
        
        # 將特徵重要性資料轉換為適合顯示的格式
        # 只顯示 Feature 和 Gain
        display_data <- top_importance[, c("Feature", "Gain")]
        colnames(display_data) <- c("特徵", "重要性 (Gain)") # 更改欄位名稱
        
        # 創建水平條狀圖
        p <- ggplot(display_data, aes(x = reorder(特徵, `重要性 (Gain)`), y = `重要性 (Gain)`)) +
          geom_bar(stat = "identity", fill = "#7cbd9c", alpha = 0.8) +
          coord_flip() +
          theme_minimal() +
          theme(
            panel.grid.major.y = element_blank(),
            panel.grid.minor = element_blank(),
            axis.text.y = element_text(size = 12, color = "#333333"),
            axis.text.x = element_text(size = 10, color = "#666666"),
            axis.title = element_text(size = 12, color = "#333333"),
            plot.title = element_text(size = 14, color = "#333333", hjust = 0.5),
            plot.background = element_rect(fill = "#ffffff", color = NA),
            panel.background = element_rect(fill = "#ffffff", color = NA)
          ) +
          labs(
            title = paste0("LGBM 影響前 ", num_to_show, " 大特徵"),
            x = "",
            y = "重要性 (Gain)"
          ) +
          scale_y_continuous(labels = scales::percent_format(accuracy = 0.1))
        
        tagList(
           plotOutput(ns("importance_plot"), height = "800px")
        )
        
        # 渲染圖表
        output$importance_plot <- renderPlot({
          p  
        })
        
      } else if (input$model_choice == "XGBoost") {
        # ====== XGBoost 特徵重要性計算與顯示 ======
        
        # 載入模型檔案
        xgb_model <- tryCatch({
           # 使用 xgb.load 從檔案載入模型
           xgboost::xgb.load("model_xgb/final_xgb_model.txt")
        }, error = function(e) {
           warning("無法載入 XGBoost 模型檔案以計算特徵重要性: ", e$message)
           return(tags$p("⚠️ 無法載入 XGBoost 模型，特徵重要性無法顯示。"))
        })
        
        if (is.null(xgb_model)) {
           return(tags$p("⚠️ 無法載入 XGBoost 模型，特徵重要性無法顯示。"))
        }
        
        # 計算特徵重要性 (使用 Gain)
        # 注意: xgb.importance 預設使用 Gain
        importance_data <- xgboost::xgb.importance(model = xgb_model)
        print(importance_data)
        if (is.null(importance_data) || nrow(importance_data) == 0) {
           return(tags$p("⚠️ 無法取得 XGBoost 特徵重要性資料。"))
        }
        
        # 選取使用者指定數量的特徵
        # 確保不超過實際特徵數量
        num_to_show <- min(num_features, nrow(importance_data))
        top_importance <- head(importance_data, num_to_show)
        
        # 將特徵重要性資料轉換為適合顯示的格式
        # 只顯示 Feature 和 Gain
        display_data <- top_importance[, c("Feature", "Gain")]
        colnames(display_data) <- c("特徵", "重要性 (Gain)") # 更改欄位名稱
        
        # 創建水平條狀圖
        p <- ggplot(display_data, aes(x = reorder(特徵, `重要性 (Gain)`), y = `重要性 (Gain)`)) +
          geom_bar(stat = "identity", fill = "#e8b98b", alpha = 0.8) +
          coord_flip() +
          theme_minimal() +
          theme(
            panel.grid.major.y = element_blank(),
            panel.grid.minor = element_blank(),
            axis.text.y = element_text(size = 12, color = "#333333"),
            axis.text.x = element_text(size = 10, color = "#666666"),
            axis.title = element_text(size = 12, color = "#333333"),
            plot.title = element_text(size = 14, color = "#333333", hjust = 0.5),
            plot.background = element_rect(fill = "#ffffff", color = NA),
            panel.background = element_rect(fill = "#ffffff", color = NA)
          ) +
          labs(
            title = paste0("XGBoost 影響前 ", num_to_show, " 大特徵"),
            x = "",
            y = "重要性 (Gain)"
          ) +
          scale_y_continuous(labels = scales::percent_format(accuracy = 0.1))
        
        tagList(
           plotOutput(ns("importance_plot"), height = "800px")
        )
        
        # 渲染圖表
        output$importance_plot <- renderPlot({
          p
        })
      }
    })
  })
}
