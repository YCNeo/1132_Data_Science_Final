library(bslib) 
library(fontawesome)

## modules/intro.R
# Intro Page module: UI and Server definitions
introPageUI <- function(id){
  ns <- NS(id)
  tagList(
    fluidPage(
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
            tags$h3("網站介紹", class = "text-primary custom-title", style = "margin: 0;")
        ),

       # 2. 概述
      fluidRow(
        column(12,
           h3("概述", class = "text-primary fw-bold mb-4"),
          card(
            class = "card border-0 shadow-sm",
            card_body(
              class = "p-4",
              h5("租金飆到讓人快乾癟？用我們的「捷運 × 租金」神預測，幫可憐的年輕人找到 CP 值最高的小窩！",
                 class = "text-primary fw-bold mb-2"),
              tags$ul(
                class = "list-unstyled",
                tags$li(class = "mb-3 d-flex align-items-center ext-muted",
                       span(fa(name = "magnifying-glass", fill = "#c9cade", height = "1.2em"), class = "me-2"), 
                       "結合捷運站距離因素進行精準預測"),
                tags$li(class = "mb-3 d-flex align-items-center ext-muted",
                       span(fa(name = "magnifying-glass", fill = "#c9cade", height = "1.2em"), class = "me-2"), 
                       "多變量分析考慮房屋面積、樓層、租賃相關資料、租賃時間等因素"),
                tags$li(class = "mb-3 d-flex align-items-cente ext-mutedr",
                       span(fa(name = "magnifying-glass", fill = "#c9cade", height = "1.2em"), class = "me-2"), 
                       "使用 R 語言實現搭配統計分析和機器學習 預測租金")
              )
            )
          )
        )
      ),
      # 3. 研究方法
      fluidRow(
        column(12,
          h3("研究方法", class = "text-primary fw-bold mb-3"),
          card(
            class = "card border-0 shadow-sm",
            card_body(
              class = "p-4",
              h5("研究方法包括: 資料收集、清洗與預處理、模型建立與訓練。",
                class = "text-primary fw-bold mb-2"),
              tags$ul(
                class = "list-unstyled",
                # 方法 1
                tags$li(class = "mb-3 d-flex align-items-center",
                       span(fa(name = "arrow-right-from-bracket", fill = "#c9cade", height = "1.2em"), class = "me-2"),
                       strong("資料收集", class = "text-primary")),
                tags$li(class = "mb-2 ms-4 text-success",
                       "1. 租金與方屋資料: "),
                tags$li(class = "mb-2 ms-4 text-muted",
                       "內政部不動產交易網站擷取臺北市房源資料，跨期14年。"),
                tags$li(class = "mb-2 ms-4 text-success",
                       "2. 捷運資料: "),
                tags$li(class = "mb-2 ms-4 text-muted",
                       "使用ArcGIS 地理編碼服務 input 為地址資訊，output為經度和緯度，
                        蒐集個捷運站的各出入口站點的經緯度資訊（台北市資料大平台)，將各捷運站出入口實際與房屋位置比較，計算與房屋最近的捷運站出入口站點距離、該站所屬路線。
                      "),
                tags$li(class = "mb-2 ms-4 text-success",
                       "3. 附近建案成交均價資料："),
                tags$li(class = "mb-2 ms-4 text-muted",
                         "考量到房價會因「地段」而顯著變化，條件相同的房屋在台北101旁邊和在關渡郊區會有明顯的差異，
                       因此新增「附近建物單價平均價格」欄位，用以量化地段價值。
                       蒐集該筆交易周邊300公尺範圍內所有建物的「單價（元∕平方公尺）」，
                       對這些單價取平均。"),
                # 方法 2
                tags$li(class = "mb-2 d-flex align-items-center",
                       span(fa(name = "arrow-rotate-right", fill = "#c9cade", height = "1.2em"), class = "me-2"),
                       strong("資料清洗與預處理", class = "text-primary")),
                tags$li(class = "mb-2 ms-4 text-muted",
                       "處理缺失值與異常值，篩選適合欄位與年份，最後資料跨期5年(2021-2025)，約12,000筆資料。"),
                # 方法 3
                tags$li(class = "mb-2 d-flex align-items-center",
                       span(fa(name = "arrow-trend-up", fill = "#c9cade", height = "1.2em"), class = "me-2"),
                       strong("建立與訓練模型", class = "text-primary")),
                tags$li(class = "mb-2 ms-4 text-muted",
                       "1. 使用 R 建立預測模型（XGBoost、LightGBM 等）。"),
                tags$li(class = "mb-2 ms-4 text-muted",
                       "2. 使用 Backward Selection， 逐步移除貢獻低點特徵，並評估模型的RMSE變化，以確認模型預測效能的提升。"),
                tags$li(class = "mb-2 ms-4 text-muted",
                       "3. 特徵選擇 (Feature Selection)時，計算每個變數的 p 值，評估模型是否優化之統計顯著性。")
              )
            )
          )
        )
      ),

      # 新增模型成效區塊
      h3("模型成效評估", class = "text-primary fw-bold mb-4"), # 將標題移到 fluidRow 外面
      fluidRow(
        # 第一張卡片：指標數字
        column(6, # 調整欄寬，例如各佔一半
          card(
            class = "mb-4 shadow-sm",
            card_header("評估指標數值"), # 新增卡片標題
            card_body(
              # 模型成效表格
              tableOutput(ns("model_performance_table"))
            ),
            card_footer("註：指標RMSE/MAPE/MEAPE越低 代表效能越好。") # 註解放在表格卡片下方
          )
        ),
        # 第二張卡片：概述與模型介紹
        column(6, # 調整欄寬
          card(
            class = "mb-4 shadow-sm",
            card_header("模型概述與簡介"), # 新增卡片標題
            card_body(
              # 新增模型成效概述文字
              tags$h6("概述", class = "text-primary fw-bold mt-0"),
              p(class = "text-muted", "無論採用哪種評估指標，LightGBM 與 XGBoost 相較於 null model 都有顯著提升，平均預測誤差降低近 10,000 元（約 20%）。
              其中，LightGBM 在 RMSE 上表現最佳，意味著它最擅長控制整體平方誤差；XGBoost 則在 MAPE 與 MEAPE 上稍微領先。總體而言，若重視絕對誤差的最小化，可優先選用 LightGBM；
              若更在意相對誤差或極端值的準確性，則 XGBoost 是較佳選擇。", class = "mb-1"),
              hr(class = "mb-3"), # 分隔線
              # 模型簡介
              tags$h6("模型簡介", class = "text-primary fw-bold mt-0"),
              tags$ul(
                tags$li(strong("Null Model:"), " 基準模型，預測值為該筆建物方圓300公尺內所有建物之平均租金。"),
                tags$li(strong("XGBoost:"), " 一種強大的梯度提升樹模型，效能卓越且應用廣泛。"),
                tags$li(strong("LightGBM:"), " 另一種高效能的梯度提升樹模型，以處理大規模數據和快速訓練聞名。")
              )
            ) # Closes card_body
          ) # Closes card
        ) # Closes column
      ), # Closes fluidRow

      # 原有的台北市租金概覽區塊
      fluidRow(
        h3("台北市租金概覽", class = "text-primary mb-3"),
        column(6, 
          card(
            class = "mb-4 shadow-sm",
            card_body(
              img(src = "台北市每平方公尺平均租金.png", alt = "台北市每平方公尺平均租金", class = "img-fluid", style = "width: 100%; height: auto;")
            ),
            card_footer("台北市每平方公尺平均租金")
          )
        ),
        column(6, 
          card(
            class = "mb-4 shadow-sm",
            card_body(
              img(src = "區域平均租金.png", alt = "區域平均租金", class = "img-fluid", style = "width: 100%; height: auto;")
            ),
            card_footer("區域平均租金")
          )
        )
      )
    )
  )
}



# Server function for Intro page
introPageServer <- function(input, output, session) {
  
  # 渲染模型成效表格
  output$model_performance_table <- renderTable({
    data.frame(
      評估指標 = c("RMSE", "MAPE", "MEAPE"),
      `Null Model` = c(19006, 0.3572, 0.2405),
      XGBoost = c(11970, 0.1530, 0.1140),
      LightGBM = c(9172, 0.1538, 0.1191),
      check.names = FALSE # 保留 Null Model 的空格
    )
  }, striped = TRUE, hover = TRUE, bordered = TRUE, spacing = "l")

  # ---------------------------------------
  # 5. Back to Welcome：返回首頁
  # ---------------------------------------
  observeEvent(input$back_welcome, {
    updateTabsetPanel(session, "tabs", selected = "welcome")
  })
  
}
