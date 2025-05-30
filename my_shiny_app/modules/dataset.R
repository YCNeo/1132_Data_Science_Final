## modules/dataset.R
# Dataset Page module: UI and Server definitions

# UI function for Dataset page
# 包含：返回按鈕、大標題、數據概述卡片、欄位說明表格、捷運距離特徵卡片
datasetPageUI <- function(id) {
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
            tags$h6("資料介紹", class = "text-primary custom-title", style = "margin: 0;")
        ),
      
      # 2. 數據概述卡片
      fluidRow(
        column(12,
          h3("資料來源", class = "text-primary fw-bold mb-4"),
          card(
            class = "mb-4 shadow-sm text-muted",
            card_body(
              h6("透過爬蟲，擷取內政部「不動產交易實價查詢服務網」的資料，跨度14年，取得包括建物標的、建物面積、屋齡、總價格及格局配置等資料共 12,271 筆。經過資料清洗後，符合訓練需求的時間跨度為2021-2025之資料。
              此外，我們認為房屋與捷運站的距離是影響租金的重要因素，因此利用GeoAmplify等API工具額外「加入與最近捷運站的距離」、「附近建物成交單位均價」作為欄位，以提升模型的預測準確性。"),
              tags$div(
                style = "display:flex; flex-wrap:wrap;",
                tags$ul(style="flex:1; list-style-type:disc; padding-left:20px;",
                  tags$li("目標欄位：租金價格（元/月）"),
                  tags$li("內政部資料欄位：房屋面積（坪）、房型、鄉鎮市區...等"),
                ),
                tags$ul(style="flex:1; list-style-type:disc; padding-left:20px;",
                  tags$li("額外新增欄位：到最近捷運站距離（公尺)"),
                  tags$li("額外新增欄位：附近建物成交單位均價"),
                )
              )
            )
          )
        )
      ),
      
      # 3. 資料欄位說明表格
      h3("資料欄位說明", class = "text-primary fw-bold mb-4"),
      fluidRow(
        column(12,
          card(
            class = "mb-4 shadow-sm text-muted",
            card_body(
              # 用 renderTable 或直接寫靜態表格
              tableOutput(ns("fields_table"))
            )
          )
        )
      ),
      
      # 4. 捷運距離特徵
      h3("捷運距離特徵", class = "text-primary fw-bold mb-4"),
      fluidRow( # Starts fluidRow for MRT features
        column(3, # Starts first column
          card( # Starts card
            class = "mb-4 shadow-sm",
            card_body( # Starts card_body
              p(class = "text-muted","除了上述原始資料欄位外，我們額外計算了每個房源到最近捷運站的距離，作為重要的預測特徵。此特徵透過地理編碼和空間分析獲得。"),
              tags$img(src = "mrt_distance_diagram.png", width = "100%", alt = "捷運距離示意圖")
            ) # Closes card_body
          ) # Closes card
        ), # Closes first column
        column(9, # Starts second column
          card( # Starts card
            class = "mb-4 shadow-sm",
            card_body( # Starts card_body
              tags$img(src = "各捷運站每平方公尺平均租金.png", width = "100%", alt = "各捷運站每平方公尺平均租金")
            ) # Closes card_body
          ) # Closes card
        ) # Closes second column
      ), # Closes fluidRow for MRT features
      
       # 5. DATA EDA
      h3("資料探索分析 (EDA)", class = "text-primary fw-bold mb-4"),
      fluidRow( # Starts fluidRow for EDA
        column(12, # Starts first column
          card( # Starts card
            class = "mb-4 shadow-sm",
            card_body( # Starts card_body
              tags$img(src = "建物面積分布.png", width = "100%", alt = "建物面積分布")
            ), # Closes card_body
            card_footer("資料建物面積分布")
          ) # Closes card
        ), # Closes first column
        column(6, # Starts second column
          card( # Starts card
            class = "mb-4 shadow-sm",
            card_body( # Starts card_body
              tags$img(src = "格局-房.png", width = "100%", alt = "格局-房資料分布")
            ), # Closes card_body
            card_footer("單筆資料房間數分布圖")
          ) # Closes card
        ), # Closes second column
        column(6, # Starts third column
          card(
            class = "mb-4 shadow-sm",
            card_body(
              tags$img(src = "格局-廳.png", width = "100%", alt = "格局-廳")
            ) ,# Closes card_body,
            card_footer("單筆資料廳數分布圖")
          ) # Closes card
        ) # Closes third column
      ) # Closes fluidRow for EDA
    ) # closes fluidPage
  )   # closes tagList
}

# Server function for Dataset page
datasetPageServer <- function(input, output, session) {
  # 3. 資料欄位說明表格
  output$fields_table <- renderTable({
    # TODO: 建立欄位說明資料框
    data.frame(
     欄位名稱 = c(
  "建物總面積平方公尺",
  "租賃住宅服務",
  "建物型態",
  "屋齡",
  "附近建物單位成交均價",
  "鄉鎮市區",
  "建物現況格局-房",
  "捷運站距離(公尺)",
  "租賃天數",
  "出租型態",
  "建物現況格局-衛",
  "建物現況格局-廳",
  "附屬設備-熱水器",
  "租賃年月日",
  "總樓層數",
  "附屬設備-網路",
  "附屬設備-冷氣",
  "捷運線",
  "附屬設備-電視機",
  "附屬設備-有線電視",
  "附屬設備-瓦斯或天然氣",
  "附屬設備-洗衣機",
  "有無管理組織",
  "交易筆棟數-建物",
  "有無電梯",
  "附屬設備-冰箱",
  "有無附傢俱",
  "建材分類",
  "有無管理員",
  "交易筆棟數-土地",
  "建物現況格局-隔間",
  "租賃層次"
),
說明 = c(
  "租賃物件的室內面積", # 對應 建物總面積平方公尺
  "（待補充說明）", # 租賃住宅服務
  "如公寓、華廈、住宅大樓等", # 對應 建物型態
  "房屋年齡", # 屋齡 - 需要計算方式或來源說明
  "距離本座標最近的", # 附近建物單位成交均價 - 需要說明如何取得或計算
  "台北市行政區，如信義區、大安區等", # 對應 鄉鎮市區
  "房間數量", # 對應 建物現況格局-房
  "到最近捷運站距離（公尺)", # 對應 捷運站距離(公尺) - 根據 dataset.R 補充
  "合約總天數", # 租賃天數 - 可能是合約總天數？
  "房東或出租人將其房屋或房地產出租給承租人使用的形式。 出租型態可以區分為雅房、套房、獨立套房等等", # 出租型態
  "衛浴數量", # 對應 建物現況格局-衛 (原 dataset.R 為 衛浴數量及是否有隔間)
  "客廳數量", # 對應 建物現況格局-廳
  "有無熱水器", # 附屬設備-熱水器
  "租賃合約簽訂日期", # 對應 租賃年月日
  "建物的總樓層數", # 對應 總樓層數
  "是否有網路設備）", # 附屬設備-網路
  "是否有冷氣（待補充說明）", # 附屬設備-冷氣
  "距離最近的捷運站點捷運線", # 捷運線 - 可能需要說明是哪條捷運線或捷運站名稱
  "是否有電視機", # 附屬設備-電視機
  "是否有有線電視", # 附屬設備-有線電視
  "是否有瓦斯或天然氣", # 附屬設備-瓦斯或天然氣
  "是否有洗衣機", # 附屬設備-洗衣機
  "是否有社區管理組織", # 對應 有無管理組織
  "辦理所有權移轉登記時，實際完成交易的建物", # 交易筆棟數-建物
  "有無電梯", # 有無電梯
  "有無附屬設備-冰箱", # 附屬設備-冰箱
  "租賃物件是否附傢俱", # 對應 有無附傢俱
  "建材分類是指將建築材料按照不同的用途、特性或來源的歸類", # 建材分類
  "有無管理員", # 有無管理員
  "辦理所有權移轉登記時，實際完成交易的土地", # 交易筆棟數-土地
  "建物現況格局-隔間", # 建物現況格局-隔間 (原 dataset.R 為 衛浴數量及是否有隔間，這裡拆分了)
  "在租賃交易中，租賃物件的複雜性或層級，地下室、低樓層、中樓層、高樓層或是透天厝" # 租賃層次(四類)
)
     
     
     )
  }, striped = TRUE, hover = TRUE, spacing = "l")

  # 4. Back to Welcome：返回首頁
  observeEvent(input$back_welcome, {
    updateTabsetPanel(session, "tabs", selected = "welcome")
  })
}




