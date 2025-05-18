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
            tags$h6("資料介紹", class = "custom-title", style = "margin: 0;")
        ),
      
      # 2. 數據概述卡片
      fluidRow(
        column(12,
          h3("數據概述"),
          card(
            class = "mb-4 shadow-sm",
            card_body(
              p("本研究使用了2020-2023年間台北市的租房數據，包含超過5,000筆房源記錄。主要數據特徵包括："),
              tags$div(
                style = "display:flex; flex-wrap:wrap;",
                tags$ul(style="flex:1; list-style-type:disc; padding-left:20px;",
                  tags$li("租金價格（元/月）"),
                  tags$li("房屋面積（坪）"),
                  tags$li("房型（幾房幾廳幾衛）")
                ),
                tags$ul(style="flex:1; list-style-type:disc; padding-left:20px;",
                  tags$li("樓層"),
                  tags$li("行政區"),
                  tags$li("到最近捷運站距離（公尺）")
                )
              )
            )
          )
        )
      ),
      
      # 3. 資料欄位說明表格
      h3("資料欄位說明"),
      fluidRow(
        column(12,
          card(
            class = "mb-4 shadow-sm",
            card_body(
              # 用 renderTable 或直接寫靜態表格
              tableOutput(ns("fields_table"))
            )
          )
        )
      ),
      
      # 4. 捷運距離特徵卡片
      h3("捷運距離特徵"),
      fluidRow(
        column(8,
          card(
            class = "mb-4 shadow-sm",
            card_body(
              p("除了上述原始資料欄位外，我們額外計算了每個房源到最近捷運站的距離，作為重要的預測特徵。此特徵透過地理編碼和空間分析獲得。"),
              tags$img(src = "mrt_distance_diagram.png", width = "100%", alt = "捷運距離示意圖")
            )
          )
        ),
        column(4,
          card(
            class = "mb-4 shadow-sm",
            card_body(
              tags$h5("捷運距離分類"),
              tags$ul(
                tags$li(tags$span(style="color:green; font-weight:bold;","●"), " 近距離：0-300 公尺"),
                tags$li(tags$span(style="color:blue; font-weight:bold;","●"), " 適中距離：300-600 公尺"),
                tags$li(tags$span(style="color:orange; font-weight:bold;","●"), " 稍遠距離：600-1000 公尺"),
                tags$li(tags$span(style="color:red; font-weight:bold;","●"), " 遠距離：1000 公尺以上")
              )
            )
          )
        )
      )
    ) # end fluidPage
  )   # end tagList
}

# Server function for Dataset page
datasetPageServer <- function(input, output, session) {
  # 3. 資料欄位說明表格
  output$fields_table <- renderTable({
    # TODO: 建立欄位說明資料框
    data.frame(
      欄位名稱 = c("鄉鎮市區","交易標的","土地位置建物門牌","土地面積平方公尺","都市土地使用分區",
                  "租賃年月日","移轉層次","總樓層數","建物型態","主要用途","主要材料",
                  "建物完成年月日","建物面積平方公尺","建物現況格局-房","建物現況格局-廳",
                  "建物現況格局-衛/隔間","有無管理組織","有無附傢俱","總額元","單價元平方公尺",
                  "車位類別","車位面積平方公尺","車位總額元"),
      說明 = c("台北市行政區，如信義區、大安區等","租賃物件類型，如房地、土地、車位等",
               "租賃物件的地址資訊","租賃物件的土地面積","分為住、商、都市，且都市計畫的組別內有更詳細分類",
               "租賃合約簽訂日期","租賃物件所在樓層","建物的總樓層數","如公寓、華廈、住宅大樓等","如住宅、商業用等",
               "建物的主要建築材料，如鋼筋混凝土等","建物的完工日期","租賃物件的室內面積",
               "房間數量","客廳數量","衛浴數量及是否有隔間","是否有社區管理組織",
               "租賃物件是否附傢俱","租金總額（新台幣）","每平方公尺租金單價",
               "如坡道平面、升降平面等","車位的面積","車位的租金金額")
    )
  }, striped = TRUE, hover = TRUE, spacing = "l")

  # 4. Back to Welcome：返回首頁
  observeEvent(input$back_welcome, {
    updateTabsetPanel(session, "tabs", selected = "welcome")
  })
}
