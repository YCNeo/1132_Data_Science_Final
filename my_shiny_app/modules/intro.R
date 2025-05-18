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
            tags$h1("網站介紹", class = "custom-title", style = "margin: 0;")
        ),

       # 2. 概述
      fluidRow(
        column(12,
           h3("概述"),  # Added h4() as per the latest edit
          card(
            class = "bg-my-yellow my-card",
            card_body(
              p("租金飆到讓人快乾癟？用我們的「捷運 × 租金」神預測，幫可憐的年輕人找到 CP 值最高的小窩！"),
              tags$ul(
                class = "list-unstyled",
                tags$li(fa(name = "magnifying-glass", fill = "#08306d", height = "1.2em"), "結合捷運站距離因素進行精準預測"),
                tags$li(fa(name = "magnifying-glass", fill = "#08306d", height = "1.2em"), "多變量分析考慮房屋面積、樓層、社區設施等因素"),
                tags$li(fa(name = "magnifying-glass", fill = "#08306d", height = "1.2em"), "使用 R 語言實現高效的統計分析和機器學習模型")
              )
            )
          )
        )
      ),
      # 3. 研究方法
      fluidRow(
        column(6,
          h3("研究方法"),  # Added h4() as per the latest edit
          card(
            class = "bg-my-yellow my-card",
            #card_header("研究方法"),
            card_body(
              p("我們的研究方法包括資料收集、清洗與預處理、模型建立與訓練。"),
              tags$ul(
              class = "list-unstyled",
              # 方法 1
              tags$li(fa(name = "arrow-right-from-bracket", fill = "#08306d", height = "1.2em"),fa(name = "1", fill = "#08306d", height = "1.2em"), strong("資料收集")),
              tags$li( "從內政部不動產交易網站擷取臺北市房源資料，並結合捷運站資料。"),
              # 方法 2
              tags$li(fa(name = "arrow-rotate-right", fill = "#0d6efd", height = "1.2em"),fa(name = "2", fill = "#0d6efd", height = "1.2em"), strong("資料清洗與預處理")),
              tags$li( "處理缺失值與異常值，並計算每筆房源到最近捷運站的距離。"),
              # 方法 3
              tags$li(fa(name = "arrow-trend-up", fill = "#8bade1", height = "1.2em"),fa(name = "3", fill = "#8bade1", height = "1.2em"), strong("建立與訓練模型")),
              tags$li( "使用 R 實現多種預測模型（如線性回歸、隨機森林、XGBoost 等）。")
            )
          )
        )
       ),
        column(6,
          card(
            class = "bg-my-yellow my-card",
            card_body(
                img(src = "mrt.png", alt = "捷運示意圖", class = "img-fluid"),
            ),
            card_footer("距離示意圖")
          )
          
        )
    ),
        
    fluidRow(
        h3("台北市租金概覽"),
        column(6, card(title="熱力圖", plotOutput(ns("heatmap_plot"), height="250px"))),
        column(6, card(title="區域平均租金", plotOutput(ns("bar_region"), height="250px"))),
        column(6, card(title="租金 vs 距離", plotOutput(ns("scatter_dist"), height="250px"))),
        column(6, card(title="捷運線平均租金", plotOutput(ns("bar_line"), height="250px")))
    )
  )
)

}



# Server function for Intro page
introPageServer <- function(input, output, session) {
  # ---------------------------------------
  # 1. Heatmap：租金熱力圖
  # ---------------------------------------
  output$heatmap_plot <- renderPlot({
    # TODO: 讀取資料 (例如 data/rentals.csv -> rent_df)
    # rent_df <- read.csv("data/rentals.csv")
    # TODO: "df_heatmap" = 處理後的資料框，包含各行政區平均租金
    # df_heatmap <- rent_df %>% group_by(region) %>% summarize(avg_rent = mean(rent))
    # TODO: 用 ggplot2 繪製熱力圖
    # ggplot(df_heatmap, aes(x = longitude, y = latitude, fill = avg_rent)) +
    #   geom_tile() + …
  })

  # ---------------------------------------
  # 2. Bar Region：各行政區平均租金長條圖
  # ---------------------------------------
  output$bar_region <- renderPlot({
    # TODO: 與 heatmap 共用或重新計算 df_region
    # df_region <- df_heatmap
    # TODO: 繪製長條圖
    # ggplot(df_region, aes(x = reorder(region, avg_rent), y = avg_rent)) +
    #   geom_col() + coord_flip() + …
  })

  # ---------------------------------------
  # 3. Scatter Dist：租金 vs. 捷運距離 散點圖
  # ---------------------------------------
  output$scatter_dist <- renderPlot({
    # TODO: 建立 df_scatter，至少包含 rent (y) 和 dist_to_mrt (x)
    # df_scatter <- rent_df %>% mutate(dist_to_mrt = …)
    # TODO: 繪製散點圖並加上回歸線
    # ggplot(df_scatter, aes(x = dist_to_mrt, y = rent)) +
    #   geom_point(alpha=0.5) +
    #   geom_smooth(method="lm")
  })

  # ---------------------------------------
  # 4. Bar Line：各捷運線平均租金長條圖
  # ---------------------------------------
  output$bar_line <- renderPlot({
    # TODO: 建立 df_line，包含 mrt_line (捷運線) 及 avg_rent
    # df_line <- rent_df %>%
    #   left_join(mrt_stations, by="station_id") %>%
    #   group_by(mrt_line) %>%
    #   summarize(avg_rent = mean(rent))
    # TODO: 繪製長條圖
    # ggplot(df_line, aes(x = mrt_line, y = avg_rent, fill = mrt_line)) +
    #   geom_col(show.legend = FALSE) + …
  })

  # ---------------------------------------
  # 5. Back to Welcome：返回首頁
  # ---------------------------------------
  observeEvent(input$back_welcome, {
    updateTabsetPanel(session, "tabs", selected = "welcome")
  })
  
}
