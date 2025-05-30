## modules/welcome.R
# Welcome Page module: UI and Server definitions

# UI function for Welcome page
welcomePageUI <- function(id) {
  ns <- NS(id)
  tagList(
    fluidPage(
      # 這裡引用 www/custom.css
      tags$head(
        tags$link(rel = "stylesheet", href = "custom.css")
      ),
      div(style = "text-align: center; margin-top: 50px;", # 調整整體上方間距
          h1("捷運距離 X 台北市租金預測 ", class = "text-secondary mb-4"), # 增加標題下方間距
          hr(),
          h4("使用 R 語言進行台北市租房價格預測!", class = "text-secondary mb-2"), # 原本的介紹文字
          h5("結合捷運距離因素，讓你能夠更精準地預測租金!", class = "text-secondary mb-3"), # 原本的介紹文字
          tags$img(src = "wel.png", style = "display: block; margin-left: auto; margin-right: auto; width: 500px; height: auto; margin-bottom: 15px; border-radius: 10px;"), # 替換為圖片，固定寬度並自動調整高度， 可以改圓角嗎
          hr(),
          h6("🔽 點擊下方以獲取更多資訊 ", class = "text-muted mb-4"), # 原本的介紹文字

          fluidRow(
            class = "justify-content-center", # 居中卡片列，減少水平間距
            column(4, # 第一個卡片 (網站介紹)
              actionButton(
                ns("go_intro"),
                label = card(
                  class = "card text-center h-100 shadow-sm", # 卡片樣式，文字置中，等高卡片
                  card_body(
                    tags$img(src = "web-analysis.png", alt = "網站介紹圖片", style = "display: block; margin-left: auto; margin-right: auto; width: 80px; height: auto; margin-bottom: 15px;"), # 替換為圖片，固定寬度並自動調整高度，並置中
                    h5("網站介紹", class = "card-title"), # 標題
                    p("如何結合捷運距離預測台北市租金", class = "card-text") # 描述
                  )
                ),
                icon = NULL,
                class = "btn btn-light shadow-sm" # 使用標準 Bootstrap 按鈕類別
              )
            ),
            column(4, # 第二個卡片 (資料介紹)
              actionButton(
                ns("go_dataset"),
                label = card(
                  class = "card text-center h-100 shadow-sm", # 卡片樣式，文字置中，等高卡片
                  card_body(
                    tags$img(src = "analysis.png", alt = "資料介紹圖片", style = "display: block; margin-left: auto; margin-right: auto; width: 80px; height: auto; margin-bottom: 15px;"), # 替換為圖片，固定寬度並自動調整高度，並置中
                    h5("資料介紹", class = "card-title"), # 標題
                    p("探索台北市租房數據與熱力圖分析", class = "card-text") # 描述
                  )
                ),
                icon = NULL,
                class = "btn btn-light shadow-sm" # 使用標準 Bootstrap 按鈕類別
              )
            ),
            column(4, # 第三個卡片 (房價預測)
              actionButton(
                ns("go_model"),
                label = card(
                  class = "card text-center h-100 shadow-sm", # 卡片樣式，文字置中，等高卡片
                  card_body(
                    tags$img(src = "historical.png", alt = "房價預測圖片", style = "display: block; margin-left: auto; margin-right: auto; width: 80px; height: auto; margin-bottom: 15px;"), # 替換為圖片，固定寬度並自動調整高度，並置中
                    h5("租金預測", class = "card-title"), # 標題
                    p("使用 R 語言模型進行精準租金預測", class = "card-text") # 描述
                  )
                ),
                icon = NULL,
                class = "btn btn-light shadow-sm" # 使用標準 Bootstrap 按鈕類別
              )
            )
          ),
          # --- TEST BUTTON --- (Temporary)
          # Check if basic Bootstrap classes are applied
          # REMOVE THIS LATER
          # tagList(
          #   fluidRow(
          #     column(12, 
          #       actionButton(
          #         "test_btn", 
          #         "Test Button", 
          #         class = "btn btn-danger btn-lg"
          #       )
          #     )
          #   )
          # )
      )
    )
  )
}

# Server function for Welcome page
# Listens to navigation buttons and updates the active tab
welcomePageServer <- function(input, output, session) {
  # Navigate to intro page
  observeEvent(input$go_intro, {
    updateTabsetPanel(session, "tabs", selected = "intro")
  })
  # Navigate to dataset page
  observeEvent(input$go_dataset, {
    updateTabsetPanel(session, "tabs", selected = "dataset")
  })
  # Navigate to model page
  observeEvent(input$go_model, {
    updateTabsetPanel(session, "tabs", selected = "model")
  })
}
