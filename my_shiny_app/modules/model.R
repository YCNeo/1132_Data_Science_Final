## modules/model.R
# Model Page module: UI and Server definitions

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
            tags$h1("租金預測", class = "custom-title", style = "margin: 0;")
        ),

      # 2. 預測模型概述卡片
      fluidRow(
        column(12,
          card(
            class = "mb-4 shadow-sm",
            card_header("預測模型概述"),
            card_body(
              p("本專案使用R語言開發了多種統計和機器學習模型，以預測台北市的租房價格。我們特別關注捷運站距離對租金的影響，並將其作為關鍵預測變量。"),
              tags$div(class = "d-flex flex-wrap",
                tags$div(style = "flex:1; min-width:200px;",
                  checkboxInput(ns("model_lm"),  "線性迴歸模型", value = TRUE),
                  checkboxInput(ns("model_rf"),  "隨機森林",     value = TRUE)
                ),
                tags$div(style = "flex:1; min-width:200px;",
                  checkboxInput(ns("model_xgb"), "XGBoost",      value = TRUE),
                  checkboxInput(ns("model_nn"),  "神經網絡",     value = TRUE)
                )
              )
            )
          )
        )
      ),

      # 3. 預測工具卡片 (左右兩欄)
      fluidRow(
        # 左側：輸入
        column(4,
          card(
            class = "mb-4 shadow-sm",
            card_header("輸入房屋資訊"),
            card_body(
              selectInput(ns("region"),    "行政區", choices = c("大安區","信義區","中正區","萬華區"), width = "100%"),
              numericInput(ns("area"),       "建物面積 (坪)", value = 25, min = 5, width = "100%"),
              fluidRow(
                column(4, numericInput(ns("rooms"), "房", value = 2, min = 0)),
                column(4, numericInput(ns("halls"), "廳", value = 1, min = 0)),
                column(4, numericInput(ns("baths"), "衛", value = 1, min = 0))
              ),
              fluidRow(
                column(6, numericInput(ns("floor"),       "所在樓層", value = 3,  min = 1)),
                column(6, numericInput(ns("total_floor"), "總樓層",   value = 12, min = 1))
              ),
              numericInput(ns("dist"),       "距離捷運站 (公尺)", value = 500, min = 0, width = "100%"),
              checkboxGroupInput(ns("extras"), "附加特徵",
                                 choices = c("附傢俱","有管理組織","含車位")),
              actionButton(ns("run_model"), "預測租金", class = "btn btn-danger btn-lg w-100")
            )
          )
        ),

        # 右側：結果
        column(8,
          card(
            class = "mb-4 shadow-sm",
            card_header("預測結果"),
            card_body(
              # 預測月租金
              tags$div(style = "background:#f8f9fa; padding:20px; border-radius:8px; text-align:center; margin-bottom:20px;",
                tags$h5("預測月租金"),
                tags$h2(textOutput(ns("pred_rent")), style = "color:#d9534f;"),
                tags$p(textOutput(ns("pred_interval")))
              ),
              # 影響因素重要性
              tags$h5("影響因素重要性"),
              uiOutput(ns("feature_importance")),
              # 類似物件參考
              tags$h5("類似物件參考"),
              tableOutput(ns("example_table"))
            )
          )
        )
      )
    )
  )
}

# Server function for Model page
modelPageServer <- function(input, output, session) {
  # Reactive: 當按下預測按鈕執行模型
  pred_data <- eventReactive(input$run_model, {
    # TODO: replace with actual model prediction
    list(
      rent     = 25800,
      interval = c(24200, 27400),
      fi = data.frame(
        feature    = c("捷運距離","建物面積","行政區","房間格局","其他因素"),
        importance = c(0.32, 0.28, 0.20, 0.12, 0.08)
      ),
      examples = data.frame(
        區域   = c("大安區","大安區","大安區"),
        格局   = c("2房1廳","2房1廳","3房1廳"),
        坪數   = c(25,22,28),
        租金   = c(26500,24000,29800)
      )
    )
  })

  # Render predicted rent
  output$pred_rent <- renderText({
    req(pred_data())
    paste0("NT$ ", formatC(pred_data()$rent, big.mark=","))
  })
  output$pred_interval <- renderText({
    req(pred_data())
    paste0("預測區間：NT$ ", formatC(pred_data()$interval[1], big.mark=","),
           " - ", formatC(pred_data()$interval[2], big.mark=","))
  })

  # Render feature importance bars
  output$feature_importance <- renderUI({
    df <- pred_data()$fi
    # 動態產生進度條
    lapply(seq_len(nrow(df)), function(i) {
      val <- df$importance[i] * 100
      tagList(
        tags$div(df$feature[i], style="margin-top:10px;"),
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

  # Render example table
  output$example_table <- renderTable({
    pred_data()$examples
  }, striped = TRUE, hover = TRUE)

  # Back to Welcome
  observeEvent(input$back_welcome, {
    updateTabsetPanel(session, "tabs", selected = "welcome")
  })
}
