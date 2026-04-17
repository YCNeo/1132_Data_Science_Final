library(shiny)
library(ggplot2)

data_path <- file.path("dataset", "rent_trades_w_mrt.csv")
rent_raw <- read.csv(
  data_path,
  fileEncoding = "UTF-8",
  stringsAsFactors = FALSE
)

numeric_cols <- c("總額元", "單價元平方公尺", "捷運站距離.公尺.", "建物總面積平方公尺")
for (col in numeric_cols) {
  rent_raw[[col]] <- suppressWarnings(as.numeric(rent_raw[[col]]))
}

district_choices <- sort(unique(stats::na.omit(rent_raw$鄉鎮市區)))
mrt_choices <- sort(unique(stats::na.omit(rent_raw$最近捷運站)))

ui <- fluidPage(
  titlePanel("台北租屋資料分析儀表板"),
  sidebarLayout(
    sidebarPanel(
      selectInput(
        "district",
        "行政區",
        choices = c("全部" = "ALL", district_choices),
        selected = "ALL"
      ),
      selectInput(
        "mrt",
        "最近捷運站",
        choices = c("全部" = "ALL", mrt_choices),
        selected = "ALL"
      ),
      sliderInput(
        "rent_range",
        "租金總額範圍",
        min = floor(min(rent_raw$總額元, na.rm = TRUE)),
        max = ceiling(max(rent_raw$總額元, na.rm = TRUE)),
        value = c(
          floor(min(rent_raw$總額元, na.rm = TRUE)),
          ceiling(max(rent_raw$總額元, na.rm = TRUE))
        ),
        step = 1000,
        pre = "NT$"
      ),
      sliderInput(
        "distance_range",
        "捷運距離範圍（公尺）",
        min = 0,
        max = ceiling(max(rent_raw$捷運站距離.公尺., na.rm = TRUE)),
        value = c(0, min(2000, ceiling(max(rent_raw$捷運站距離.公尺., na.rm = TRUE)))),
        step = 50
      ),
      checkboxInput("furnished_only", "只看有附家具", FALSE),
      checkboxInput("elevator_only", "只看有電梯", FALSE)
    ),
    mainPanel(
      fluidRow(
        column(4, wellPanel(h4("樣本數"), textOutput("row_count"))),
        column(4, wellPanel(h4("平均租金"), textOutput("avg_rent"))),
        column(4, wellPanel(h4("平均捷運距離"), textOutput("avg_distance")))
      ),
      tabsetPanel(
        tabPanel(
          "散點圖",
          plotOutput("scatter_plot", height = 420),
          verbatimTextOutput("corr_text")
        ),
        tabPanel(
          "分布圖",
          plotOutput("hist_plot", height = 420)
        ),
        tabPanel(
          "資料表",
          tableOutput("preview_table")
        )
      )
    )
  )
)

server <- function(input, output, session) {
  filtered_data <- reactive({
    df <- rent_raw

    if (input$district != "ALL") {
      df <- df[df$鄉鎮市區 == input$district, ]
    }

    if (input$mrt != "ALL") {
      df <- df[df$最近捷運站 == input$mrt, ]
    }

    df <- df[
      !is.na(df$總額元) &
        df$總額元 >= input$rent_range[1] &
        df$總額元 <= input$rent_range[2],
    ]

    df <- df[
      !is.na(df$捷運站距離.公尺.) &
        df$捷運站距離.公尺. >= input$distance_range[1] &
        df$捷運站距離.公尺. <= input$distance_range[2],
    ]

    if (isTRUE(input$furnished_only) && "有無附傢俱" %in% names(df)) {
      df <- df[df$有無附傢俱 == "有", ]
    }

    if (isTRUE(input$elevator_only) && "有無電梯" %in% names(df)) {
      df <- df[df$有無電梯 == "有", ]
    }

    df
  })

  output$row_count <- renderText({
    format(nrow(filtered_data()), big.mark = ",")
  })

  output$avg_rent <- renderText({
    df <- filtered_data()
    if (!nrow(df)) return("無資料")
    paste0("NT$ ", format(round(mean(df$總額元, na.rm = TRUE)), big.mark = ","))
  })

  output$avg_distance <- renderText({
    df <- filtered_data()
    if (!nrow(df)) return("無資料")
    paste0(round(mean(df$捷運站距離.公尺., na.rm = TRUE), 1), " m")
  })

  output$scatter_plot <- renderPlot({
    df <- filtered_data()
    validate(need(nrow(df) > 1, "目前篩選條件下沒有足夠資料可繪圖。"))

    ggplot(df, aes(x = 捷運站距離.公尺., y = 總額元)) +
      geom_point(alpha = 0.45, color = "#1f5aa6") +
      geom_smooth(method = "lm", se = TRUE, color = "#c0392b") +
      labs(
        title = "租金與捷運距離散點圖",
        x = "捷運距離（公尺）",
        y = "總額元"
      ) +
      theme_minimal(base_size = 13)
  })

  output$corr_text <- renderText({
    df <- filtered_data()
    valid <- df[!is.na(df$總額元) & !is.na(df$捷運站距離.公尺.), ]
    if (nrow(valid) < 2) return("相關係數：資料不足")
    corr <- cor(valid$總額元, valid$捷運站距離.公尺.)
    paste0("租金總額與捷運距離的 Pearson 相關係數：", round(corr, 3))
  })

  output$hist_plot <- renderPlot({
    df <- filtered_data()
    validate(need(nrow(df) > 0, "目前篩選條件下沒有資料可繪圖。"))

    ggplot(df, aes(x = 總額元)) +
      geom_histogram(bins = 30, fill = "#2e8b57", color = "white", alpha = 0.9) +
      labs(
        title = "租金分布",
        x = "總額元",
        y = "筆數"
      ) +
      theme_minimal(base_size = 13)
  })

  output$preview_table <- renderTable({
    df <- filtered_data()
    if (!nrow(df)) return(data.frame(訊息 = "目前沒有符合條件的資料"))

    keep_cols <- c(
      "鄉鎮市區",
      "土地位置建物門牌",
      "總額元",
      "單價元平方公尺",
      "最近捷運站",
      "捷運站距離.公尺.",
      "建物總面積平方公尺",
      "建物型態"
    )
    keep_cols <- keep_cols[keep_cols %in% names(df)]
    utils::head(df[, keep_cols, drop = FALSE], 20)
  }, striped = TRUE, bordered = TRUE, spacing = "xs")
}

shinyApp(ui = ui, server = server)
