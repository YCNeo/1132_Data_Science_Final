## modules/welcome.R
# Welcome Page module: UI and Server definitions

# UI function for Welcome page
welcomePageUI <- function(id) {
  ns <- NS(id)
  print(ns)  # Debugging: print the namespace
  tagList(
    # embed custom CSS for styling the welcome card
    tags$head(
      tags$style(HTML(
        ".welcome-card {
            max-width: 1000px;
            margin: 100px auto;
            padding: 20px;
            border-radius: 12px;
            box-shadow: 0 4px 12px rgba(0,0,0,0.1);
            text-align: center;
          }
          .welcome-card img {
            width: 200px;
            margin: 10px;
            cursor: pointer;
            border-radius: 8px;
          }"
      ))
    ),
    fluidPage(
      div(class = "welcome-card",
          h1("歡迎來到台北市租房價格預測網站"),
          h4("這是一個使用 R 語言進行台北市租房價格預測的網站"),
          h4("我們結合了捷運距離因素，讓你能夠更精準地預測租金"),
          h5("請選擇下方的按鈕以了解更多資訊"),
          # navigation buttons with images
          actionButton(
            ns("go_intro"),
            label = tagList(
              tags$img(src = "btn_intro.png", alt = "網站介紹")
            ),
            icon = NULL
          ),
          actionButton(
            ns("go_dataset"),
            label = tagList(
              tags$img(src = "btn_data.png", alt = "資料介紹")
            ),
            icon = NULL
          ),
          actionButton(
            ns("go_model"),
            label = tagList(
              tags$img(src = "btn_model.png", alt = "模型介紹")
            ),
            icon = NULL
          )
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
