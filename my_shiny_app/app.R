library(shiny)
library(bslib)   # 一定要載入 bslib
library(rsconnect)

# 一次定義全站主題
my_theme <- bs_theme(
  version            = 5,
  primary            = "#8084a1",
  secondary          = "#514b54",
  base_font          = font_google("Noto Sans TC"),
  heading_font       = font_google("Noto Sans TC"),
  font_scale         = 1,
  background_color   = "#0a114a",
)


# 載入 modules
source("modules/welcome.R")
source("modules/intro.R")
source("modules/dataset.R")
source("modules/model.R")

ui <- fluidPage(
  theme = my_theme,   # ← 在這裡套用主題
  tags$head(
    tags$style(HTML("
      .navbar { display: none !important; }
    "))
  ),
  navbarPage(
    id    = "tabs",
    title = NULL,

    tabPanel("Welcome", value = "welcome", welcomePageUI("welcome")),
    tabPanel("介紹",   value = "intro",   introPageUI("intro")),
    tabPanel("資料集", value = "dataset", datasetPageUI("dataset")),
    tabPanel("模型",   value = "model",   modelPageUI("model"))
  )
)

server <- function(input, output, session) {
  callModule(welcomePageServer,   "welcome")
  callModule(introPageServer,     "intro")
  callModule(datasetPageServer,   "dataset")
  callModule(modelPageServer,     "model")

  observeEvent(input$`welcome-go_intro`, {
    updateTabsetPanel(session, "tabs", selected = "intro")
  })
  observeEvent(input$`welcome-go_dataset`, {
    updateTabsetPanel(session, "tabs", selected = "dataset")
  })
  observeEvent(input$`welcome-go_model`, {
    updateTabsetPanel(session, "tabs", selected = "model")
  })
  observeEvent(input$back_welcome, {
    print("Global back_welcome clicked")
    updateTabsetPanel(session, "tabs", selected = "welcome")
  })
}

shinyApp(ui, server)
