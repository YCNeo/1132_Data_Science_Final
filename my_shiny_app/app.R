library(shiny)
library(bslib)   # 一定要載入 bslib

# 一次定義全站主題
my_theme <- bs_theme(
  version            = 5,
  primary            = "#1f4298",
  secondary          = "#dfddbd",
  base_font          = font_google("Shippori Mincho B1"),
  heading_font       = font_google("Shippori Mincho B1"),
  font_scale         = 1,
  background_color   = "#2d69a5",
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
    updateTabsetPanel(session, "tabs", selected = "welcome")
  })
}

shinyApp(ui, server)
