library(shiny)
library(shinydashboard)   #  Фрейм-ворк для работы с дашбордами
library(shinyjs)          #  Визуальные плюшки, анимация страниц
library(tidyverse)
library(recommenderlab)
library(reshape2)         #  создание матриц
library(shinyWidgets)     #  Дополнительные виджеты

# read data ----
model <- readRDS("data/model.RDS")
data(MSWeb)

# UI ----
dash_header <- dashboardHeader(title = "Recommender system")
dash_sidebar <-    dashboardSidebar(
  width = 300,
  sidebarMenu(
    id = "sidebar_menu",
    menuItem("Пример работы", tabName = "recommender", selected = TRUE),
    uiOutput("select_products"),
    uiOutput("select_n_recommends"),
    actionButton("go", "Рассчитать")
    )
  )

dash_body <-  dashboardBody(
  # Скрыть все сообщения об ошибках в интерфейсе от пользователей
  tags$style(type="text/css",
             ".shiny-output-error { visibility: hidden; }",
             ".shiny-output-error:before { visibility: hidden; }"),
  tabItems(
    # tab ONE ----
    tabItem(
      tabName = "recommender",
      fluidRow(
        box(
          title = "Выбранные позиции",
          width = NULL,
          dataTableOutput("show_selected")
        )
      ),
      fluidRow(
        box(
          title = "Рекомендуемые позиции",
          width = NULL,
          dataTableOutput("show_pred")
        )
      )
    )
  )


)
ui <- dashboardPage(dash_header, dash_sidebar, dash_body, skin = "red")

server <- function(input, output, session) {

  output_pred <- eventReactive(input$go, {
    # формирование предсказания
    predict <- model %>%
      predict(pred_matrix(), n = input$select_n_recommends.server)
    # таблица с предсказанными значениями
    as(predict, "list") %>%
      as_tibble()
  })

  output$select_n_recommends <- renderUI({
    sliderInput(
      "select_n_recommends.server",
      label = "Укажите кол-во рекомендуемых позиций",
      min = 1,
      max = 15,
      value = 4
    )
  })


  ### селектор позиций ----
  output$select_products <- renderUI({
    pickerInput(
      "select_products.server",
      label = "Выберите позиции:",
      width = 280,
      choices = colnames(MSWeb),
      options = list(`actions-box` = TRUE,
                     `live-search` = TRUE,
                     `select-all-text` = "Все",
                     `deselect-all-text` = "Ничего"),
      inline = TRUE,
      multiple = TRUE
    )
  })

  ### накопление вектора ----
  input_table <- reactive({
    input$select_products.server %>%
      as_tibble()
  })

  # матрица для предсказаний ----
  pred_matrix <- reactive({
    # сюда накапливаем продукты, которые выбирает пользователь
    test_samples <- input_table() %>% pull(value)

    # Делаем из этих продуктов матрицу
    # размерностью, такой же как у исходно обученного алгоритма
    temp_df <- colnames(MSWeb) %>%
      as_tibble() %>%
      mutate(quantity = ifelse(value %in% test_samples, 1, 0),
             user_id = "fake_id")

    temp_mt <- dcast(temp_df,
                     user_id ~ value,
                     value.var = "quantity",
                     fill = 0)
    temp_mt <- as.matrix(temp_mt[,-1])
    rownames(temp_mt) <- "value"

    tmp_imatrix <- as(temp_mt, "itemMatrix")
    # финальная матрица, которая пойдет в предсказание
    new("binaryRatingMatrix", data = tmp_imatrix)
  })

  ### показать выбранные товары ----
  output$show_selected <- renderDataTable(
    options = list(pageLength = 10),
    input_table()
  )

  ### показать предсказанные товары ----
  output$show_pred <- renderDataTable(
    options = list(pageLength = 10),
    output_pred()
  )

}

shinyApp(ui, server)
