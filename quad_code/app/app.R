# libraries ----

library(shiny)
library(tidyverse)
library(echarts4r)        #  интерактивные графики
library(bs4Dash)          #  фреймворк для дашбордов
library(fs)               #  Работа с папками
library(DT)
library(writexl)          #  Сохранение экселек
library(lubridate)        #  Даты
library(tokenizers)
library(wordcloud)

# read data ----

all_files <- dir_ls("data/", recurse = T, glob = "*.RDS")
#all_files <- dir_ls("app/data/", recurse = T, glob = "*.RDS")
for (i in all_files) {
  df_name <- all_files[i]  %>%
    #str_remove("app/") %>%
    str_extract("(?<=\\/)(.*?)(?=\\.RDS)")
  temp_df <- readRDS(all_files[i])
  assign(df_name, temp_df)
  rm(temp_df)
}

# UI header ----

header <-  bs4Dash::dashboardHeader(
  title = bs4Dash::dashboardBrand(
    title = " quadcode на Хабре",
    color = "danger",
    href = "https://habr.com/ru/company/quadcode/blog/",
    image = "2.png",
    opacity = 1
  ),
  leftUi = tagList(
    dropdownMenu(
      badgeStatus = "success",
      type = "notifications",
      notificationItem(
        inputId = "notification_date",
        text = paste0("Данные на ", "26-12-2021"),
        status = "success"
      )
    )
  )
)

# UI sidebar ----

sidebar <-  bs4Dash::bs4DashSidebar(
  width = 300,
  collapsed = TRUE,
  bs4Dash::sidebarMenu(
    id = "sidebar_menu",
    bs4Dash::menuItem("Сводный анализ",
                      icon = icon("book-reader"),
                      tabName = "generate_orders",
                      selected = TRUE)
  )
)

# UI controlbar ----

controlbar <- bs4Dash::bs4DashControlbar(
  id = "controlbar",
  skin = "light",
  pinned = FALSE,
  collapsed = TRUE,
  width = 290,
  overlay = FALSE,
  controlbarMenu(
    id = "controlbarMenu",
    type = "tabs",
    controlbarItem(
      "Отбор данных",
      column(
        width = 12,
        align = "left",
        uiOutput("date_ranger.ui"),
        uiOutput("select_period.ui"),
        uiOutput("select_calc.ui")
      )
    )
  )
)

# UI body ----

body <-  bs4Dash::bs4DashBody(

  # HIDE ALL ERRORS ----
  tags$style(type="text/css",
             ".shiny-output-error { visibility: hidden; }",
             ".shiny-output-error:before { visibility: hidden; }"),

  bs4Dash::tabItems(
    bs4Dash::tabItem(
      ## Основной таб ----
      tabName = "generate_orders",
      fluidRow(
        bs4Dash::tabBox(
          type = "tabs",
          label = "Исходные данные",
          id = "north_star",
          status = "primary",
          solidHeader = FALSE,
          width = 12,
          tabPanel(
            title = "Таблица",
            dataTableOutput("parced_table")
          ),
          tabPanel(
            title = "Легенда к данным",
              p(HTML("&#8226"), " Данные обновляются при запуске приложения"),
              p(HTML("&#8226"), " По каким проектам ведётся подсчёт ", HTML("&#8212"),  'B2B Движение, B2B техподдержка, B2Bmotion.cloud, Sell-Out, Sell-Out ABB техподдержка, АВС B2BC'),
              p(HTML("&#8226"), " Оценка в часах ", HTML("&#8212"),  "суммарная оценка по всем задачам из бэклога"),
              p(HTML("&#8226"), " Уже отработано, часов ", HTML("&#8212"),  "суммарное время по задачам из бэклога, отмеченное в Tempo. В сумму не входят Эпики."),
              p(HTML("&#8226"), " Осталось, часов ", HTML("&#8212"),  "разница между средним кол-во часов (50 ч.), и отработанным временем в спринте")
          ),
          tabPanel(
            title = "Скачать датасет"
          )
        )
      ),
      fluidRow(
        echarts4rOutput("calendar")
      ),
      fluidRow(
        echarts4rOutput("wordcloud")
      )
    )
  )
)


# UI ALL ----

ui <- bs4Dash::bs4DashPage(header, sidebar, body, controlbar, dark = FALSE)

server <- function(input, output, session) {

  # INPUTS ----
  
  # OUTPUTS ----
  # calendar ----
  output$calendar <- renderEcharts4r({
    parced_data %>% 
      arrange(date) %>% 
      # mutate(check = 1,
      #        cumsum = cumsum(check)) %>% 
      #select(date, check) %>% 
      e_charts(date) %>% 
      e_calendar(range = "2021") %>%  
      e_heatmap(views, coord_system = "calendar") %>% 
      e_visual_map(max = 15000, top = "200") |> 
      e_title("Как часто выходили посты от QuadCode на Хабре") %>% 
      e_tooltip("item") 
  })
  
  # parced table ----
  output$parced_table <- renderDataTable(
    callback = JS('table.page(3).draw(false);'),
    options = list(pageLength = 15,
                   columnDefs = list(list(targets = c(3,4,5),
                                          render = JS(
                                            "function(data, type, row, meta) {",
                                            "return type === 'display' && data.length > 6 ?",
                                            "'<span title=\"' + data + '\">' + data.substr(0, 6) + '...</span>' : data;",
                                            "}")))),
    parced_data
    )
  
  # wordcloud ----
  output$wordcloud <- renderEcharts4r({
    parced_data %>% 
      unnest_tokens(word, body)  %>% 
      anti_join(stop_words, by = "word") %>%
      mutate(word = tokenize_word_stems(word, language = "russian", simplify = TRUE)) %>% 
      unnest(word) %>% 
      count(title, user, word, sort = TRUE) %>% 
      filter(user == "darya-dvoeglazova") %>% 
      head(20) %>% 
      e_color_range(n, color) %>% 
      e_charts() %>%  
      e_cloud(word, n, color, shape = "circle", sizeRange = c(5, 150)) %>% 
      e_title(paste0("20","Самых часто употребляемых слов в посте ", "darya-dvoeglazova"), "Приведена морфемная форма слова, исключены стоп-слова")
  })

  
}

shinyApp(ui, server)
