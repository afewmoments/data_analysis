# libraries ----

library(shiny)
library(tidyverse)
library(echarts4r)        #  интерактивные графики
library(bs4Dash)          #  фреймворк для дашбордов
library(fs)               #  Работа с папками
library(DT)               #  Рендеринг таблиц
library(writexl)          #  Сохранение экселек
library(lubridate)        #  Даты
library(tokenizers)       # для анализа текстов
library(tidytext)         # для анализа текстов
library(writexl)          #  Сохранение экселек
library(fs)               #  Работа с папками

translate_group_type <- function(i) {
  case_when(
    i == "total" ~ "суммарного кол-ва голосов", 
    i == "positive" ~ "лайков", 
    i == "negative" ~ "дизлайков",
    i == "sum" ~ "финального кол-во голосов", 
    i == "views" ~ "просмотров", 
    i == "saves" ~ "сохранений", 
    i == "body_length" ~ "кол-во символов в посте")
}

source("functions.R")

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
    title = " ozontech blog",
    color = "danger",
    href = "https://habr.com/ru/company/ozontech/blog/",
    image = "2.png",
    opacity = 1
  ),
  leftUi = tagList(
    dropdownMenu(
      badgeStatus = "success",
      type = "notifications",
      notificationItem(
        inputId = "notification_date",
        text = paste0("Данные на ", "12-04-2022"),
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
    bs4Dash::menuItem("Анализ Хабро-блога",
                      icon = icon("book-reader"),
                      tabName = "generate_orders",
                      selected = TRUE)
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
            a("Описание скрипта сбора данных на Github", href = "https://github.com/driapitek/data_analysis/blob/master/ozon/get_data.R"),
            br(),
            br(),
            p(HTML("&#8226"), " id ", HTML("&#8212"), "идентификатор поста"),
            p(HTML("&#8226"), " user ", HTML("&#8212"), "никнейм сотрудника на Хабре"),
            p(HTML("&#8226"), " title ", HTML("&#8212"), "Заголовок поста"),
            p(HTML("&#8226"), " body ", HTML("&#8212"), "Тело поста"),
            p(HTML("&#8226"), " url ", HTML("&#8212"), "Ссылка на пост"),
            p(HTML("&#8226"), " date ", HTML("&#8212"), "Дата публикации"),
            p(HTML("&#8226"), " total ", HTML("&#8212"), "Общее число голосов всех пользователей Хабра"),
            p(HTML("&#8226"), " positive ", HTML("&#8212"), "Количество позитивных оценок"),
            p(HTML("&#8226"), " negative ", HTML("&#8212"), "Количество негативных оценок"),
            p(HTML("&#8226"), " sum ", HTML("&#8212"), "Финальная оценка. Равна сумме позитивных и негативных оценок"),
            p(HTML("&#8226"), " views ", HTML("&#8212"), "Количество просмотров публикации. Считается самим Хабром"),
            p(HTML("&#8226"), " saves ", HTML("&#8212"), "Количество добавлений в избранное"),
            p(HTML("&#8226"), " body_length ", HTML("&#8212"), "Количество символов в посте")
          ),
          tabPanel(
            title = "Скачать датасет",
            downloadButton("download_button", 
                           "Скачать полную таблицу в .csv"),
          )
        )
      ),
      fluidRow(
        bs4Dash::tabBox(
          type = "tabs",
          label = "Как выходили посты в блоге",
          id = "calendar_and_month",
          status = "primary",
          solidHeader = FALSE,
          width = 12,
          tabPanel(
            width = 12,
            title = "Ежедневный разрез",
            uiOutput("select_group_type.ui"),
            uiOutput("select_year.ui"),
            echarts4rOutput("calendar")
          ),
          tabPanel(
            width = 12,
            title = "Ежемесячный разрез",
            echarts4rOutput("by_month")
          )
        )
      ),
      fluidRow(
        box(
          width = 12,
          title = "Корреляции данных",
          uiOutput("select_group_type2.ui"),
          uiOutput("select_group_type3.ui"),
          plotOutput("correlation")
        )
      ),
      fluidRow(
        box(
          width = 12,
          title = "Облако тегов",
          uiOutput("select_user.ui"),
          uiOutput("select_post.ui"),
          uiOutput("word_n_range.ui"),
          echarts4rOutput("wordcloud")
        )
      )
    )
  )
)


# UI ALL ----

ui <- bs4Dash::bs4DashPage(header, sidebar, body, dark = FALSE)

server <- function(input, output, session) {

  # INPUTS ----
  # select user ----
  output$select_user.ui <- renderUI({
    selectInput(
      inputId = "select_user.server",
        label = "Пользователь:",
        width = 280,
        choices = parced_data %>% 
                     distinct(user) %>% 
                     arrange(user) %>%  
                     pull(user),
        selected = "Antishev"
        )
  })

  # select group2 type ----
  output$select_group_type2.ui <- renderUI({
    selectInput(
      inputId = "select_group_type2.server",
      label = "Первый параметр для сравнения:",
      width = 280,
      choices =   c('Всего голосов' = "total", 
                    'Лайки' = "positive", 
                    'Дизлайки' = "negative",
                    'Финальная оценка' = "sum", 
                    'Просмотров' = "views", 
                    'Сохранения' = "saves", 
                    'Символов в посте' = "body_length"),
      selected = "total"
    )
  })
  
  # select group3 type ----
  output$select_group_type3.ui <- renderUI({
    foo <- c('Всего голосов' = "total", 
             'Лайки' = "positive", 
             'Дизлайки' = "negative",
             'Финальная оценка' = "sum", 
             'Просмотров' = "views", 
             'Сохранения' = "saves", 
             'Символов в посте' = "body_length")
    bar <- which(foo == input$select_group_type2.server)
    
    selectInput(
      inputId = "select_group_type3.server",
      label = "Второй параметр для сравнения:",
      width = 280,
      choices =   foo[-bar],
      selected = "positive"
    )
  })
    
  # select group type ----
  output$select_group_type.ui <- renderUI({
    selectInput(
      inputId = "select_group_type.server",
      label = "Разрез данных:",
      width = 280,
      choices =   c('Всего голосов' = "total", 
                    'Лайки' = "positive", 
                    'Дизлайки' = "negative",
                    'Финальная оценка' = "sum", 
                    'Просмотров' = "views", 
                    'Сохранения' = "saves", 
                    'Символов в посте' = "body_length"),
      selected = "positive"
    )
  })
  
  # select year for calendar ----
  output$select_year.ui <- renderUI({
    selectInput(
      inputId = "select_year.server",
      label = "Выберите год:",
      width = 280,
      choices = parced_data %>% 
        mutate(year = year(date)) %>% 
        distinct(year) %>% 
        pull(year),
      selected = "2021"
    )
  })
  
  # range to wordcloud
  output$word_n_range.ui <- renderUI({
    sliderInput(
      inputId = "word_n_range.server",
      label = "Кол-во слов:",
      width = 280,
      min = 1,
      max = 25,
      value = 25
    )
  })
  
  # select post ----
  output$select_post.ui <- renderUI({
    shinyWidgets::pickerInput(
      inputId = "select_post.server",
      label = "Пост пользователя:",
      width = 280,
      options = list(
        `actions-box` = TRUE,
        `select-all-text` = "Все",
        `deselect-all-text` = "Ни одного",
        `live-search` = TRUE,
        `none-selected-text` = "Ничего не выбрано"),
      multiple = TRUE,
      choices = parced_data %>% 
        filter(user %in% input$select_user.server) %>% 
        pull(title),
      selected = parced_data %>% 
        filter(user %in% input$select_user.server) %>% 
        head(1) %>% 
        pull(title) 
      )
  })
  

  
  # OUTPUTS ----
  # download button ----
  output$download_button <- downloadHandler(
    filename = "ozon_habr_blog_parsed.csv",
    content = function(file){
      write_csv(parced_data, file)
    })

  # by_month ----
  output$by_month <- renderEcharts4r({
    parced_data %>% 
     mutate(year = year(date),
            month = month(date)) %>% 
     count(year, month, name = "post_count") %>% 
     group_by(year) %>% 
     mutate(mean_count = mean(post_count)) %>% 
     ungroup() %>% 
     mutate(year_month = as.Date(paste0(year, "-", month, "-01"))) %>% 
     e_charts(year_month) %>% 
     e_bar(post_count) %>% 
     e_labels() %>% 
     e_line(mean_count) %>% 
     e_tooltip("item", formatter = tooltip_item_formatter("decimal", locale =  "ru")) %>% 
     e_title(text = "Ежемесячное кол-во постов",
             subtext = "за период с 01-12-2018 по 01-12-2021. Зелёным - в среднем постов за календарный год") %>% 
     e_legend(show = FALSE)
  })
    
  # calendar ----
  output$calendar <- renderEcharts4r({
    max_value <- parced_data %>% 
      filter(year(date) == input$select_year.server) %>% 
      select(input$select_group_type.server) %>% 
      pull() %>% 
      max()
    
    median_value <- parced_data %>% 
      filter(year(date) == input$select_year.server) %>% 
      select(input$select_group_type.server) %>% 
      pull() %>% 
      median() %>% 
      round(., 1)
    
    user_max <- parced_data %>% 
      filter(year(date) == input$select_year.server) %>% 
      filter(across(all_of(input$select_group_type.server)) == max_value) %>% 
      pull(user)
    
    title_max <- parced_data %>% 
      filter(year(date) == input$select_year.server) %>% 
      filter(across(all_of(input$select_group_type.server)) == max_value) %>% 
      pull(title)
    
    date_max <- parced_data %>% 
      filter(year(date) == input$select_year.server) %>% 
      filter(across(all_of(input$select_group_type.server)) == max_value) %>% 
      pull(date)
    
    plot <- parced_data %>% 
      arrange(date) %>% 
      # mutate(check = 1,
      #        cumsum = cumsum(check)) %>% 
      #select(date, check) %>% 
      e_charts(date) %>% 
      e_calendar(range = input$select_year.server,  top = "80") %>%  
      e_heatmap_(input$select_group_type.server, coord_system = "calendar") %>% 
      e_visual_map(max = max_value, top = "240", type = c("piecewise")) %>%  
      e_title(paste0("Медианно по ", median_value, " ", translate_group_type(input$select_group_type.server), " на пост"), 
              paste0("Максимум ", translate_group_type(input$select_group_type.server), " у поста: «", title_max,"» пользователя ", 
                     user_max, " (", max_value, ") от ", date_max)) %>% 
      e_tooltip("item", formatter = tooltip_item_formatter("decimal", locale =  "ru"))
    
    if (input$dark_mode) plot <- plot %>% e_theme("dark-bold")
    plot
    
  })
  
  # correlation ----
  output$correlation <- renderPlot({
    to_plot <- parced_data %>%
      rename("x" = input$select_group_type2.server,
             "y" = input$select_group_type3.server)
    
    coeff_cor <- cor(to_plot$x, to_plot$y) %>% round(3)
    
    conclution <- if(coeff_cor == 1){
      "данные скореллированы"
    } else if (coeff_cor > 0.8){
      "высокой связанности параметров"
    } else if (coeff_cor > 0.5){
      "невысокой связанности параметров"
    } else if (coeff_cor > 0.2) {
      "незначительной связанности параметров"
    }else if (coeff_cor < 0.2) {
      "том, что изменение одного параметра, практически не влияет на другой параметр"
    }
    
    to_plot %>%
      ggplot(aes(x, y)) +
      geom_point() +
      geom_smooth(method = "lm") +
      labs(
        x = translate_group_type(input$select_group_type2.server),
        y = translate_group_type(input$select_group_type3.server),
        title = paste0("Коэффициент корреляции = ", coeff_cor),
        subtitle = paste0("Что может говорить о ", conclution)
      )
  })
  
  # parced table ----
  output$parced_table <- renderDataTable(
    callback = JS('table.page(3).draw(false);'),
    options = list(pageLength = 10,
                   columnDefs = list(list(targets = c(3, 4, 5),
                                          render = JS(
                                            "function(data, type, row, meta) {",
                                            "return type === 'display' && data.length > 6 ?",
                                            "'<span title=\"' + data + '\">' + data.substr(0, 6) + '...</span>' : data;",
                                            "}")))),
    parced_data
    )
  
  # wordcloud ----
  output$wordcloud <- renderEcharts4r({
    plot <- parced_data %>% 
      unnest_tokens(word, body)  %>% 
      anti_join(stop_words, by = "word") %>%
      mutate(word = tokenize_word_stems(word, language = "russian", simplify = TRUE)) %>% 
      unnest(word) %>% 
      count(title, user, word, sort = TRUE) %>% 
      filter(user %in% input$select_user.server,
             title %in% input$select_post.server) %>% 
      head(input$word_n_range.server) %>% 
      e_color_range(n, color) %>% 
      e_charts() %>%  
      e_cloud(word, n, color, shape = "circle") %>% 
      e_title(paste0(input$word_n_range.server," cамых часто употребляемых слов в посте ", input$select_user.server), "Приведена морфемная форма слова, исключены стоп-слова")
    
    if (input$dark_mode) plot <- plot %>% e_theme("dark-bold")
    plot
  })

  
}

shinyApp(ui, server)
