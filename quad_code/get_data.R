# libraries ----
library(rvest)
library(tidyverse)
library(lubridate)

# get data ----
# ссылка на свод статей
links_raw <- read_html("https://habr.com/ru/company/quadcode/blog/")

# получаем ссылки на все посты в блоге компании
links <- links_raw %>% 
  html_elements(".tm-article-snippet__title_h2") %>% 
  html_element("a") %>% 
  html_attr("href")

output <- tibble()
for (i in links) {
  url <- paste0("https://habr.com", i)
  # делаем один запрос к странице
  input <- read_html(url)
  
  # основной контент страницы
  raw_data <- input %>% 
    html_elements(".tm-article-presenter__content_narrow") %>% 
    html_children() %>% 
    html_text2()
  
  # сводная информация с плашки с комментами
  info <- input %>% 
    html_element(".tm-article-sticky-panel__icons") %>% 
    html_children() %>% 
    html_text2()
  
  # всего голосов
  total <- info[1] %>% 
    str_extract("(?<=\\в )(.*?)(?=\\:)") %>% 
    as.integer()
  
  # плюсы посту
  positive <- info[1] %>% 
    str_extract("(?<=\\↑)(.*?)(?=\\ и)") %>% 
    as.integer()
  
  # минусы посту
  negative <- info[1] %>% 
    str_extract("(?<=\\↓)(.*?)(?=\\+)") %>% 
    as.integer()
  
  # итоговая оценка
  sum <- info[1] %>% 
    str_extract("(?<=\\+).*") %>% 
    as.integer()
  
  # Просмотры в тысячах
  views_in_k <- info[2] %>% 
    parse_number()

  # Сохранения в избранное
  saves <- info[3] %>% 
    parse_number()
  
  # Пользователь
  user <- raw_data[1] %>% 
    str_extract("(.*?)(?=\\ )")
  
  # Название поста
  title <- raw_data[1] %>% 
    str_extract("(?<=\\n)(.*?)(?=\\n)")
  
  # Дата поста
  date <- raw_data[1] %>% 
    str_extract("(?<=\\ )(.*?)(?=\\n)") %>% 
    str_extract("(.*?)(?=\\ в)") %>% 
    paste0(" 2021") %>% 
    lubridate::as_date(format = "%d %B %Y")
  
  # Непосредственно сам пост
  body <- raw_data[2]
  
  # Формируем датасет по каждому посту
  temp_df <- tibble(id = which(links == i),
                    user = user,
                    title = title,
                    body = body,
                    url = url,
                    date = date,
                    total = total,
                    positive = positive,
                    negative = negative,
                    sum = sum,
                    views = views_in_k * 1000,
                    saves = saves)
  
  output <- bind_rows(output, temp_df)
}

# accurate data ----
parced_data <- output %>% 
  replace_na(list(total = 0, 
                  positive = 0,
                  negative = 0,
                  sum = 0)) %>% 
  mutate(body = str_replace_all(body, "\n", " "),
         body = str_replace_all(body, "  ", " "),
         body_length = str_length(body))

# save data ----
parced_data %>% 
  saveRDS("app/data/parced_data.RDS")