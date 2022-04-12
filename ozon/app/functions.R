translate_period <- function(period) {
  case_when(
    period == "День" ~ "created_at",
    period == "Неделя" ~ "week",
    period == "Месяц" ~ "month",
    period == "Квартал" ~ "quarter"
  ) %>% across()
}

# функция форматирования подписей на графиках
# на основе оригинальных функций пакета echarts4r
labels_item_formatter <- function (style = c("decimal", "percent", "currency"), digits = 0,
                                   locale = "ru", currency = "rub") {
  if (is.null(locale)) {
    locale <- .get_locale()
  }
  style <- match.arg(style)
  opts <- list(style = style, minimumFractionDigits = digits,
               maximumFractionDigits = digits, currency = currency)
  tip <- htmlwidgets::JS(sprintf(
  "function(params, ticket, callback) {
  var fmt = new Intl.NumberFormat('%s', %s);
  return fmt.format(parseFloat(params.value[1]));
  }",
  locale, jsonlite::toJSON(opts, auto_unbox = TRUE)))
  tip <- structure(tip, class = c("JS_EVAL", "item_formatter"))
  return(tip)
}

tooltip_item_formatter <- function (style = c("decimal", "percent", "currency"), digits = 0,
                                    locale = NULL, currency = "USD")
{
  if (is.null(locale)) {
    locale <- .get_locale()
  }
  style <- match.arg(style)
  opts <- list(style = style, minimumFractionDigits = digits,
               maximumFractionDigits = digits, currency = currency)
  tip <- htmlwidgets::JS(sprintf(
    "function(params, ticket, callback) {
  var fmt = new Intl.NumberFormat('%s', %s);
  var idx = 0;
  return params.value[0] + '<br>' +
    params.marker + ' ' +
    params.seriesName + ': ' + fmt.format(parseFloat(params.value[1]));
}",
    locale, jsonlite::toJSON(opts, auto_unbox = TRUE)))
  tip <- structure(tip, class = c("JS_EVAL", "item_formatter"))
  return(tip)
}

calculate_it <- function(type, var, item_count = NULL) {
  if(type == "sum"){
    sum(var, na.rm = TRUE)
  } else if(type == "avg"){
    mean(var, na.rm = TRUE)
  } else if(type == "count"){
    n()
  } else if(type == "med"){
    median(var,  na.rm = TRUE)
  } else if(type == "max"){
    max(var, na.rm = TRUE)
  } else if(type == "min") {
    min(var, na.rm = TRUE)
  } else if(type == "items"){
    mean(item_count, na.rm = TRUE)
  }
}

# Функция округления до миллиона
axis_formatter <- function(style = c("decimal", "percent", "currency"), digits = 0,
          locale = NULL, currency = "USD")
{
  if (is.null(locale)) {
    locale <- .get_locale()
  }
  style <- match.arg(style)
  opts <- list(style = style, minimumFractionDigits = digits,
               maximumFractionDigits = digits, currency = currency)
  htmlwidgets::JS(sprintf("function(value, index) {
  var fmt = new Intl.NumberFormat('%s', %s);
  return value/1000000 + ' Млн';
                          }",
                          locale, jsonlite::toJSON(opts, auto_unbox = TRUE)))
}

translate_group_type <- function(i) {
  case_when(i == "is_individual" ~ "ФЛ / ЮЛ",
            i == "ckg" ~ "ЦКГ",
            i == "source" ~ "Источник в системе",
            i == "brand" ~ "Брэнд",
            i == "parent_group_name" ~ "Группа товаров")
}

translate_calc_type <- function(i) {
  case_when(i == "sum" ~ "Сумма заказов",
            i == "avg" ~ "Средний чек",
            i == "med" ~ "Медианный чек",
            i == "count" ~ "Кол-во заказов",
            i == "max" ~ "Максимальный заказ",
            i == "min" ~ "Минимальный заказ",
            i == "items" ~ "В среднем SKU в заказе")
}
