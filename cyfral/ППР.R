# визуалиазация числа ППР на карте
library(tidyverse)
set.seed(42)
data = tibble(x = rnorm(1:515), y = rnorm(1:515), type = factor(c(rep('ППР',510),rep("ЗН",5))))
ggplot(data, aes(x, y, size = 0.01, alpha = 3/4, col = type)) + geom_point()

data2 = tibble(x = rnorm(1:65), y = rnorm(1:65), type = factor(c(rep('ППР',60),rep("ЗН",5))))
ggplot(data2, aes(x, y, size = 0.01, alpha = 3/4, col = type)) + geom_point()

data3 = tibble(x = rnorm(1:8), y = rnorm(1:8), type = factor(c(rep('ППР',3),rep("ЗН",5))))
ggplot(data3, aes(x, y, size = 0.01, alpha = 2/3, col = type)) + geom_point()
