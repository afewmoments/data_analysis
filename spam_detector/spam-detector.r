Sys.setenv(TZ='GMT')
setwd("e:/work/r")

# Подключаем нужные пакеты
library('tm')		# текстовый майнинг
library('ggplot2')	# рисование графиков

# Задаём пути к файлам с письмами
spam_learn.path <- file.path("data", "spam_learn")
spam_verify.path <- file.path("data", "spam_verify")
easy_nonspam_learn.path <- file.path("data", "easy_nonspam_learn")
easy_nonspam_verify.path <- file.path("data", "easy_nonspam_verify")
hard_nonspam_verify.path <- file.path("data", "hard_nonspam_verify")

# Считывает текстовое послание из заданного файла с письмом
getMessage <- function(path)
{
    con <- file(path, open = "rt", encoding = "latin1")
    text <- readLines(con)

    # Текстовое послание всегда начинается после пустой строки
    msg <- text[seq(which(text == "")[1] + 1, length(text), 1)]
    close(con)

    return(paste(msg, collapse = "\n"))
}

############################################################
### Готовим тренировочные данные из спамного корпуса текстов
############################################################

# Загружаем все спамные письма в единый вектор
spam_learn.docs <- dir(spam_learn.path)
spam_learn.docs <- spam_learn.docs[which(spam_learn.docs != "cmds")]
all.spam_learn <- sapply(spam_learn.docs,
                         function(p) getMessage(file.path(spam_learn.path, p)))

# Создаёт терм-документную матрицу (TDM) из вектора писем doc.vec.
# Она нужна, чтобы "конструировать признаки" для нашего детектора.
getTDM <- function(doc.vec)
{
    control <- list(stopwords=TRUE, removePunctuation=TRUE,
                    removeNumbers=TRUE, minDocFreq=2)
    doc.corpus <- Corpus(VectorSource(doc.vec))
    doc.dtm <- TermDocumentMatrix(doc.corpus, control)
    return(doc.dtm)
}

# Создаём терм-документную матрицу для корпуса текстов со спамом
spam_learn.tdm <- getTDM(all.spam_learn)

# Создаём срез данных, который будет хранить набор спам-признаков
spam_learn.matrix <- as.matrix(spam_learn.tdm)
spam_learn.counts <- rowSums(spam_learn.matrix)
spam_learn.df <- data.frame(cbind(names(spam_learn.counts),
                            as.numeric(spam_learn.counts)))
names(spam_learn.df) <- c("term", "frequency")
spam_learn.df$frequency <- as.numeric(spam_learn.df$frequency)

# Генерируем тренировочные данные
spam_learn.occurrence <- sapply(1:nrow(spam_learn.matrix),
                                function(i)
                                {
                                    length(which(spam_learn.matrix[i, ] > 0)) /
                                    ncol(spam_learn.matrix)
                                })
spam_learn.density <- spam_learn.df$frequency / sum(spam_learn.df$frequency)

spam_learn.df <- transform(spam_learn.df,
                           density = spam_learn.density,
                           occurrence = spam_learn.occurrence)

############################################################
### Готовим тренировочные данные из корпуса текстов easy_nonspam
############################################################

# Загружаем все письма easy_nonspam в единый вектор
easy_nonspam_learn.docs <- dir(easy_nonspam_learn.path)
easy_nonspam_learn.docs <- 
                    easy_nonspam_learn.docs[which(easy_nonspam_learn.docs != "cmds")]
all.easy_nonspam_learn <-
                    sapply(easy_nonspam_learn.docs[1:length(spam_learn.docs)],
                           function(p) getMessage(file.path(easy_nonspam_learn.path, p))
                          )

# Создаём терм-документную матрицу
easy_nonspam_learn.tdm <- getTDM(all.easy_nonspam_learn)

# Создаём срез данных, который будет хранить набор признаков для easy_nonspam
easy_nonspam_learn.matrix <- as.matrix(easy_nonspam_learn.tdm)
easy_nonspam_learn.counts <- rowSums(easy_nonspam_learn.matrix)
easy_nonspam_learn.df <- data.frame(cbind(names(easy_nonspam_learn.counts),
                                          as.numeric(easy_nonspam_learn.counts)))
names(easy_nonspam_learn.df) <- c("term", "frequency")
easy_nonspam_learn.df$frequency <- as.numeric(easy_nonspam_learn.df$frequency)

# Генерируем тренировочные данные
easy_nonspam_learn.occurrence <-
                       sapply(1:nrow(easy_nonspam_learn.matrix),
                              function(i)
                              {
                                  length(which(easy_nonspam_learn.matrix[i, ] > 0)) /
                                  ncol(easy_nonspam_learn.matrix)
                              })
easy_nonspam_learn.density <- easy_nonspam_learn.df$frequency /
                              sum(easy_nonspam_learn.df$frequency)

easy_nonspam_learn.df <- transform(easy_nonspam_learn.df,
                                   density = easy_nonspam_learn.density,
                                   occurrence = easy_nonspam_learn.occurrence)

############################################################
### Создаём классификатор, и проверяем его
############################################################

# Вычислят наивную байесовскую вероятность: спам / не-спам
classifyEmail <- function(path, trainingDF, prior=0.5, cNone=1e-3)
{
	# Первые три шага - точно те же, что мы 
	# email text data in a workable format
	msg <- getMessage(path)
	msg.tdm <- getTDM(msg)
	msg.freq <- rowSums(as.matrix(msg.tdm))

	# Находим термы, которые есть и в анализируемом письме и в срезе данных
	msg.match <- intersect(names(msg.freq), trainingDF$term)

	# А теперь выполняем наивный байесовский подсчёт
	if(length(msg.match) < 1)
	{
		return(prior * cNone^(length(msg.freq)))
	}
	else
	{
		match.probs <- trainingDF$occurrence[match(msg.match, trainingDF$term)]
		return(prior * prod(match.probs) * cNone ^ (length(msg.freq)-length(msg.match)))
	}
}

# Проверяем насколько хорошо классификатор обрабатывает письма категории hard_nonspam
hard_nonspam_verify.docs <- dir(hard_nonspam_verify.path)
hard_nonspam_verify.docs <-
                hard_nonspam_verify.docs[which(hard_nonspam_verify.docs != "cmds")]

hard_nonspam_verify.spam_test <- sapply(hard_nonspam_verify.docs,
                function(p) classifyEmail(file.path(hard_nonspam_verify.path, p),
                                          trainingDF = spam_learn.df))
    
hard_nonspam_verify.nonspam_test <- sapply(hard_nonspam_verify.docs,
                function(p) classifyEmail(file.path(hard_nonspam_verify.path, p),
                                          trainingDF = easy_nonspam_learn.df))
    
hard_nonspam_verify.res <-
          ifelse(hard_nonspam_verify.spam_test > hard_nonspam_verify.nonspam_test,
                      TRUE, FALSE)

summary(hard_nonspam_verify.res)

############################################################
### Тестируем свой детектор на письмах всех трёх категорий
############################################################

spamClassifier <- function(path)
{
  prob.spam_learn <- classifyEmail(path, spam_learn.df)
  prob.nonspam <- classifyEmail(path, easy_nonspam_learn.df)
  return(c(prob.spam_learn, prob.nonspam, ifelse(prob.spam_learn > prob.nonspam, 1, 0)))
}

easy_nonspam_verify.docs <- dir(easy_nonspam_verify.path)
easy_nonspam_verify.docs <-
         easy_nonspam_verify.docs[which(easy_nonspam_verify.docs != "cmds")]

hard_nonspam_verify.docs <- dir(hard_nonspam_verify.path)
hard_nonspam_verify.docs <-
         hard_nonspam_verify.docs[which(hard_nonspam_verify.docs != "cmds")]

spam_verify.docs <- dir(spam_verify.path)
spam_verify.docs <- spam_verify.docs[which(spam_verify.docs != "cmds")]

# Прогоняем детектор по письмам всех трёх категорий
easy_nonspam_verify.class <- suppressWarnings(lapply(easy_nonspam_verify.docs,
                      function(p)
                      {
                          spamClassifier(file.path(easy_nonspam_verify.path, p))
                      }))
hard_nonspam_verify.class <- suppressWarnings(lapply(hard_nonspam_verify.docs,
                      function(p)
                      {
                          spamClassifier(file.path(hard_nonspam_verify.path, p))
                      }))
spam_verify.class <- suppressWarnings(lapply(spam_verify.docs,
                      function(p)
                      {
                          spamClassifier(file.path(spam_verify.path, p))
                      }))

# Готовим итоговый срез данных, чтобы посмотреть как детектор отработал
easy_nonspam_verify.matrix <- do.call(rbind, easy_nonspam_verify.class)
easy_nonspam_verify.final <- cbind(easy_nonspam_verify.matrix, "easy_nonspam")

hard_nonspam_verify.matrix <- do.call(rbind, hard_nonspam_verify.class)
hard_nonspam_verify.final <- cbind(hard_nonspam_verify.matrix, "hard_nonspam")

spam_verify.matrix <- do.call(rbind, spam_verify.class)
spam_verify.final <- cbind(spam_verify.matrix, "SPAM")

class.matrix <- rbind(easy_nonspam_verify.final,
                      hard_nonspam_verify.final,
                      spam_verify.final)
classDF <- data.frame(class.matrix)
names(classDF) <- c("Prob.SPAM" ,"Prob.NONSPAM", "Class", "Type")

classDF$Prob.SPAM <- as.numeric(classDF$Prob.SPAM)
classDF$Prob.NONSPAM <- as.numeric(classDF$Prob.NONSPAM)
classDF$Class <- as.logical(as.numeric(classDF$Class))
classDF$Type <- as.factor(classDF$Type)

# Показываем процент ложноположительных и ложноотрицательных срабатываний
getResults <- function(bool.vector)
{
  results <- c(length(bool.vector[which(bool.vector == FALSE)]) / length(bool.vector),
               length(bool.vector[which(bool.vector == TRUE)]) / length(bool.vector))
  return(results)
}

easy_nonspam_verify.col <- getResults(subset(classDF, Type == "easy_nonspam")$Class)
hard_nonspam_verify.col <- getResults(subset(classDF, Type == "hard_nonspam")$Class)
spam_verify.col <- getResults(subset(classDF, Type == "SPAM")$Class)
class.res <- rbind(easy_nonspam_verify.col, hard_nonspam_verify.col, spam_verify.col)
colnames(class.res) <- c("NONSPAM", "SPAM")
rownames(class.res) <- c("easy_nonspam", "hard_nonspam", "spam")

print(class.res)

# Рисуем на графике результат работы детектора
class.plot <- ggplot(classDF, aes(x = log(Prob.NONSPAM), log(Prob.SPAM))) +
    geom_point(aes(shape = Type, alpha = 0.5)) +
    geom_abline(intercept = 0, slope = 1) +
    scale_shape_manual(values = c("easy_nonspam" = 1,
                                  "hard_nonspam" = 2,
                                  "SPAM" = 3),
                       name = "Email Type") +
    scale_alpha(guide = "none") +
    xlab("log[Prob(NONSPAM)]") +
    ylab("log[Prob(SPAM)]") +
    theme_bw() +
    theme(axis.text.x = element_blank(), axis.text.y = element_blank())

ggsave(plot = class.plot,
       filename = file.path("images", "final_classify.pdf"),
       height = 10,
       width = 10)