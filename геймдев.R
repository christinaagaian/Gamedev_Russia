getwd()

setwd('C:/Users/User/Downloads')

# Загружаем библиотеки
#install.packages("plotrix")
#install.packages("dplyr")   
#install.packages("tidyverse")
#install.packages("ggplot2")
#install.packages("fivethirtyeight")
#install.packages("reshape2")
#install.packages("ggcorrplot")
#install.packages("car")
#install.packages("gclus")
library("plotrix")
library("dplyr")


library("tidyverse")
library("ggplot2")
library('corrplot')
library(car)
library("reshape2")
library("plotrix")
library("dplyr")

library("tidyverse")
library("ggplot2")
library("ggcorrplot")
library(readxl)
ddf <- read_excel("геймдев (8).xlsx")
View(ddf)


m(is.na(ddf))

colnames(ddf)[which(colnames(ddf) == "Выручка_млрд_$")] <- "Выручка_млрд"
df <- subset(ddf, Страна != "Япония")


names(df)[names(df) == "Объем_рынка_млрд_$"] <- "Объем_рынка_млрд"

summary(df)


ggplot(data = df, aes(x = Объем_рынка_млрд)) +
  geom_histogram(binwidth = 0.2, fill = "red", color = "black") +
  ggtitle("Объем_рынка_млрд") 



ggplot(data=df, aes(x = Налоговая_ставка)) +
  geom_histogram(binwidth = 0.2, fill = "purple", color = "black") +
  ggtitle("Налоговая_ставка") 



ggplot(data=df, aes(x = Налоговый_вычет)) +
  geom_histogram(binwidth = 0.2, fill = "green", color = "black") +
  ggtitle("Налоговый_вычет") 




ggplot(data=df, aes(x = Субсидии)) +
  geom_histogram(binwidth = 0.2, fill = 'blue', color = "black") +
  ggtitle("Субсидии")



ggplot(data=df, aes(x = Кластеры)) +
  geom_histogram(binwidth = 0.2, fill = "orange", color = "black") +
  ggtitle("Кластеры") 



ggplot(data=df, aes(x = Маркировка)) +
  geom_histogram(binwidth = 0.2, fill = "yellow", color = "black") +
  ggtitle("Маркировка")




cor(df$Объем_рынка_млрд, df$Налоговая_ставка, method = "pearson")
corr1 <- cor.test(df$Объем_рынка_млрд, df$Налоговая_ставка )
corr1


cor(df$Объем_рынка_млрд, df$Налоговый_вычет, method = "pearson")
corr2 <- cor.test(df$Объем_рынка_млрд, df$Налоговый_вычет)
corr2


cor(df$Объем_рынка_млрд, df$Субсидии, method = "pearson")
corr3 <- cor.test(df$Объем_рынка_млрд, df$Субсидии)
corr3
# Связь есть

cor(df$Объем_рынка_млрд, df$Кластеры, method = "pearson")
corr4 <- cor.test(df$Объем_рынка_млрд, df$Кластеры)
corr4
# Связь есть

cor(df$Объем_рынка_млрд, df$Маркировка, method = "pearson")
corr5 <- cor.test(df$Объем_рынка_млрд, df$Маркировка)
corr5



# Построим корреляционную матрицу и проинтерпретируем ее
ggcorrplot(cor(df),hc.order = TRUE, lab = TRUE)
# Корреляционный анализ (немного другая матрица внешне)
correlation_matrix <- cor(df[, c("Объем_рынка_млрд", "Налоговая_ставка", "Налоговый_вычет", "Субсидии", "Кластеры", "Маркировка")])
corrplot(correlation_matrix, , method = "circle")




# Некоторые переменные имеют выбросы

# Построим ящики с усами для подозрительных переменных 
par(mfrow = c(1, 3))

boxplot(df$Объем_рынка_млрд, main='Объем_рынка_млрд',col='Sky Blue')

boxplot(df$Налоговая_ставка, main='Налоговая_ставка',col='Sky Blue')
boxplot(df$Налоговый_вычет, main='Налоговый_вычет',col='Sky Blue')
boxplot(df$Субсидии, main='Субсидии',col='Sky Blue')
boxplot(df$Кластеры, main='Кластеры',col='Sky Blue')
boxplot(df$Маркировка, main='Маркировка',col='Sky Blue')

dim(df)
quartiles <- quantile(df$Налоговая_ставка, probs=c(.25, .75), na.rm = FALSE)
IQR <- IQR(df$Налоговая_ставка)
Lower <- quartiles[1] - 1.5*IQR
Upper <- quartiles[2] + 1.5*IQR 
df <- subset(df, df$Налоговая_ставка > Lower & df$Налоговая_ставка < Upper)
dim(df)



ols_model <- lm(Объем_рынка_млрд ~ Налоговая_ставка + Налоговый_вычет + Субсидии + Кластеры + Маркировка, data = df)
summary(ols_model)



library(readr)
library(dplyr)
library(plm)
library(lmtest)
library(car)




fe3 <- plm(Объем_рынка_млрд ~ Налоговая_ставка + Налоговый_вычет + Субсидии + Кластеры + Маркировка, data = df, index = c("Страна","Год")) 
summary(fe3)


# Random Effects
re_model <- plm(Объем_рынка_млрд ~ Налоговая_ставка + Налоговый_вычет + Субсидии + Кластеры + Маркировка, 
                data = df, model = "random")
summary(re_model)



phtest(fe3, re_model)
pbgtest(fe_model)
bptest(ols_model)





plmtest(fe3, effect = "individual")

# Lagrange Multiplier test
plmtest(re, type = "bp")








library(plm)

# Убедимся, что df — pdata.frame
df_p <- pdata.frame(df, index = c("Страна", "Год"))

# Модель с индивидуальными и временными эффектами
fe_tw <- plm(
  Объем_рынка_млрд ~ Налоговая_ставка + Налоговый_вычет + Субсидии + Кластеры + Маркировка,
  data = df_p,
  model = "within",
  effect = "twoways"
)

x2025 <- c(
  Налоговая_ставка = 0.03,
  Налоговый_вычет = 0,
  Субсидии = 20,
  Кластеры = 1,
  Маркировка = 1
)

x2026 <- c(
  Налоговая_ставка = 0.1,
  Налоговый_вычет = 0.3,
  Субсидии = 25,
  Кластеры = 1,
  Маркировка = 1
)

x2027 <- c(
  Налоговая_ставка = 0.1,
  Налоговый_вычет = 0.3,
  Субсидии = 30,
  Кластеры = 1,
  Маркировка = 1
)

x2028 <- c(
  Налоговая_ставка = 0.15,
  Налоговый_вычет = 0.3,
  Субсидии = 35,
  Кластеры = 1,
  Маркировка = 1
)


x2029 <- c(
  Налоговая_ставка = 0.2,
  Налоговый_вычет = 0.3,
  Субсидии = 40,
  Кластеры = 1,
  Маркировка = 1
)


x2030 <- c(
  Налоговая_ставка = 0.2,
  Налоговый_вычет = 0.3,
  Субсидии = 45,
  Кластеры = 1,
  Маркировка = 1
)

predict_future2 <- function(model, x, country, year, safe = TRUE) {
  coefs <- coef(model)
  
  # Проверим имена
  if (!all(names(coefs) %in% names(x))) {
    stop("Ошибка: не все признаки найдены в x")
  }
  
  # Индивидуальный эффект
  fx_country <- fixef(model, effect = "individual")[country]
  if (is.na(fx_country) && safe) {
    fx_country <- median(fixef(model, effect = "individual"), na.rm = TRUE)
  }
  
  # Временной эффект
  time_effects <- tryCatch(fixef(model, effect = "time"), error = function(e) NULL)
  if (!is.null(time_effects)) {
    fx_time <- time_effects[as.character(year)]
    if (is.na(fx_time) && safe) {
      fx_time <- mean(time_effects, na.rm = TRUE)
    }
  } else {
    fx_time <- 0
  }
  
  y_hat <- sum(x[names(coefs)] * coefs) + fx_country + fx_time
  return(y_hat)
}
predict_future2(fe_tw, x2030, "Россия", 2030)

#Налог на прибыль
#доход_от_прибыли = прибыльность_отрасли × объем_рынка × ставка_налога
0.15 * 5.413582  * 0.20 = 0.162 


#средняя_зарплата = 120 тыс ₽ × 12 = 1.44 млн ₽ в год
#работников = 15 000
#фонд_оплаты = 1.44 × 15 000 = 21.6 млрд ₽
#взносы = фонд_оплаты × 30% ≈ 6.48 млрд ₽


#расходы = 0.050 + 0.016 = 0.066 млрд ₽



#