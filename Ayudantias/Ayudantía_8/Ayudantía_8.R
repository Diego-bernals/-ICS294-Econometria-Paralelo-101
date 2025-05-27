library(tidyverse)
library(tidymodels)
library(MASS)
library(readxl)
library(writexl)
library(rlang)
library(ggfortify)
library(tseries)
library(forecast)
library(tswge)
library(car)
library(tseries)
library(quantmod)
library(zoo)
library(stargazer)


df <- read_excel("datos_energía.xlsx", 
                            sheet = "Hoja1")

df$energia_gwh <- df$energia_kwh / 1e6

glimpse(df)

df$Mes <- factor(df$Mes,levels = c(1,2,3,4,5,6,7,8,9,10,11,12),labels = c('Enero','Febrero','Marzo','Abril','Mayo',
                                                                            'Junio','Julio','Agosto','Septiembre',
                                                                            'Octubre','Noviembre','Diciembre'))

val_ts <- ts(df$energia_gwh,start = c('2015'),frequency = 12)
autoplot(val_ts)
df$tiempo <- time(val_ts)

reg_1 <- lm(energia_gwh~Imac, data = df)
reg_2 <- lm(val_ts ~ time(val_ts))
reg_3 <- lm(energia_gwh~Imac + tiempo,data = df)

reg_1_ts <- ts(reg_1$fitted.values,start = 2015,frequency = 12)
reg_2_ts <- ts(reg_2$fitted.values,start = 2015,frequency = 12)
reg_3_ts <- ts(reg_3$fitted.values,start = 2015,frequency = 12)



autoplot(val_ts, series = "Consumo Energetico GWh",col ="black")+ autolayer(reg_1_ts, series = "Regresión con IMACEC") + autolayer(reg_2_ts,
                                                                                                                                   series = "Regresión con el Tiempo")+ 
  ggtitle("Consumo de Energía Vallenar 2015-2022 \n Cliente regulado") +xlab("Tiempo") + ylab("GWh") + autolayer(reg_3_ts, series = "Regresión tiempo y Imacec") 


stargazer(reg_1,reg_2,reg_3,type = 'latex',align = TRUE)

