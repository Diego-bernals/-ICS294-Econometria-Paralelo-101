#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#      Econometria 2-2025
#  
#     Series de Tiempo(Forecast)
#     
#   Ayudante: Diego Bernal Soto
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

library(readxl)
library(Metrics)
library(caret)
library(lars)
library(dplyr)
library(ggplot2)
library(corrplot)
library(sf)
library(sp)
library(geosphere)
library(ggmap)
library(xgboost)
library(plotly)
library(forecast)
library(quantmod)
library(stargazer)
library(scales)
library(mFilter)
library(ggmap)
library(readr) # CSV file I/O, e$g$ the read_csv function
library(stringr)
library(sp)
library(RColorBrewer)
library(openxlsx)
library(data.table)
library(tseries)

Des <- read_excel("./Desempleo.xlsx", 
                        sheet = "Data")


#Analisis

desem <- ts(Des$tasa,frequency = 12,start = c(2010,01))

autoplot(desem) +
  ylab("Tasa de Desempleo") +
  xlab("Años") +
  labs(title = "Tasa de desempleo en Chile") +
  geom_line(color = "steelblue")+
  geom_point(color = "black", size = 0.5) 



autoplot(desem)+ ylab("Tasa de Desempleo") +xlab("Años") + labs(title="Tasa de desempleo en Chile")

dec <- decompose(desem)
autoplot(dec)

adf.test(desem)


Cer <- stl(log(desem), s.window="periodic")
ap.sa <- exp(seasadj(Cer))
autoplot(cbind(desem, SeasonallyAdjusted=ap.sa),main = "Serie desestacionalizada de desempleo") +
  xlab("Fecha") + ylab("Tasa de desempleo") 

#Testing stationary
adf.test(log(desem))

#~~~~~~~~~~~~~~~~~~~~~~Forecast~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#Suavizamiento exponencial(basados en la descripci?n de la tendencia y estacionalidad)

fc <- ses(desem, h=12)
round(accuracy(fc),2)

fc$model
#Como podemos observar el alpha es mas cercano a 1 por lo que se le esta dando un mayor enfasis a el valor mas reciente
#

#Grafico
autoplot(fc) +
  autolayer(fitted(fc), series="Fitted") +
  ylab("Tasa de desempleo") + xlab("Year")

#Holt simple

hol <- HoltWinters(desem,beta = FALSE, gamma = FALSE)
hol

#Grafico

autoplot(hol$fitted, main = "Holt-Winters suavizamiento exponencial para desempleo")+ autolayer(desem,series = "Desempleo")

#ETS

ets <- ets(desem)
ets

autoplot(ets,main="ETS suavizamiento eponencial de desempleo")

#Moving avarage

model_ma <- ma(desem, order = 5 , centre = FALSE) #k=5

model_ma  

autoplot(desem,series = "Desempleo") + autolayer(model_ma, series="Fitted") +
  ylab("Tasa de Desempleo") + xlab("Tiempo")

fc_ma <- predict(model_ma,h=12)

autoplot(fc_ma,series="Desempleo") + autolayer(fitted(fc_ma), series="fitted")
+ ylab("Tasa de desempleo")


masq <- rollmean(desem,k=3, fill = NA, align = "right")
masq

masse <- rollmean(desem,k=6, fill = NA, align = "right")
masse

masa <- rollmean(desem,k=12, fill = NA, align = "right")
masa


#Grafico de comparacion de modelos de moving avarage
autoplot(desem, series = "Original")+ 
  autolayer(masq, series = "Desem quarter") +
  autolayer(masse,series = "Desem semester") +
  autolayer(masa, series = "Desem anual") +
  labs(title= "Comparaci?n SMA modelos") +
  ylab("Tasa de desempleo %") +
  xlab("Meses")

#La tasa de desempleo actualmente va a la baja la tendencia, suavizamos la informaci?n

acf(desem, lag.max = 20)

mean(desem)
sd(desem)

MA1 <- arima(desem,order = c(0,0,1))
MA1

predict(MA1,n.ahead = 12)

MA_forecast <- predict(MA1,n.ahead = 12)$pred
MA_forecast_se <- predict(MA1,n.ahead = 12)$se


#Grafico con intervalos de confianza de la prediccion
plot(desem)
points(MA_forecast,type = "l", col=2)
points(MA_forecast-2*MA_forecast_se,type="l",col = 2, lty=2)
points(MA_forecast+2*MA_forecast_se,type="l",col = 2, lty=2)




#Modelo de autoregresion  AR(1)

pacf(desem)

ar1 <- arima(desem,order = c(1,0,0))
ar1

AR_fit <- desem - residuals(ar1)

points(AR_fit, type = "l", col = 2, lty = 2)

#Forecast

predict_ar1 <- predict(ar1, n.head = 5)$pred
predict_ar1se <- predict(ar1, n.head = 5)$se
plot(desem)
points(predict_ar1, type = "l", col = 2)
points(predict_ar1 - 2*predict_ar1se, type = "l", col = 2, lty = 2)
points(predict_ar1 + 2*predict_ar1se, type = "l", col = 2, lty = 2)


# ARIMA (Enfocados en la descripcion de la autocorrelaci?n)
#split the data
library(TSstudio)

split_desem <- ts_split(desem, sample.out = 12)

training <- split_desem$train
test <- split_desem$test

arima_diag(training)


#Lag of 3 AR, 



