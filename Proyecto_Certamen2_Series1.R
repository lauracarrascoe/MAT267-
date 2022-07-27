library(fpp2)
library(tidyverse)
library(ggplot2)
library(MASS)
library(forecast)
library(tstools)
library(Metrics)

gold_data <- read.csv("C:/Users/Laura/Downloads/gold_price_data.csv")

#Truncamos la serie (1 al 37 eliminar)
gpd_ <- gold_data[-c(1:37),]
gpd <- ts(gpd_$Value, start = 1979, frequency = 252)


#Grafica de serie de tiempo

plot(gpd, main = "Valor del precio del Oro", xlab = "Tiempo",
     ylab = "[USD]")

#Descomposición de la Serie
gpd.comp <- decompose(gpd, type = "multiplicative")
plot(gpd.comp)

#Definición de conjuntos de entrenamiento y testing
cc <- round(length(gpd_$Value)*0.98)
cc2 <-length(gpd_$Value)
gpd.train <- head(gpd, cc)
gpd.test <- tail(gpd, (cc2-cc))

##HOLTWINTERS
gpd.hw<-HoltWinters(gpd.train, seasonal = "multiplicative")
predhw<- as.numeric(gpd.hw$mean)
p.hw<-predict(gpd.hw, n.ahead=(cc2-cc))
plot(gpd, ylim= c(35,2500), main="Predicción Holt Winters", ylab = "Precio del Oro [USD]", xlab = "Tiempo") 
lines(p.hw, col="red")


###ACF y PACF

#par(mfrow = c(3,2))

#Serie sin diferenciar
acf(gpd, main= "Serie original")
pacf(gpd, main= "Serie original")

#Serie diferenciada
acf(diff(gpd), main= "Serie diferenciada")
pacf(diff(gpd), main= "Serie diferenciada")

#Serie doblemente diferenciada
acf(diff(diff(gpd)), main= "Serie doblemente diferenciada")
pacf(diff(diff(gpd)), main= "Serie doblemente diferenciada")

#ARIMA(3,2,1) 
fit_arima<-arima(gpd.train, c(3, 2, 1))
p.arima<-predict(fit_arima, n.ahead=(cc2-cc))
plot(gpd, ylim= c(35,2000), main="Predicción ARIMA (3,2,1)", ylab = "Precio del Oro [USD]", xlab = "Tiempo") 
lines(p.arima$pred, col="red")

#FUNCION AUTOARIMA Y ARIMA(0,1,0)
auto.arima(gpd)
fit_arima<-arima(gpd.train, c(0, 1, 0))
p.arimaauto<-predict(fit_arima, n.ahead=(cc2-cc))
plot(gpd, ylim= c(35,2000), main="Predicción ARIMA (0,1,0)", ylab = "Precio del Oro [USD]", xlab = "Tiempo") 
lines(p.arimaauto$pred, col="red")

#FUNCION STLM
stlm.train<-stlm(gpd.train)
fit_stlm<-stlm.train$modelfunction
p.stlm<-forecast(fit_stlm(gpd.train), h=(cc2-cc))
plot(gpd, ylim= c(35,2000), main="Predicción STLM", ylab = "Precio del Oro [USD]", xlab = "Tiempo") 
lines(p.stlm$mean, col="red")

#CÁLCULO RMSE AJUSTES
rmse(gpd.test, p.hw)#Holt-Winters
rmse(gpd.test, p.arima$pred) #arima(3,2,1)
rmse(gpd.test, p.arimaauto$pred)#arima(0,1,0)
rmse(gpd.test, p.stlm$mean)#STLM

#rmse menor: ARIMA(3,2,1)
#PREDICCIÓN APROXIMADAMENTE UN AÑO A FUTURO
fit_fut<-arima(gpd, c(3, 2, 1))
p.fut<-predict(fit_fut, n.ahead=(cc2-cc))
plot(gpd, ylim= c(35,2000), main="Predicción a futuro ARIMA (3,2,1)", ylab = "Precio del Oro [USD]", xlab = "Tiempo") 
lines(p.fut$pred, col="green")

#DIAGNOSTICO DEL MODELO
checkresiduals(fit_fut)