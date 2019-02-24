setwd("D:/Pedro/Master MBIT/Temario/Modulo5/Sesion 52, 54, 55, 56 y 64 - Estadistica Avanzada - Ramon Carrasco")
rm(list = ls())

install.packages("PerformanceAnalytics")
library("PerformanceAnalytics")

install.packages("forecast")
library("forecast")

install.packages("tseries")
library("tseries")


# METODO BOX-JENKINGS:

# Lectura temporal
i_data<-read.table("Tractor-Sales.csv", header = TRUE, sep=",")
head(i_data)
str(i_data)
summary(i_data)
# Tenemos 144 obs y 144 level, deberiamos revsar que tenemos datos para todos los meses

# Histograma
hist(i_data$Number.of.Tractor.Sold)

# Crear time serie
ts_data<-ts(i_data$Number.of.Tractor.Sold,start = c(2003,1),frequency = 12)
ts_data

# La pintamos
plot(ts_data,xlab="años",ylab="ventas tractores")
plot(decompose(ts_data))
plot(decompose(ts_data, type = "multiplicative"))
# random feo....
# Se ve clara la tendencia, y varianza desigual en la estacionalidad

# Quitamos herocedestidad
ts_log_data <- log10(ts_data)
plot(ts_log_data,xlab="años",ylab="log ventas tractores")

# Quitamos tendencia:
ts_dif_log_data <- diff(ts_log_data)
plot(ts_dif_log_data,xlab="años",ylab="dif ventas tractores")
# Es suficiente con d=1. Porque la tendencia es constante
# para hacer d=2 --> diff(ts_log_data,2)

# IMP: Se ve que la media sera 0 y la varianza parece 1
mean(ts_dif_log_data)
sd(ts_dif_log_data)

# CONTRASTES:
# H0: NO es estacionaria // H1: es estacionaria 
adf.test(ts_dif_log_data) # 0.01 --> Se rechaza H0 --> Es estacionaria
# Ya podriamos utilizar al menos una AR(p), para utilizar MA(q) o ARMA(p,q) o ARIMA(p,d,q) tenemos que probar mas cosas

# Vamos a intentar verlo a ojo
par(mfrow = c(1,2))
acf(as.numeric(ts_dif_log_data), lag.max = 144,main="ACF dif-log-ventas tractores")
pacf(as.numeric(ts_dif_log_data), lag.max = 144,main="PACF dif-log-ventas tractores")

# Hacemos estimacion ARIMA:
# OJO aqui podemos meter la ts sin tratamiento de diferencias. Pero el log si es obligatorio
require("forecast")
mod_arima <- auto.arima(ts_log_data, approximation=FALSE,trace=TRUE)
summary(mod_arima)

# CONCLUSION:
# Mejor modelo: ARIMA(0,1,1) y SARIMA (0,1,1)[12] 

# Analizamos los residuos:
par(mfrow=c(1,1))
qqnorm(mod_arima$residuals)
qqline(mod_arima$residuals)
mean(mod_arima$residuals)
sd(mod_arima$residuals)
par(mfrow=c(1,2))
acf(as.numeric(mod_arima$residuals), lag.max = 144,main="ACF residuos")
pacf(as.numeric(mod_arima$residuals), lag.max = 144,main="PACF residuos")
par(mfrow=c(1,1))
# Los residuos no salen del intervalo --> BUENA PINTA

# Chequeamos los residuos para asegurar:
# H0: Residuos se distribuyen de forma independiente
Box.test(mod_arima$residuals) # p=0.8985 > 0.05 --> Se distribuyen de forma independiente

# H0: Residuos normales:
shapiro.test(mod_arima$residuals) # p=0.15 > 0.05 --> Se distribuyen como normal

# H0: Residuos normales tambien se puede hacer asi:
jarque.bera.test(mod_arima$residuals) # p=0.05892 > 0.05 --> Se distribuyen como normal, muy justo


# Predicción
# Predecimos 36 meses
pred_log <- predict(mod_arima, n.ahead=36)
str(pred_log) # Datos estimados sin transformar
pred <- pred_log
pred$pred <- 10^(pred_log$pred)

# El intervalo de confianza siempre es +-SE
pred$inf<-10^(pred_log$pred-2*pred_log$se)
pred$sup<-10^(pred_log$pred+2*pred_log$se) 

#Visualización
plot(ts_data,xlim=c(2004,2018),ylim=c(1,1600),xlab = "Años",ylab = "Ventas Tractores")

lines(pred$pred,col="blue")
lines(pred$sup,col="green")
lines(pred$inf,col="red")

# Se puede hace con forecast
pred_log <- forecast(mod_arima, h=36)
pred <- pred_log
pred$pred <- 10^(pred_log$mean)
pred$low <- ts(10^(pred$lower[,2]), start=c(2015), frequency=12)
pred$upp <- ts(10^(pred$upper[,2]), start=c(2015), frequency=12)


plot(ts_data,xlim=c(2004,2018),ylim=c(1,1600),xlab = "Años",ylab = "Ventas Tractores")
lines(pred$pred,col="blue")
lines(pred$low,col="green")
lines(pred$upp,col="red")
