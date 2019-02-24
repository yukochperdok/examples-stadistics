# ************************************************************************************
# Preparacion entorno
# ************************************************************************************

# se elimina todo lo existente
rm(list=ls())

# Función para verificar si un paquete está instalado o no
is.installed <- function(paquete) is.element(
  paquete, installed.packages())

if(!is.installed('forecast'))  
  install.packages("forecast")
library("forecast")

# Preparacion entorno ejemplos
setwd("Z:/dataExamples")

# -----------------------------------------------------------
# Metodología de Box-Jenkins. Ejemplo con R
# -----------------------------------------------------------
# Obtener la serie temporal
i_data<-read.table("ST-AIR.csv",header=TRUE, sep = ";")

# Primeras inspecciones de datos
str(i_data)
head(i_data)

# Conversión a serie de tiempo
ts_data <- ts(i_data$Total,start=c(1996,1),frequency=12)


# Representación gráfica de la serie
plot(ts_data,xlab="años",ylab="n pasajeros")

# Eliminar heterocedasticidad
ts_log_data <- log10(ts_data)
plot(ts_log_data,xlab="años",ylab="log n pasajeros")

# Eliminar tendencia (d=1)
ts_dif_log_data <- diff(ts_log_data)
plot(ts_dif_log_data,xlab="años",ylab="dif n pasajeros")
#plot(diff(ts_data,differences=2),xlab="años",ylab="dif n pasajeros")

# Identificación preliminar del modelo
par(mfrow = c(1,2))
acf(as.numeric(ts_dif_log_data),main="ACF dif-log-n pasajeros") 
pacf(as.numeric(ts_dif_log_data),main="PACF dif-log-n pasajeros")

# Estimacion modelo más adecuado
require("forecast")
mod_arima <- auto.arima(ts_log_data, approximation=FALSE,trace=FALSE)
summary(mod_arima)

# Chequeo del modelo. Se comprueban que los residulos no tengan correlaciones significativas:
qqnorm(mod_arima$residuals)
qqline(mod_arima$residuals)
mean(mod_arima$residuals)
sd(mod_arima$residuals)
par(mfrow=c(1,2))
acf(as.numeric(mod_arima$residuals),main="ACF residuos")
pacf(as.numeric(mod_arima$residuals),main="PACF residuos")

# Prueba Ljung-Box: correlaciones significativas
Box.test(mod_arima$residuals)

# Prueba normalidad residuos
shapiro.test(mod_arima$residuals)

# Prueba normalidad (asimetria y curtosis)
jarque.bera.test(mod_arima$residuals)


# Predicción
pred_log <- forecast(mod_arima, h=12)
str(pred_log)

# Datos estimación (se deshace transformación)
pred <- pred_log
pred$pred <- 10^(pred_log$mean)
lower<-ts(10^(pred_log$lower[,2]),start=c(2015,1),frequency=12)
upper<-ts(10^(pred_log$upper[,2]),start=c(2015,1),frequency=12)

# Visualización
plot(ts_data,xlim=c(1996,2016),ylim=c(40000000,110000000),xlab = "Años",ylab = "pasajeros")
lines(pred$pred,col="blue")
lines(lower,col="green")
lines(upper,col="red")

