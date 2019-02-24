setwd("D:/Pedro/Master MBIT/Temario/Modulo5/Sesion 52, 54, 55, 56 y 64 - Estadistica Avanzada - Ramon Carrasco")
rm(list = ls())

install.packages("PerformanceAnalytics")
library("PerformanceAnalytics")

install.packages("forecast")
library("forecast")

# Lectura temporal vuelos USA
i_st_air<-read.table("ST-AIR.csv", header = TRUE, sep=";")
ls(i_st_air)
str(i_st_air)
class(i_st_air)
typeof(i_st_air)

# En series temporales hay que comprobar que no hay huecos de tiempo.
# vemos que hay en Date 229 obs y 229 levels...no tiene mala pinta. Pero habria que comprobarlo

head(i_st_air)
summary(i_st_air)

#Graficamos
plot(i_st_air, xlab="fecha",ylab="pasajeros")
# No vemos mucho...

# Convertimos a serie temporal
ts_air <- ts(i_st_air$Total,start = c(1996,1), frequency = 12)
head(ts_air)
str(ts_air)

plot(ts_air, xlab="fecha",ylab="pasajeros")

# Vemos algunas cosas de la serie temporal
start(ts_air)
frequency(ts_air)
summary(ts_air)

# Acortamos tiempo de ventana:
ts_air2001<-window(ts_air,start=c(2001,1),end=c(2001,12))
plot(ts_air2001, xlab="fecha2001",ylab="pasajeros2001")

# Algunas operaciones
ts_air_ag <- aggregate(ts_air)
ts_air_cy <- cycle(ts_air)

# Miramos normalidad
qqnorm(ts_air)
qqline(ts_air)

# Descomposicion de una serie aditiva
ts_air_desc<-decompose(ts_air)
str(ts_air_desc)
plot(ts_air_desc)

# Hay que revisar que el error (ramdom) no tiene ciclos. Mediante un constaste de independencia, un Durbin-Watson, etc...

# Podemos probar una Serie temporal multiplicativa:
ts_air_desc_m<-decompose(ts_air,type = "multiplicative")
str(ts_air_desc_m)
plot(ts_air_desc_m)

# Comparar y ver cuya media de ramdom sea mas cercano a 0
summary(ts_air_desc$random)
summary(ts_air_desc_m$random) #La media mas crecana es la multiplicativa


# Correlograma
acf(ts_air)
# Correlacion de xt con xt, con xt-1, con xt-2, etc...
# Obviamente con quien mas correlacion tiene es con sigo mismo luego va perdiendo
# Y luego vuelve a ir ganado

# Acotamos lag
acf(ts_air,lag.max = 50)


# Correlograma parcial
pacf(ts_air)
# OJO Aqui ya no hay correlacion parcial con sigo mismo
# 11 correlaciones hasta 1.

# Identificar Ruido blanco:
# El ruido blanco siempre sigue la normal, porque es N(0,1)
rw<-rnorm(365)
plot(rw,type="l")
mean(rw)# Tiene que aproximarse a 0
sd(rw)# TIene que aproximarse a 1

acf(rw)
pacf(rw)
# Practicamente todas no salen del intervalo, no se relacionan => ruido blanco


# Creamos caminos aleatorios:
rw<-rnorm(365)
ca <- rw[1]
for(t in 2:365) ca[t] <- ca[t-1] + rw[t]
plot(ca, type = "l")

# Vemos la correlacion
layout(1:2)
# Del ruido
acf(rw,lag.max = 40)
# Del camino
acf(ca,lag.max = 40)

# Y correlacion parcial
layout(1:2)
# Del ruido
pacf(rw,lag.max = 40)
# Del camino
pacf(ca,lag.max = 40)
layout(1:1)
# ca solo esta correlado parcialmente con el anterior.
# A esto se denomina camino aleatorio


# Volvemos al caso anterior ts_air y calculamos correlaciones
acf(na.omit(ts_air_desc$random))
pacf(na.omit(ts_air_desc$random))
# Sacamos algunos valores mas:
acf(na.omit(ts_air_desc$random),lag.max = 300)
pacf(na.omit(ts_air_desc$random),lag.max = 300)
# Vemos que no es ruido blanco, y hay una regularidad y de hecho tenemos correlacion en 
# los valores proximos
# IMP: Lo que nos interesa es que el random se parezca al ruido blanco--> que no haya correlacion.

# Vemos tendencia y estacionalidad
acf(na.omit(ts_air_desc$trend),lag.max = 300)
pacf(na.omit(ts_air_desc$trend),lag.max = 300)
acf(na.omit(ts_air_desc$seasonal),lag.max = 300)
pacf(na.omit(ts_air_desc$seasonal),lag.max = 300)



# MODELO BOX-JENKINGS:
head(i_st_air)
plot(i_st_air, xlab="fecha",ylab="pasajeros")
ts_air <- ts(i_st_air$Total,start = c(1996,1), frequency = 12)
head(ts_air)
str(ts_air)
plot(ts_air, xlab="fecha",ylab="pasajeros")

# HAY QUE CONTINUAR