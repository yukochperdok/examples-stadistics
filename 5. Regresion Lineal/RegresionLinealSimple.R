# Limpiar el workspace
rm(list = ls(all = TRUE))

# En un determinado estudio médico se pretende medir la relación existente entre la
# exposición al ruido y la hipertensión. Los siguientes datos han sido extraídos del Jourrnal
# of Sound and Vibration: 
# Y 1 0 1 2 5 1 4 6 2 3 5 4 6 8 4 5 7 9 7 6
# X 60 63 65 70 70 70 80 80 80 80 85 89 90 90 90 90  94 100 100 100

# Donde X representa la presión sonora en dB, e Y el aumento de la presión sanguínea en mmHg.
# 1) Realizar un diagrama de dispersión de Y frente a X.
# 2) Realizar el modelo de regresión lineal simple. 
###########################

Y<-c(1,0,1,2,5,1,4,6,2,3,5,4,6,8,4,5,7,9,7,6)
X<-c(60,63,65,70,70,70,80,80,80,80,85,89,90,90,90,90,94,100,100,100)


#diagrama de dispersión
qqplot(X,Y)

#regresión
rg<-lm(Y ~ X)
summary(rg)

#recta de regresión
qqplot(X,Y)
abline(rg)

#coeficiente de correlación alto
cor(X,Y)

#############################################################################################
# Se está estudiando la relación entre el número de años que una persona está afiliada al
# sindicato y el nivel de satisfacción con la actuación de dicho sindicato. Para ello se parte de
# los datos de 7 individuos tomados aleatoriamente de personas adscritas a partidos
# políticos, obteniéndose:
#   Años          8 7 10 3 6 13 4
#   Satisfacción  7 5 8 5 9 9 3
# 1. Calcular el coeficiente de correlación lineal. Comentar el resultado obtenido.
# 2. Predecir el índice de satisfacción de una persona que lleva 11 años militando al
# sindicato. Conociendo que el índice de satisfacción es de 6 predecir los años que
# lleva en el sindicato.

####### Variables
x<-c(8,7,10,3,6,13,4)
y<-c(7,5,8,5,9,9,3)

# 1. Calcular el coeficiente de correlación lineal. Comentar el resultado obtenido.
xy<-cor(x,y,method = c("pearson"))
xy
# Entre dos unas variables no existe correlacion parcial, puesto que no se ven afectadas por otras.
# Lo anterior es lo mismo que 
cor(x,y)
# y equivalente a:
cov(x,y)
# Existen tambien otros metodos (kendall, spearman), pero el mas usado es pearson



# 2. Predecir el índice de satisfacción de una persona que lleva 11 años militando al
# sindicato. Conociendo que el índice de satisfacción es de 6 predecir los años que
# lleva en el sindicato.

# Si sabemos que tienen una alta correlacion lineal planteamos una regresion de X sobre y,
# o de y sobre x. Observar que las regresiones son diferentes aunque la incertidumbre (p-values)
# es la misma.

rg1<-lm(y ~ x)
summary(rg1)

# En una lm podemos ver:
# 1. Residuos:
#      1      2      3      4      5      6      7 
#   0.090 -1.436  0.142  0.460  3.038 -0.280 -2.014
# Para cada valor de la var.dependiente (y en este caso) la diferencia con la recta de regresion
# Vemos que el valor con mas error se situa en el valor 5: 6 años - 9 satisfaccion
# Nos interesa que la varianza del error no sea alta. Tambien se indica abajo: 
#  Residual standard error: 1.77 on 5 degrees of freedom
# En este caso es relativamente alta.

# 2. Coeficientes de la regresion:
#             Estimate Std. Error t value Pr(>|t|)  
# (Intercept)   3.1180     1.6661   1.871   0.1202  
# x             0.4740     0.2094   2.263   0.0731 .
#---
#  Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
#
# Aqui te estan indicando que y=3.1180+0.4740*x
# A partir de x calculas los estimados de y. 
# Los p-value te indican la incertidumbre de ese coeficiente. Cuanto menor pvalue
# menos incertidumbre. Para p-value menor que 0.05 tenemos que su coeficiente no es fiable.
# En este caso ambos son relatibamente fiables=> beta0 (intercepto), beta1 (coef de x)

# 3. Bondad de ajuste para Regresion, coef determinacion:
#
# Multiple R-squared:  0.506,	Adjusted R-squared:  0.4072 
# F-statistic: 5.122 on 1 and 5 DF,  p-value: 0.07306
# 
# Para regresion Lineal simple nos vale el R-squared=0.506 (50%), es decir, MAL modelo
# El Adjusted R-squared vale para la reg.lineal multiple
# Y el estadistico F y p-value finales vale para la reg.lineal multiple.
# Cuanto mayor sea este mejor, y sera mas pequeño p-value mejor. Nos indicara que
# no esiste una correlacion lineal entre alguna combinacion de variables del modelo (Bondad de ajuste sobre el modelo)



# Para predecir los valores en un modelo existen 2 opciones:
# 1. Manualmente
#para 11 años
xx<-11
y1<-0.4740*xx+3.1180
y1


# 2. Metodo predict(regresion, data.frame de valores a predecir)
# Vamos a predecir la satisfaccion para 11,12 y 5 años (nunca fuera del rango de valores)
# No vale para x=1000 por ejemplo

x.estimado<-c(11,12,5)
# No olvidar que newdata tiene que ser un dataframe con columna llamada x (que coincida con el nombre del coeficiente)
# Si tuvieramos 2 coeficientes=> dos columnas en el dataframe.
y.estimado <- predict(lm(y ~ x),newdata=data.frame(x=x.estimado), se.fit = FALSE)
y.estimado
#     1     2     3 
# 8.332 8.806 5.488
# Para 11 años tenemos un indice de satisfacion de 8.332
# Para 12 años 8.806
# Para 5 años 5.488

# Lo bueno de la funcion predict es:
# 1. Te permite predecir un array de valores (para cada coeficiente)
# 2. Te premite predecir la curva de confianza
predict(lm(y ~ x),newdata=data.frame(x=x.estimado), se.fit = FALSE, interval="confidence",level = 0.01)
# lwr y upr son los intervalos de confianza a un 99% de confianza
# La confianza que tienes sobre los coeficientes de la recta: beta0, beta1,etc...

# 3. Te premite predecir la curva de prediccion
predict(lm(y ~ x),newdata=data.frame(x=x.estimado), se.fit = FALSE, interval="prediction", level = 0.05)
# lwr y upr son los intervalos de prediccion a un 95% de confianza.
# La confianza que tienes sobre la estimacion de los valores y.estimada


# Para predecir la x (años de permanencia) partiendo del indice de satisfaccion tenemos 
# que plantear la regresion al reves x~y


rg2<-lm(x ~ y)
summary(rg2)

# predecir satisfacción en 6 años
y6<-6
x6<-1.0676*y6+0.2703
x6 #6.6759

# o bien:
predict(lm(x ~ y), newdata = data.frame(y=c(6)), se.fit = FALSE, interval = "prediction", level = 0.05)
# 6.675676 con intervalo al 95% [6.487645,6.863707]



# Se puede ver como se dispersan los residuos con respecto a los estimados de y:
y.estimado<-predict(lm(x ~ y), newdata = data.frame(y=y), se.fit = FALSE)
plot(y.estimado,lm(x ~ y)$residuals)
abline(h=0)
# Los residuos siembre deben estar alrededor del 0 y su media tiende a 0.
mean(lm(x ~ y)$residuals) # -7.930939e-17
sd(lm(x ~ y)$residuals) # Error estandar


#############################################################################################
# Dados los siguientes conjuntos de datos:
# U 1 2 3 4 5 6 7 8 9 10
# V 3 5 6 5 7 9 10 9 10 10
# W 4.543 4.543 4.543 4.543 4.543 4.543 4.543 4.543 4.543 14.117
# X 6.646 6.646 6 6 6 7 7 5.684 8.838 14.186
#1. Dibujar el diagrama de dispersión de cada uno de los conjuntos de datos.
#2. Calcular la recta de regresión de cada uno de los conjuntos de datos y dibujarla en
#el diagrama de dispersión, considerando como variables independientes las
#variables U y X.
# 3. Calcular el coeficiente de correlación lineal para cada uno de los conjuntos.
# 4. ¿Qué podemos observar?
#5. Eliminando los outliers vuelve a calcular los apartados 2 y 3.
# ¿Qué conclusiones podemos extraer de este problema? 

# Es decir:
# V=a1*U+b1
# W=a2*X+b2
# Por separado

############ variables
U<-c(1,2,3,4,5,6,7,8,9,10)
V<-c(3,5,6,5,7,9,10,9,10,10)
W<-c(4.543,4.543,4.543,4.543,4.543,4.543,4.543,4.543,4.543,14.117)
X<-c(6.646,6.646,6,6,6,7,7,5.684,8.838,14.186)

#1. Dibujar el diagrama de dispersión de cada uno de los conjuntos de datos.
# diagrama dispersión
qqplot(U,V)

qqplot(X,W)


#2. Calcular la recta de regresión de cada uno de los conjuntos de datos y dibujarla en
#el diagrama de dispersión, considerando como variables independientes las
#variables U y X.
re1<-lm(V ~ U)
summary(re1)
# diagrama dispersión
qqplot(U,V)
abline(re1)


re2<-lm(W ~ X)
summary(re2)
# diagrama dispersión
qqplot(X,W)
abline(re2)


# 3. Calcular el coeficiente de correlación lineal para cada uno de los conjuntos.
UV<-cor.test(U,V,method = c("pearson"))
WX<-cor.test(W,X,method = c("pearson"))
UV  # 0.93
WX  #0.93

# 4. ¿Qué podemos observar?
# Las rectas son iguales

#5. Eliminando los outliers vuelve a calcular los apartados 2 y 3.
boxplot(W)
boxplot(X)

#eliminado el punto (W=14.117,x=14.186)
WW<-c(4.543,4.543,4.543,4.543,4.543,4.543,4.543,4.543,4.543)
XX<-c(6.646,6.646,6,6,6,7,7,5.684,8.838)

re3<-lm(WW ~ XX)
summary(re3)
# Importante:
# Te avisa de una asociacion lineal nula: 
# Multiple R-squared:    NaN,	Adjusted R-squared:    NaN 
# In summary.lm(re3) : essentially perfect fit: summary may be unreliable
# Si tenemos los puntos encima de la recta, tenemos una sobreestimacion=> El modelo no es bueno


# diagrama dispersión
qqplot(XX,WW)
abline(re3)  #asociación lineal nula

# ¿Qué conclusiones podemos extraer de este problema? 
# Es importante visualizar el diagrama de dispersión
# Un modelo de regresion que ajusta perfectamente a una linea no es buen estimador.

