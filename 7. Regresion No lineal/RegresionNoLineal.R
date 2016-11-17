# Limpiar el workspace
rm(list = ls(all = TRUE))


############################### EJEMPLO 1 #####################################
#A partir de las siguientes observaciones para 5 años de las variables X e Y,
#ajústese el modelo de regresión de Y en función de X más idóneo. #
#Y: producción nacional de un subsector industrial, en millones de toneladas.
#X: tiempo
#Año	X	Y
#1995	1	1,25
#1996	2	5
#1997	3	11,25
#1998	4	20
#1999	5	30,5

year<-seq(1995,1999,1)
x<-seq(1,5,1)
y<-c(1.25,5,11.25,20,30.5)

# Graficamos para ver la forma
plot(x,y)
# Parece una exponencial o potencia o algo por el estilo... cuadratica, cubica,...

##########################AJUSTE FUNCION LINEAL############################
#Ajuste de función lineal Y = a+bx

r<-lm(y~x)
summary(r) # y = -8.45 + 7.35x
# R^2=0.9671

# nube de puntos
plot(x,y)
abline(r)
# residuos
plot(predict(r),r$residuals) #alta variabilidad de los residuos
abline(0,0)

##Bondad del ajuste (varianza residual)
#Error Cuadrático Medio: suma(error^2)/N (valor entre 0 y la var(y))
sum((r$residuals)^2)/5  #3.67

# Graficamos para ver la forma
plot(x,y)
curve(r$coefficient[1]+r$coefficient[2]*x,add=T,col="red")


#####################AJUSTE FUNCION POTENCIA########################
#Ajuste de una función potencial Y = a*X^b

# Transformamos --> yy=log(y) = log(a*X^b) = log(a)+log(X^b) = log(a)+blog(X)
# Teniando como resultado:  log(y) = log(a) + blog(x)
yy=log(y)
xx=log(x)
# Sabiendo que a pasaria a transformarse en aa=log(a)
# Tendiramos una regresion lineal de esta forma
#         yy=aa+b*xx

# donde x=exp(xx)
#       y=exp(yy)
#       a=exp(aa)
#       b=b

rpot<-lm(yy~xx)
summary(rpot) #yy = 0.227672 + 1.990197*xx
# R^2=1

# Con lo cual aa=0.227672 y b=1.990197

# Convertimos aa y b, en coeficientes para regresion potencia
# a = exp(aa)
# b = b
a=exp(rpot$coefficients[1])
b=rpot$coefficients[2]
a
b

# Calculamos y:
# OPCION 1: Cogiendo los coeficientes y calculando
# y = a*X^b
yestimado=a*x^b
yestimado
y

# OPCION 2: Calculando yy y luego haciendo conversion
# yy= aa + bxx = aa + blog(x)
# y = exp(yy)
yyestimado=0.227672+1.990197*xx
yestimado2=exp(yyestimado)
yestimado2
y

##Bondad del ajuste (varianza residual)
#Error Cuadrático Medio: suma(error^2)/N (valor entre 0 y la var(y))
sum((y-yestimado)^2)/5  #0.03958911


# Graficamos para ver la forma
plot(x,y)
curve(a*x^b,add=T,col="green")

#####################AJUSTE FUNCION EXPONENCIAL########################
#Ajuste de una función exponencial Y = a*b^x

# Transformamos --> yy=log(y) = log(a*b^x) = log(a)+log(b^x) = log(a)+log(b)x
# Teniando como resultado:  log(y) = log(a) + log(b)x

yy=log(y)

# Sabiendo que a pasaria a transformarse en aa=log(a) y b en bb=log(b)
# Tendiramos una regresion lineal de esta forma
#         yy=aa+bb*x

# donde x=x
#       y=exp(yy)
#       a=exp(aa)
#       b=exp(bb)

# Regresion lineal --> yy = aa + bb x
rex<-lm(yy~x)
summary(rex)  # yy = -0.1994 + 0.7775X IMP:Intercepto no significativo
# R^2=0.9448

# Convertimos aa y bb, en coeficientes para regresion exponencial
# a = exp(aa)
# b = exp(bb)
a=exp(rex$coefficients[1])
b=exp(rex$coefficients[2])
a
b

# Calculamos y:
# OPCION 1: Cogiendo los coeficientes y calculando
# Y = a*b^x
yexestimado=a*b^x
yexestimado
y

# OPCION 2: Calculando yy y luego haciendo conversion a y
# yy= aa + bbx
# y = exp(yy)
yyexestimado=-0.1994+0.7775*x
yexestimado2=exp(yyexestimado)
yexestimado2
y

##Bondad del ajuste (varianza residual)
#Error Cuadrático Medio: suma(error^2)/N (valor entre 0 y la var(y))
sum((y-yexestimado)^2)/5 #20.38753

# Graficamos para ver la forma
plot(x,y)
curve(a*b^x,add=T,col="blue")

#IMP: El intercepto de la regresion lineal no era significativo
# si no lo tuvieramos en cuenta: yy = 0.7775X ; aa=0
a=exp(0) # =1
yexestimado=a*b^x
yexestimado
y
sum((y-yexestimado)^2)/5 #68.5184 mayor error cuadratico
# Y se ajusta algo peor, sobre todo para x=5
curve(a*b^x,add=T,col="red")


############################## CONCLUSION:#########################
#-------------------------
# Bondad del ajuste (R^2)-
#-------------------------
#F.Lineal           0.9671
#F.Potencial             1
#F.Exponencial      0.9448

#------------------------
# ECM                   -
#------------------------
#F.Lineal           3.670
#F.Potencial        0.039
#F.Exponencial     20.387


#La comparación de la bondad de modelos de regresión mediante el coeficiente de determinación
#sólo es correcta cuando la variable dependiente no ha sido sometida a transformaciones no lineales
#(por ejemplo, una transformación logarítmica). Por eso, para comparar los tres ajustes efectuados
#utilizamos el Error Cuadrático Medio. 
#El mejor ajuste resulta ser el potencial puesto que presenta el menor valor para el ECM.

# Entonces si queremos predecir el valor para el año 2000, teniendo en cuenta la regresion potencia
# Año 2000 --> x=6 , y??
x<-6
y<-1.255673*x^1.990197
y #44.41717

################################# EJEMPLO 2 ##############################################
# Se dispone de la información que aparece recogida en la siguiente tabla, relativa a la
# Renta disponible bruta (X) y al Gasto total en alimentos y bebidas no alcohólicas (Y) de los
# hogares en las distintas Comunidades Autónomas, para el año 2002:
# X e Y millones de euros
# Fuente: INE. Contabilidad Regional de España
# Encuesta Continua de Presupuestos Familiares

ccaa<-c('Andalucía','Aragón','Asturias','Baleares','Canarias','Cantabria','Castilla y León','Castilla-La Mancha',
        'Cataluña','Comunidad Valenciana','Extremadura','Galicia','La Rioja','Madrid','Murcia','Navarra',
        'País Vasco')

renta<-c(65.0,14.3,10.8,11.1,18.2,6.0,26.9,16.4,77.3,44.4,8.9,26.0,3.4,69.4,11.2,7.3,28.3)

gasto<-c(8.7,1.6,1.4,1.1,2.1,0.8,3.3,2,8.9,5.2,1.2,3.8,0.4,7.1,1.6,0.7,2.9)


# a) Determinar una función potencial que explique el gasto en alimentación a partir de la renta.

boxplot(renta,gasto)

plot(renta,gasto) # gráfico con relación lineal
# Parece relacion lineal, probamos regresion lineal

reg<-lm(gasto~renta)
summary(reg)
# R^2 al 97% lo podriamos dejar como: gasto=0.115345*renta
# El gasto es el 11% de la renta.

# Grafico
plot(renta,gasto, ylab = "Gasto en alimentación (mill. euros)", xlab = "Renta (mill. euros)")
abline(reg, col="red")

# Analizamos los residuos
plot(predict(reg),reg$residuals) 
abline(0,0)
#alta variabilidad de los residuos
# Por lo tanto intentamos con otras regresiones. Por ejemplo Potencial como nos piden


# gasto=a*renta^b
# log(gasto)=log(a*renta^b)=log(a)+blog(renta)
# trans.gasto=log(gasto)
# trans.renta=log(renta)
# aa=log(a)
# b=b

trans.gasto<-log(gasto)
trans.renta<-log(renta)
reg.pot<-lm(trans.gasto~trans.renta)
summary(reg.pot)
# Encuentro regresion lineal al 98%
# log(gasto) = -2.09889 + 0.99038 * log(renta)

# Comprobamos linealidad entre log(gasto) y log(renta)
plot(trans.renta,trans.gasto)
abline(reg.pot)

# Invierto para volver al principio:
# gasto=exp(trans.gasto)
# renta=exp(trans.renta)
# a=exp(aa)
# b=b
a<-exp(reg.pot$coefficients[1])
a
b<-reg.pot$coefficients[2]
b
# Estimo gasto con la potencial
gasto.estimado=a*renta^b
gasto.estimado
gasto

# Veo la grafica
plot(renta, gasto, ylim = c(0,80))
curve(a*renta^b, xname = "renta",add=T,col="red")
# Parece practicamente como una recta, pero es que b es practicamente 1

####### SE PUEDE HACER LO MISMO UTILIZANDO LOGARITMO EN BASE 10
## y = a x^b
## log y = log a + b log x

# Realmente veremos que al transformar y e x, ya bien sea log en base e o base 10
# encontraremos el mismo coeficiente para la x y lo que sera diferente sera el intercepto
# Es decir creamos rectas igual de pendiente pero en diferente escala.

rr<-log(renta,10)
gg<-log(gasto,10)
qqplot(rr,gg)
rg<-lm(gg ~ rr)
summary(rg)

# nube de puntos
plot(rr,gg, ylab = "Log gasto en alimentación", xlab = "Log renta")
abline(rg)

# residuos
plot(predict(rg),rg$residuals) #variabilidad de los residuos entre dos valores
abline(0,0)

# log10(a)=rg$coefficients[1]  => a = 10^rg$coefficients[1]
a=10^rg$coefficients[1]
a

b=rg$coefficients[2]
b

gasto.estimado=a*renta^b
gasto.estimado
gasto

# Veo la grafica
plot(renta, gasto, ylim = c(0,80))
curve(a*renta^b, xname = "renta",add=T,col="red")


# b) Si la renta disponible bruta de los hogares en Ceuta y Melilla, en el año 2002, 
# fue de 1.500.000 de euros, ¿Cuál será la estimación del gasto en alimentación de los hogares
# de Ceuta y Melilla?

# y = a x^b
ceuta<-a*(1.5)^b 
ceuta*1000000     # estimación del gasto es de 183172.6 (0.1831726 mill.)


############################# EJEMPLO 3 ####################################
# Datos de Kepler del siglo XVII, en los cuales incluye la media de distancia solar
# y del periodo orbital por planetas. ¿Se puede establecer algun tipo de relacion, lineal
# o no lineal entre ellas?
planets = read.table(header=T, row.name=1, text="
planet    dist   period
Mercury   57.9    87.98
Venus    108.2   224.70
Earth    149.6   365.26
Mars     228.0   686.98
Ceres    413.8  1680.50
Jupiter  778.3  4332.00
Saturn  1427.0 10761.00
Uranus  2869.0 30685.00
Neptune 4498.0 60191.00
Pluto   5900.0 90742.00
")
planets

# Lo primero es ponerlo en unidades entendibles:
# Dividimos la distancia por la constante solar
planets$dist = planets$dist/149.6
# Y el periodo por numero de dias
planets$period = planets$period/365.26
planets

# Vemos los datos:
with(planets, scatter.smooth(period ~ dist, span=7/8, pch=16, cex=.6, las=1))
# Parece una exponencial o incluso una potencia

# Si probamos transformaciones
# Exponencial
with(planets, scatter.smooth(log(period) ~ dist))
title(main="Exponencial")
# Potencia
with(planets, scatter.smooth(log(period) ~ log(dist)))
title(main="Potencia")
# Parece que se adapta perfectamente a una transformacion potencia

# Planteamos modelo potencia, con transformacion de y e x
lm.out = lm(log(period) ~ log(dist), data=planets)
summary(lm.out)
# Se ajusta muchisimo, R^2=1, y los residuos son practicamente 0
# IMP: El intercepto no es significativo.
# Seria log(periodo)=1.5*log(dist)

# Analizamos residuos
plot(lm.out,1)
# Practicamente nulos, Neptuno y Pluto se compensan

# Y si quitaramos Pluto, ¿como quedarian la regresion y los residuos?
lm.out=update(lm.out, data=planets[-10,])       
summary(lm.out)
# Seguimos teniendo el mismo unico coeficiente significativo igual
# Por lo tanto no es influyente
plot(lm.out,1)
# Es practicamente una recta sobre el y=0

# CONCLUSION:
# Una regresion Potencia se adapta practicamente al 100% al periodo de rotacion con respecto
# a la media de distancia solar.
# No seria necesario ni siquiera quitar Pluton, no es influyente pero no cambia el analisis de 
# los residuos.

# Si tuvieramos que predecir el periodo de un planeta que estuviera a una media de distancia
# solar de 500.
dist.500<-500/149.6 #3.342246
# log(periodo)=b*log(dist)
# periodo = dist^b
periodo.500=dist.500^1.5  #6.110231
(periodo.500*365.26) # Seria un periodo de 2231.823


############################# EJEMPLO 4 ####################################
# Encontrar una relacion lineal o no lineal entre el indice de molestias de viandantes
# con respecto al volumen de pajaros por hectarea de la zona.

sparrows = read.table(header=T, text="
pedestrians pairs
                      1.75  14.2
                      5.83  30.1
                      5.33  71.2
                      4.67  77.5
                      7.17  75.9
                      5.50 121.8
                      9.33 132.1
                      6.83 159.0
                      7.50 181.9
                      10.80 184.3
                      11.30 194.6
                      11.40 219.1
                      10.50 246.8
                      12.80 166.9
                      13.80 162.2
                      17.80 134.5
                      16.20  94.1
                      19.80  88.6
                      21.90  90.2
                      26.60  44.3
                      28.70  14.2
                      32.60  15.8
                      38.20  47.5
                      ")
sparrows

# Miramos un poco la curva de relacion entre ambos
with(sparrows, scatter.smooth(pairs ~ pedestrians))
# Parece una regresion cuadratica o cubica. La probamos

lm.out = lm(pairs ~ pedestrians + I(pedestrians^2) + I(pedestrians^3), data=sparrows)
summary(lm.out)
# Tenemos un R^2adj=67% (ya que tenemos varias variables, en realidad x^2=w, x^3=v)
# No es una bondad de ajuste muy alta pero si significativa, y los coefficientes tambien

# Para predecir:
pairs.hat <- -91.2 + 49.7 * sparrows$pedestrians - 2.83 * sparrows$pedestrians^2 + 0.0426 * sparrows$pedestrians^3

plot(sparrows$pedestrians, sparrows$pairs)
curve(-91.2 + 49.7*x - 2.83*x^2 + 0.0426*x^3, add=T, col="red")
# Se adapta bastante pero la verdad es que tenemos algunos puntos distantes para pairs muy altos
# que no predecimos muy bien, de ahi el 67%

# Vamos a estudiar la influencia de dichos puntos:
plot(lm.out,4)
# El punto 23 es bastante influyente, tiene una distancia de cook de 0.8

# Probamos a borrarlo:
lm.out2 = update(lm.out, data=sparrows[-23,])
summary(lm.out2) 

plot(sparrows$pedestrians[-23], sparrows$pairs[-23])
curve(-105.19464 + 54.09956*x - 3.18069*x^2 + 0.05024*x^3, add=T, col="red")

# De todas formas los coeficientes cambian bastante, el 23 es un punto influyente. No deberia eliminarse

# Podemos ver el error cuadratico medio
ecm<-sum((sparrows$pairs-pairs.hat)^2)/dim(sparrows)[1] # 1277.983

# CONCLUSION: Hay una mediana adaptacion con un ECM bastante alto para el modelo cubico

############################# EJEMPLO 5 ####################################
# Utilizaremos los datos pressure, en los cuales encontramos la temperatura (en grados Celsius)
# y el vapor que genera en milimetros de mercurio.

data(pressure)
str(pressure)

# Convertimos primero los datos en algo comparable:
pressure$temperature = pressure$temperature + 273.15
# La tempreratura a grados Kelvin
pressure$pressure = pressure$pressure * .1333       
# La presion en KiloPascales
summary(pressure)

pres = pressure$pressure
temp = pressure$temperature

# Graficamos a ver que nos encontramos:

plot(pres ~ temp, main="Presion de vapor del mercurio",
     xlab="Temperatura (grados Kelvin)", ylab="Presion (kPascales)")
# Da la sensacion como una exponencial o potencia. Probamos con varias transformaciones logaritmicas:

par(mfrow=c(1,3))
plot(pres ~ temp, main="Presion de vapor del mercurio",
     xlab="Temperatura (grados Kelvin)", ylab="Presion (kPascales)", log="xy")
plot(pres ~ temp, main="Presion de vapor del mercurio",
     xlab="Temperatura (grados Kelvin)", ylab="Presion (kPascales)", log="x")
plot(pres ~ temp, main="Presion de vapor del mercurio",
     xlab="Temperatura (grados Kelvin)", ylab="Presion (kPascales)", log="y")
par(mfrow=c(1,1))

# Vamos a probar con las regresiones exponenciales y potencia:
par(mfrow=c(1,2))
# Exponencial: Hacemos log solo a la var. de salida
# Y=a*b^x --> log(y)=log(a)+log(b)x
lm.out1 = lm(log(pres) ~ temp) 
summary(lm.out1)
plot(lm.out1,1)# analisis residuos

# Potencia: Hacemos log a las 2 variables
# Y=a*x^b --> log(y)=log(a)+blog(x)
lm.out2 = lm(log(pres) ~ log(temp))
summary(lm.out2)
plot(lm.out2,1)# analisis residuos

# Parece que se adapta mejor la potencia, pero los residuos siguen un patron aleatorio.

# Probamos con regresion polinomica:
lm.out3 = lm(pres ~ temp + I(temp^2) + I(temp^3))
summary(lm.out3)
par(mfrow=c(1,1)) 
plot(lm.out3,1)# analisis residuos

# Veamos como ajusta el modelo polinomial:
plot(pres~temp)
curve(-475.8 + 3.804*x - .009912*x^2 + .00000844*x^3, add=T)
# Si bien es cierto que tenemos un alto ajuste del 98%, los residuos presentan patrones,
# y la curva no termina de ajustarse bien

# Probamos con un polinomio de grado 4:
lm.out4 = lm(pres ~ temp + I(temp^2) + I(temp^3) + I(temp^4))
summary(lm.out4) 
plot(lm.out4,1)

plot(pres~temp)
curve(647.6 - 6.948*x + 0.02758*x^2 - 0.00004813*x^3 + 0.00000003121*x^4, add=T)
# Esta curva si se adapta mucho mejor pero habria que asegurarse de la homocedeisticidad de las varianzas
# puesto que el analisis de residuos no queda del todo claro. Pero parece un buen modelo.

# OTRA OPCION: Transformaciones Box-cox
# Intentamos ver como seria la inversa de la temperatura:
plot(pres ~ I(1/temp))              
# Podria ser una de las ramas de una hiperbole, la rama x>0

# Las transformaciones de box-cox nos permiten encontrar la transformacion optima para la
# variable de salida:
library("MASS")
par(mfrow=c(1,2))
# plot basico de box
boxcox(pres ~ I(1/temp))
# resolucion mas clara en lambda, le incluimos limites al cuadrante
boxcox(pres ~ I(1/temp), lambda=seq(-.2,.2,.01))    
par(mfrow=c(1,1))

# lambda=0 esta al 95%, esto nos indica que la transformacion a utilizar es log(var_salida)
plot(log(pres) ~ I(1/temp))
# Recta perfecta
lm.out5 = lm(log(pres) ~ I(1/temp))
summary(lm.out5)
# Significativos y con 99% de explicacion

# Miramos residuos y distancias de cook
par(mfrow=c(2,2))
plot(lm.out5)
par(mfrow=c(1,1))
# Exceptuando el punto 4, el resto estan perfectamente alineados
# TENEMOS REGRESION LINEAL OPTIMA
# log(pres) = 1.626 - 7307*1/temp
# Sabiendo que log(pres)=y e 1/temp=x

log.pres.estimado=16.26 - 7307*(1/temp)
pres.estimado=exp(log.pres.estimado)
pres.estimado
pres

#Error cuadratico medio
(ecm=sum((pres-pres.estimado)^2)/length(pres)) # 1.59, muy bajo

plot(pres~temp)
curve(exp(16.26 - 7307*(1/x)), add=T)

# Si nos fijamos en el punto 4 se desvia un poco:
plot(pres~temp, xlim=c(250,350),ylim=c(0,0.01))
curve(exp(16.26 - 7307*(1/x)), add=T)

# Seguramente sin el punto 4 tendriamos una estimacion perfecta, pero esta regresion
# puede ajustar muchisimo

# Ahora podemos hacer predicciones facilmente:
new.temps = seq(from=400, to=600, by=50)
new.temps
new.pres = exp(predict(lm.out5, list(temp=new.temps)))
new.pres
