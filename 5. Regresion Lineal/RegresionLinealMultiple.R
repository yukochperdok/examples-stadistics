# Limpiar el workspace
rm(list = ls(all = TRUE))
library(scatterplot3d)
library(Rcmdr)

##################################EJEMPLO 1###################################
# Se desea estimar los gastos en alimentación de una familia (y) en base a 
# los ingresos mensuales (x1) y el número de miembros de la familia (x2).
# Para ello se recoge una muestra aleatoria simple de 15 familias:
# (El gasto e ingreso está dado en cientos de miles de pesetas)

# gasto: 
y<-c(43,31,32,46,125,44,52,29,129,35,35,78,43,47,38)

# ingresos:
x1<-c(21,11,9,16,62,23,18,10,89,24,12,47,35,29,14)

# número de miembros de la familia:
x2<-c(3,4,5,4,4,3,6,5,3,2,4,3,2,3,4)

####### Revisar muestras (variables)
df<-data.frame(y,x1,x2)

str(df)
summary(df)
hist(y)
hist(x1)
hist(x2)
boxplot(y)
stem(y)
boxplot(x1)
stem(x1)

# Probamos una ANOVA sobre x2 que parece un tratamiento sobre y
# Habria que comprobar normalidad (valor-media por trat) en cada poblacion e igualdad de varinzas
fac.x2<-factor(x2)
tapply(y,x2,mean)

p.aov<-aov(y~x2)
summary(p.aov)
# Vemos que si esta distribuido de igual forma por tratamiento

########## RELACIONES ENTRE VARIABLES

pairs(df)
cor(df)
parcor(cor(df))
pcor(c("x1","x2","y"),cor(df))

# x1 se relaciona altamente con y, x2 tambien pero su relacion con x1 nos hace sospechar.

########## MODELO(s)
# Iniciamos con x1
reg1<-lm(y~x1)
summary(reg1)
# Modelo lineal simple con R cuadrado = 88% y coeficientes muy definidos

# Diagrama de dispersion
qqplot(x1,y, ylim=c(-10,100))
abline(reg1, col="red")

#########ANALISIS RESIDUOS
# Residuos tipificados VS VD estimada
plot(predict(reg1),(reg1$residuals-mean(reg1$residuals))/sd(reg1$residuals))
abline(h=0,col="red")
# Alta varianza pero sin patrones raros


# Residuos VS VD
plot(x1, reg1$residuals)
abline(h=0,col="red")
# Idem que la anterior

# Se podria añadir un plot de los residuos con el tiempo

# Analisis de normalidad y homocedeistedad en los residuos por tramos
shapiro.test(reg1$residuals) # Normalidad OK

grupo.residuos<-factor(c(rep("g1",3),rep("g2",3),rep("g3",3),rep("g4",3),rep("g5",3)))
identical(length(grupo.residuos),length(reg1$residuals))
leveneTest(reg1$residuals, group = grupo.residuos) # Varianzas iguales

# Comprobamos independencia en los residuos
#H0 no ha correlacion entre los residuos
durbinWatsonTest(reg1, alternative = "two") # Aceptamos H0

############# MODELO 2
# Añadimos x2 al modelo
reg2<-lm(y~x1+x2)
summary(reg2) # Peor modelo porque el intercepto empeora. Aunque R ajustado sea mejor

qqplot(x1, y, ylim=c(-10,100))
abline(reg2,col="red")

qqplot(x2, y, ylim=c(-10,100))
abline(reg2,col="red")

##################CI y PI
# Nos quedamos con reg1
qqplot(y, predict(reg1))
abline(c(0,1))

qqplot(x1, y)
abline(reg1, col="red")

# Predecir valores
interval <- sample(9:89, 50, replace=T)


y.predict<-predict(reg1,newdata = data.frame(x1=interval))
# Intervalos confianza
y.CI<-predict(reg1,newdata = data.frame(x1=interval), interval = "confidence", level = 0.9)
# Intervalos de prediccion
y.PI<-predict(reg1,newdata = data.frame(x1=interval), interval = "prediction", level = 0.9)

qqplot(x1, y)
abline(reg1, col="red")
# Dibujamos intervalo de prediccion en verde e intervalo de confianza en azul
abline(line(interval, y.CI[,"lwr"]), col="blue")
abline(line(interval, y.CI[,"upr"]), col="blue")
abline(line(interval, y.PI[,"lwr"]), col="green")
abline(line(interval, y.PI[,"upr"]), col="green")


############## CURIOSIDAD
# Es muy probable que los valores atipicos en y y x1 nos esten dañando, 
# ¿y si los quitamos que rango de betas tendriamos?
boxplot(y)
boxplot(x1)
# Posiciones 5 y 9
new.y<-y[-c(5,9)]
new.x1<-x1[-c(5,9)]
new.x2<-x2[-c(5,9)]
boxplot(new.y)
boxplot(new.x1)
boxplot(new.x2)

new.df<-data.frame(new.y,new.x1,new.x2)
cor(new.df)
pairs(new.df)

parcor(cor(new.df))

new.reg1 <- lm(new.y~new.x1)
summary(new.reg1)
summary(reg1)

new.reg2 <- lm(new.y~new.x1+new.x2)
summary(new.reg2)
summary(reg2)

# CONSEGUIMOS PEORES MODELOS QUITANDO LOS OUTLAYERS.
# Quitar los outlayers no siempre significa encontrar un mejor modelo.

############ANOVA
# Puedes plantear una anova sobre la regresion lineal
summary(reg1)
anova(reg1)

summary(reg2)
anova(reg2)

#Incluso
anova(lm(y~x1*x2))

# Nos surge la curiosidad de como quedaria un modelo: Y=b0+b1x1+b2x2+b3x1x2
x3<-x1*x2
reg3<-lm(y~x1+x2+x3)
summary(reg3)
# Parece mejorar pero p-value de x1 empeora

# ¿Y si le incluyo el cuadrado de ambos?
x4<-x1**2
x5<-x2**2
reg4<-lm(y~x1+x2+x3+x4+x5)
summary(reg4)
# Mucho peor modelo, sobran practicamente todos

# Y si quitamos el x1 del reg3??
reg5<-lm(y~x2+x3)
summary(reg5)

# Mucho mejor modelo, R cuadrado ajustada al 98% y los coeficientes con mucha menos incertidumbre


##################################EJEMPLO 2###################################
#Datos de R sobre coches: mtcars
# Explicar el consumo (mpg) en función de la potencia (hp) y del peso (wt):

data(mtcars)
attach(mtcars)
# Mostramos datos
head(mtcars)
str(mtcars)
summary(mtcars)
# Con el summary podemos ver los factores, si hay nulos, tipos de valores.
# Si son discretos o continuos, a parte de como estan distribuidos mas o menos
# Vemos que am, gear, carb, cyl son valores discretos. Pero habria que factorizarlos
# Vemos sin embargo que mpg, drat, wt y qsec son valores continuos
# Y vemos que disp y hp son valores discretos no factorizables

# Revisamos las variables que nos han pedido:
# mpg - Var. continua - consumo
head(mpg)
summary(mpg)
boxplot(mpg) # Sin valores atipicos: entre 10 y 30. Mediana 20
hist(mpg)
plot(density(mpg))# Claramente una normal

shapiro.test(mpg) # Distribucion normal
mean(mpg) # 20.09062
sd(mpg) # 6.026948

# hp - Var discreta - Potencia
head(hp)
summary(hp)
boxplot(hp) # Pocos val. atipicos.
hist(hp)
plot(density(hp))# Parece una lognormal

shapiro.test(hp) # No es normal
shapiro.test(log(hp))# Es log normal
boxplot(log(hp)) # Eliminados val. atipicos, bigotes perfectos
hist(log(hp))
plot(density(log(hp)))# Confirmado Distribucion lognormal

# wt - Var discreta - Peso
head(wt)
summary(wt)
boxplot(wt) # Pocos val. atipicos,cercanos al bigote superior
hist(wt)
plot(density(wt))# Parece una lognormal

shapiro.test(wt) # Podria tratarse como una normal
shapiro.test(log(wt))# Pero es LogNormal

boxplot(log(wt)) # Eliminados val. atipicos, bigotes perfectos
hist(log(wt))
plot(density(log(wt)))# Confirmado Distribucion lognormal


# Hacemos un plot de las variables para ver que correlaciones nos podemos encontrar:
mtcars.minimo<-mtcars[,c("mpg","wt","hp")]
plot(mtcars.minimo)
# Buscamos correlaciones
cor(mtcars.minimo)
parcor(cor(mtcars.minimo))
# Vemos que hay bastante correlacion parcial entre mpg, hp y wt


######MODELO(s) 
# Se deberia ir incluyendo uno a uno. De hecho nosotros ya hemos visto que se 
# va a comportar mejor con los log.
# De todas formas aqui nos han fijado ambas variables y nos han pedido un modelo
# por lo tanto montamos el modelo directamente.
cars.lm = lm(mpg~hp+wt)
summary(cars.lm)
# Bastante buen modelo: 81% y p-values bajos.
#modelo de regresión
# millas recorridas por galón = 37.22 - 0.03 potencia - 3.87 peso

# Analisis de residuos

# Residuos VS consumo estimado
# para ver si hay algún coche que se comporta de modo muy distinto a los demás:
plot(predict(cars.lm), cars.lm$residuals)
abline(h=0)
#o:
plot(cars.lm$fitted.values,cars.lm$residuals)
abline(h=0)
# No hay una tendencia clara pero hay algun valor que tiene una varianza muy alta: val. atipicos
# Si normalizamos lo veremos mejor
mean.res<-mean(cars.lm$residuals)
sd.res<-sd(cars.lm$residuals)
plot(cars.lm$fitted.values,(cars.lm$residuals-mean.res)/sd.res)
abline(h=0)
abline(h=-1,col="blue")
abline(h=1,col="blue")
# Hay valores extremos que se salen. Podemos confirmarlo con Levene
grupo.residuos<-factor(c(rep("g1",8),rep("g2",8),rep("g3",8),rep("g4",8)))
identical(length(grupo.residuos),length(cars.lm$residuals))
# H0: varianzas iguales
leveneTest(cars.lm$residuals, group = grupo.residuos) # => Aceptamos H1-->Varianzas desiguales
############ NO VALE NUESTRO MODELO

############TRANSFORMACIONES

# Averiguamos poder de transformacion:
spreadLevelPlot(cars.lm) # 0.5853955
# Poder de transformacion 0.5, es decir es equivalente a log
curve(x**0.5,0:20, col = "green",xlim = c(0,4), ylim = c(-2,2))
curve(log,0:20, add = T ,col = "violet")
curve(x**2,0:20, add = T ,col = "blue")
curve(log(x,5),0:20, add = T ,col = "red")
abline(h=0, v=0)

# TRANSFORMACION EXPONENCIAL 0.5
hp.trans<-hp**0.5
wt.trans<-wt**0.5
cars.lm2 = lm(mpg~hp.trans+wt.trans)
summary(cars.lm2)
#Analizamos residuos
mean.res<-mean(cars.lm2$residuals)
sd.res<-sd(cars.lm2$residuals)
plot(cars.lm2$fitted.values,(cars.lm2$residuals-mean.res)/sd.res)
abline(h=0)
abline(h=-1,col="blue")
abline(h=1,col="blue")
# Mucho mejor los residuos

# TRANSFORMACION LOGARITMICA
cars.lmlog = lm(mpg~log(hp)+log(wt))
summary(cars.lmlog)
#Analizamos residuos
mean.res<-mean(cars.lmlog$residuals)
sd.res<-sd(cars.lmlog$residuals)
plot(cars.lmlog$fitted.values,(cars.lmlog$residuals-mean.res)/sd.res)
abline(h=0)
abline(h=-1,col="blue")
abline(h=1,col="blue")

# Varianzas iguales??
grupo.residuos<-factor(c(rep("g1",8),rep("g2",8),rep("g3",8),rep("g4",8)))
identical(length(grupo.residuos),length(cars.lmlog$residuals))
# H0: varianzas iguales
leveneTest(cars.lmlog$residuals, group = grupo.residuos) # => Varianzas iguales por poco

# Residuos normales???
shapiro.test(cars.lmlog$residuals) # Poca normalidad en los residuos

#Residuos Independientes 
#H0 no ha correlacion entre los residuos
durbinWatsonTest(cars.lmlog, alternative = "two") # Aceptamos H0 -> Residuosindependientes

# Cerramos nuestro modelo con 88%, bastante explicativo pero los residuos tienen aun varianzas altas
# y poca normalidad.
# CONCLUSION: Se deberian tomar mas datos o incluir mas variables para explicar mejor el consumo

#Si queremos predecir las millas recorridas por galón por un coche con 150 caballos y peso 2.5:

# Consumo real VS estimado
pred<-predict(cars.lmlog) 
qqplot(mpg,pred)
abline(c(0,1))
#o:
plot(mpg,58.231-11.400*log(wt)-5.193*log(hp),  xlim = c(0,40), ylim = c(0,50))
abline(c(0,1))

# Representacion VD VS VI's
plot(log(wt), mpg)
abline(lm(mpg~log(wt)),col="red")

plot(log(hp), mpg)
abline(lm(mpg~log(hp)),col="red")

# Con todas las VI's
s3d<-scatterplot3d(log(wt),log(hp),mpg, main="3D Scatterplot", xlim = c(0,5),zlim = c(-10,60),ylim = c(0,5))
s3d$plane3d(cars.lmlog)
#o
s3d<-scatterplot3d(log(wt),log(hp),mpg, pch=16, highlight.3d=TRUE,
              type="h", main="3D Scatterplot",xlim = c(0,5),zlim = c(-10,60),ylim = c(0,5)) 

s3d$plane3d(cars.lmlog)

# o tambien con graficos RGL
scatter3d(wt, hp, mpg) 



# Para predecir lo mejor es usar los intervalos de prediccion:

interval.wt<-runif(10,min(wt),max(wt))
interval.hp <- sample(min(hp):max(hp), 10, replace=T)
mpg.predict<-predict(cars.lmlog,newdata = data.frame(wt=interval.wt,hp=interval.hp))
# Intervalos de prediccion
mpg.PI<-predict(cars.lmlog,newdata = data.frame(wt=interval.wt,hp=interval.hp), interval = "prediction", level = 0.9)

# Dibujamos el intervalo de prediccion (90%) en negro y azul
# Y la prediccion exacta en rojo
s<-scatterplot3d(interval.wt,interval.hp,mpg.predict, color = "red", xlim = c(0,10),zlim = c(0,50),ylim = c(10,350))
s$points3d(interval.wt,interval.hp,mpg.PI[,"lwr"], col = "black")
s$points3d(interval.wt,interval.hp,mpg.PI[,"upr"], col = "blue")








##################################EJEMPLO 3###################################

# Trabajar con un modelo explocativo de mpg para todas las variables de mtcars

# Imaginemos que ya tenemos toda la exploracion de las variables hecha.
# Ajustamos un posible modelo:
fit <- lm(mpg ~ hp + wt + disp, data=mtcars)
summary(fit)

############ FUNCIONES INTERESANTES:
# Coeficientes del modelo
coefficients(fit) 
coefficients(fit)[1]# Intercepto
coefficients(fit)["hp"]# coef. hp
coefficients(fit)["wt"]# coef. wt
coefficients(fit)["disp"]# coef. disp
#o 
fit$coefficients


# Residuos
residuals(fit) 
#o
fit$residuals


#Valores estimados
fitted(fit)
#o
fit$fitted.values
fit$fitted.values["Fiat X1-9"] # Valor estimado para el Fiat X1-9


# Intervalos de confianza para cada para coeficiente
confint(fit, level=0.95) # Dejando 2.5 y 97.5 a cada lado

# Tabla ANOVA
anova(fit)  

# Matriz de covarianza para los coeficientes
vcov(fit)  

# Diagnostico de regresion:
# Te devuelve los coefficientes para cada tipo de coche.
# Las desviaciones por tipo de coche.
# Pesos de los residuos por tipo de coche.
# Todo esto para poder diagnosticar que influye mas en cada coeficiente, residuo, etc...
influence(fit)
influence(fit)$coefficients # Coefficientes
influence(fit)$sigma # Desviaciones
influence(fit)$wt.res # Pesos
# Por ejemplo: El coche con mayor residuo:
max(influence(fit)$wt.res) # Toyota Corolla
# Coche con minimo coeficiente para disp
min(influence(fit)$coefficients[,"disp"]) # Maserati Bora

######### PLOTS INTERESANTES:
layout(matrix(c(1,2,3,4),2,2)) # optional 4 graphs/page 
plot(fit)
# Simplemente haciendo esto ya vemos los 4 plots mas interesantes:
# 1. Residuos VS Estimados --> Checkeamos heteroscedasticidad de las varianzas
# 2. Residuos tipificados VS Estimados --> Checkeamos heteroscedasticidad de las varianzas
# 3. Quantiles de residuos VS quantiles de  dist.normal --> Normalidad de residuos
# 4. Residuos VS Leverage --> Detectar observaciones influyentes. (aquellas que sobrepasan la distacia de Cook)
# IMP: en todos marca una linea roja indicando tendencia.Y te marca los residuos 
# mas atipicos: en nuestro caso Corolla, Chrysler y Maserati Bora.
layout(matrix(1,2)) # optional 4 graphs/page 


################COMPARAR MODELOS - CON ANOVA

fit1 <- lm(mpg ~ hp + wt + disp, data=mtcars)
fit2 <- lm(mpg ~ hp + wt, data=mtcars)
anova(fit1, fit2)

# Te compara entre el modelo fit1 y fit2 y te dice que el modelo fit2 es mejor porque
# su suma de cuadrados de los residuos tiene 0.05 MAS que la del fit1
# Se puede comprobar aqui:
            #           Df Sum Sq Mean Sq
anova(fit1) # Residuals 28 194.99    6.96
anova(fit2) # Residuals 29 195.05    6.73

# IMP: Cuanto mayor sea la SC, menor sera la varianza residual.


################# CROSS - VALIDATION

fit <- lm(mpg ~ hp + wt + disp, data=mtcars)
summary(fit)
library(DAAG)
cv.lm(data = mtcars, fit, m=3)

# Para las K variables de un modelo, cv.lm, te calcula el error estandar. Es decir
# los estimados por VD y los dibuja en lineas de regresion de diferentes colores
# y formas
# Asi puedes enfrentar: valores reales de mpg VS estimados de mpg (con respecto a cada VD)

# Tambien se puede ver como se va reduciendo la R2:
library(bootstrap)
# Definimos funciones theta
theta.fit <- function(x,y){lsfit(x,y)}
theta.predict <- function(fit,x){cbind(1,x)%*%fit$coef} 

# Matriz de VI
X <- as.matrix(mtcars[c("hp","wt","disp")])
# Vector de VD
y <- as.matrix(mtcars[c("mpg")]) 

results <- crossval(X,y,theta.fit,theta.predict,ngroup=10)
cor(y, fit$fitted.values)**2 # R2 en bruto
cor(y,results$cv.fit)**2 # R2 cross validada


############ METODOS PARA SELECCIONAR EL MEJOR MODELO
#####STEPWISE
# Stepwise Regression
# Este metodo se basa en la combinacion de 
# 1. forward: partiendo de un modelo base(normalmente modelo vacio) ir incorporando 
# ordenadamente por coeficiente de correlacion parcial todas las variables mientras 
# no superen un p-value de barrera 
# 2. backward: partiendo de un modelo completo (normalmente todas las variables a estudiar)
# se val eliminando ordenandamente segun su p-value mayor. Hasta que no hay un p-value que 
# supere un p-value min.
# Para cada step se evalua de nuevo el modelo, pues los p-value han podido cambiar.
# both: conjuga ambas y va añadiendo (forward) por orden de correlacion parcial, y eliminando
# por p-value maximo.



library(MASS)
fit <- lm(mpg ~ hp + wt + disp, data=mtcars)
step <- stepAIC(fit, direction="backward")
step$anova

# Para la seleccion del tipo de metodo utilizamos "direction".
# En step(modelo, direction) o stepAIC(modelo, direction) comprueba el ajuste al modelo 
# de la variable, no con p-value o con R2-ajust, sino con AIC (que depende de la suma de cuadrados de los residuos)
# A mayor suma de cuadrados=> Mayor AIC => Menor varianza de errorres => MEJOR MODELO

# step$anova te muestra el modelo inicial y el modelo final. 
# Y el porque de las variables eliminadas en funcion a AIC.
# En nuestro caso ha eliminado disp.

# Si queremos partiendo de un modelo base coger el mejor modelo explicativo de mpg:

####### Planteamos todo el modelo fit0
# mpg ~ con todas las variables de mtcars
fit_max <- lm(mpg ~ ., data=mtcars)
summary(fit_max)
anova(fit_max)
# Solo tendria sentido incluir wt

####### Planteamos el modelo minimo
# mpg ~ con ninguna variable
fit_min <- lm(mpg ~ 1, data=mtcars)
summary(fit_min)
anova(fit_min)
# Solo intercepto, esto significaria que no hay modelo

####### Planteamos soluciones para Forward, Backward y Both
step.backward <- stepAIC(fit_max, direction="backward")
step.backward$anova
# Inicial modelo el maximo: mpg ~ cyl + disp + hp + drat + wt + qsec + vs + am + gear + carb
# Final modelo ajustado por AIC: mpg ~ wt + qsec + am
# FIjaros que al principio qsec y am practicamente ni se tenian en cuenta, y sin embargo
# cyl o disp si parecian entrar. Multicolinedad seguramente.

step.forward <- stepAIC(fit_min, direction="forward", 
                         scope = list(lower=fit_min, upper=fit_max))
step.forward$anova
# Inicial modelo el minimo: mpg ~ 1
# Final modelo ajustado por AIC: mpg ~ wt + cyl + hp
# Fijaros que no coincide para nada con el backward. Ahora cyl y hp si han entrado.

# CONCLUSION: Los metodos forward y backward son incompletos. 
# Hay que utilizar both

step.both <- stepAIC(fit_min, direction="both", 
                        scope = list(lower=fit_min, upper=fit_max))
step.both$anova
# Inicial modelo el minimo: mpg ~ 1
# Final modelo ajustado por AIC: mpg ~ wt + cyl + hp
# Este si coincide con step.backward, porque no se ha tenido que quitar ninguna
# variable del modelo una vez incluida.
# El modelo explicativo MINIMO para mpg es:
#       mpg ~ wt + cyl + hp
fit.optimo<-lm(mpg ~ wt + cyl + hp, data=mtcars)
summary(fit.optimo)
anova(fit.optimo)
# Como vereis hemos encontrado un modelo explicado al 82%
# las variables wt y cly si se ajustan perfectamente pero la hp no. ¿Entonces?
# Aqui depende mucho de las variables que le incluyamos y las transformaciones:
log.hp<-log(mtcars$hp)
log.wt<-log(mtcars$wt)

fit_max <- lm(mpg ~ .+log.hp+log.wt, data=mtcars)
summary(fit_max)
anova(fit_max)
step.both <- stepAIC(fit_min, direction="both", 
                     scope = list(lower=fit_min, upper=fit_max))
step.both$anova
# Initial Model: mpg ~ 1
# Final Model:   mpg ~ log.wt + log.hp

fit.optimo<-lm(mpg ~ log.wt + log.hp, data=mtcars)
summary(fit.optimo)
anova(fit.optimo)
# MODELO OPTIMO: explicado al 88% y con SE residuos bajisimo: 2.08
# Por lo tanto el mejor modelo es:
#       mpg ~ log.wt + log.hp

# OBS: Tambien podriamos hacer transformaciones para el resto de variables.
# Y para la variable dependiente.

########MEJOR SUBCONJUNTO
# Este metodo se basa en encontrar para un modelo completo de partida, la mejor combinacion
# de variables de tamaño n. Para elejirlo se puede basar en R2 o CP o R2-ajustado
# Si por ejemplo estamos determinados por un nuemro de variables maximo es interesante
# Hacerlo por este metodo. Sino es asi, compensa computacionalmente Stepwise
library(leaps)
attach(mtcars)
# OBS: No vamos a incluir todas las variables para que se vean mejor las visualizaciones
# pero se podrian incluir todas perfectamente
leaps<-regsubsets(mpg~log.wt+log.hp+cyl+disp+qsec,data=mtcars,nbest=3)
# Se ha indicado nbest=3, queremos ver para cada n combinacion, 3 posibilidades.
summary(leaps)
# Devuelve una tabla de resultados ordenado por n y dentro de cada n el orden de eleccion (nbest)
# de tal forma que si nosotros queremos elegir el mejor subconjunto de 3 variables:
#          log.wt log.hp cyl disp qsec
# 3  ( 1 ) "*"    "*"    " " " "  "*" 
# Elegiriamos: log.wt + log.hp + qsec + (Intercepto)

# Y lo podemos ver en grafico, ordenados por "Cp" o "adjr2" o "r2".
plot(leaps,scale="adjr2")
plot(leaps,scale="Cp")

# O con el grafico de subconjuntos ordenados por: adjr2 o rsq o cp, o incluso rss o bic
library(car)
subsets(leaps, statistic="adjr2")
subsets(leaps, statistic="cp")

fit.optimo.3.var<-lm(mpg ~ log.wt + log.hp + qsec, data=mtcars)
summary(fit.optimo.3.var)
anova(fit.optimo.3.var)


######### RELATIVA IMPORTANCIA DE LAS VI
# Se puede calcular por diversos algoritmos la relativa importancia de cada variable.
# Esto te dice que variable es mas importante en el modelo. NO la que aporta mas
# que seria tipificando como la encontrariamos, sino la que mas aporta en efectos 
# de ajuste al modelo.
# El algoritmo mas utilizado y recomendado es lmg: se basa en R2

library(relaimpo)
calc.relimp(fit.optimo.3.var,type=c("lmg","last","first","pratt"),rela=TRUE)
# Devuelve muchas cosas pero lo mas importante es:
#           lmg   last first  pratt
# log.wt 0.5111 0.8839 0.475 0.6736
# log.hp 0.4003 0.0951 0.422 0.2813
# qsec   0.0887 0.0209 0.103 0.0451
# Te dice para los 4 algoritmos que la mas relevante es log.wt. 

# Se puede ver con otro paquete:
boot <- boot.relimp(fit.optimo.3.var, b = 1000, type = c("lmg","last", "first", "pratt")
                    , rank = TRUE, diff = TRUE, rela = TRUE)
booteval.relimp(boot) # print result
plot(booteval.relimp(boot,sort=TRUE)) # plot result 
# Aqui te lo explican graficamente con las diferencias de relavancia entre
# las variables, la que esta primera con respecto a las demas es log.wt
# para los 4 algoritmos.


############### TRATAMIENTOS IMPROTANTES
### 1. Tratamiento Outlayers
# Imaginemos que tenemos esta regresion:
fit <- lm(mpg~disp+hp+wt+drat, data=mtcars)
summary(fit)

# Identificar outlayers sobre recta de regresion (con quantiles)
qqPlot(fit, main="QQ Plot")

# Identificar outlayers 
leveragePlots(fit) 

# 3 Plots para la identificacion de los outlayers con su distancia Cook
par(mfrow=c(2,2)) 
plot(fit,which = 4:6)
par(mfrow=c(1,1))

# Test de Bonferonni para las observaciones mas extremas:
# Coge el valor con un residuo estandarizado mas alto y utiliza una t de Student
# para recoger los puntos mas "extremos", que no tienen porque ser los que visualmente
# veamos en la grafica mas extremos.
outlierTest(fit, cutoff=3) 
# En nuestro caso Los 3 primeros son:
# Toyota Corolla    2.515970           0.018380      0.58816
# Fiat 128          2.495829           0.019238      0.61562
# Chrysler Imperial 2.244689           0.033518           NA
# Sin embargo el Maserati Bora no aparece. Porque su residuo standarizado es menor.

### 2. Tratamiento Outlayers INFLUYENTES
# Cuando ya tenemos los outlayers identificados como en el ùnto anterior
# tenemos que ver cuales de ellos son influyentes.
# Para ello tenemos que calcular un numero de corte cutoff. Que sera el minimo numero
# entre 0 y 1 para que sea influyente ese punto.
# Hay gente que aplica: 4/(n-k-1), 
#                         para k dimensiones (variables)
#                         para n valores de cada variable
# Otros prefieren aplicar: 2(k+1)/n incluso 3(k+1)/n para los mas restrictivos

# Primero vemos un plot de la VD con cada VI para ver los outlayers
# OJO DEPRECATED FUNCTION
library(car)
av.plots(fit)

# Segundo calculamos el minimo cutoff y vemos el plot de distancia de Cook para 
# ese cutoff minimo
# cutoff > 4/(n-k-1) 
k<-length(fit$coefficients)-1 # Le quito el intercepto
n<-nrow(mtcars) # Numero de filas (num.valores)
cutoff <- 4/((n-k)-1) # 0.1481481 
cutoff <- 4/((n-k)-1) # 0.1481481 
plot(fit, which=4, cook.levels=cutoff)
# Nos aparecen marcados como influyentes por su distancia:
# Maserati Bora, Chryler Imperial y Toyota Corolla

# Por ultimo nos ayudamos del plot de influencia para ver realmente 
# los outlayers que mas influyen:
# Influence Plot 
influencePlot(fit, id.method="identify", main="Influence Plot", 
              sub="Circle size is proportial to Cook's Distance" )
# En el grafico te muestra la influencia que tienen en funcion al diametro de su circulo
# Te los deja marcar para identificarlos.
# En el eje de las x tienes los que cometen mas error y en el eje de las y los que tienen
# mas distancia de cook (calculada antes).

# CONCLUSION: Aunque Toyota corolla tiene mas residuos standard, tiene menos influencia
# que Chrysler y Maserati. IMP: Estos dos en ningun caso han de quitarse.



####### 3. Tratamiento Normalidad
# Para revisar si los residuos cumplen normalidad:

# QQplot para residuos estandarizados
qqPlot(fit, main="QQ Plot")
# Y tambien ver el plot de residuos estandarizados sobre los quantiles de la normal:
plot(fit, which=2)

# Distribucion de residuos estandarizados
library(MASS)
sresid <- studres(fit) 
hist(sresid, freq=FALSE, 
     main="Distribucion de residuos estandarizados")
# Devuelve una secuencia de 40 obs. desde el minimo al maximo
xfit<-seq(min(sresid),max(sresid),length=40) 
# Probabilidad de una normal para cada valor de xfit
yfit<-dnorm(xfit) 
# Las enfrento y reviso la normalidad
lines(xfit, yfit) 

# Puedo incluso aplicar algun test:
shapiro.test(sresid) # Veo la NO NORMALIDAD



###### 4. Tratamiento Varianzas Iguales
# Para revisar si la varianza de los residuos es constante
# Una de las formas mas comunes es crear grupos para los residuos y aplicar un levene.test

# Una forma mas facil de aplicar lo mismo es con un ncvTest(non-constant error variance test)
# H0: varianzas constantes
ncvTest(fit) #p-value=0.231818>0.05 => Aceptamos H0=> Varianzas constantes

# Otro test analogo seria:
# H0: varianzas constantes
library(lmtest)
bptest(fit) #p-value=0.8371>0.05 => Aceptamos H0 => Varianzas constantes


# Cuando las varianzas no son constantes tenemos que transformar las variables.
# En este plot vemos los residuos VS estimados y vemos como van cambiando
# las varianzas(linea verde) a traves de la regression(linea roja).
spreadLevelPlot(fit)
# Y te sugiere un poder de transformacion: 0.6616338
# Cuya p siempre sera el multiplo de 0.5 mas cercano. En nuestro caso p=0.5 => Transformar variables en X^p
# Si p=0 => Transformar en ln(X)

# Otra opcion es hacer una transformacion BoxCOx para la VD: mpg en nuestro caso
library(caret)
mpg.trans<-BoxCoxTrans(mtcars$mpg)
print(mpg.trans)
# Hemos creado una transformacion ahora tenemos que cargar los valores
mpg.new<-predict(mpg.trans,mtcars$mpg)
new.mtcars<-cbind(mtcars,mpg.new)
head(new.mtcars)

# Ahora creamos una regresion transformada:
fit.trans.boxcox<-lm(mpg.new~disp+hp+wt+drat, data=new.mtcars)
summary(fit.trans.boxcox)
summary(fit)

# Vemos de nuevo las varianzas
ncvTest(fit.trans.boxcox)
spreadLevelPlot(fit.trans.boxcox)

ncvTest(fit)
spreadLevelPlot(fit)

# Hemos reducido la desviacion, se ve claramente en las graficas y en el 
# Residual standard error del summary de los fit.

####### 5. Independencia de residuos
# Para que sea valida una regresion lineal se debe cumplir que cada residuo
# sea independiente del siguiente. Esto se demuestra con el test de Durbin-Watson:

# H0 no ha correlacion entre los residuos
durbinWatsonTest(fit, alternative = "two") # p-value= 0.272>0.05=> Independencia de los residuos




####### 6. Multicolinealidad
# Aunque ya lo tenemos bastante pillado, dos variables VD pueden estar correlacionadas 
# entre ellas. Se ve claramente cuando tenemos un R^2 alto y algun p-value muy alto.
# Esa variable se explica por otra.

# Una forma facil de revisarlo es utilizando el coeficiente VIF:
# Se define como 1/1-R^2(de la variable)

vif(fit) # variance inflation factors 
sqrt(vif(fit)) > 2 # Si la raiz de la influencia es mayor que 2 => Se explica por otra
# En nuestro caso tenemos multicolinealidad de las variables disp y wt.
# Tendriamos que ir quitando 1º disp, y luego volver a mirar que tal wt
# Ejemplo:
sqrt(vif(lm(mpg~hp+wt+drat, data=mtcars)))>2
# Quitando disp, la inflaccion de wt baja muchisimo

# CONCLUSION: Habia multicolinealidad entre wt y disp. Quitando disp solucionamos el problema 


######## 7. Evaluar Linealidad
# Cuanto mas lineal son los puntos mas se ajustara un modelo lineal

# Plot del componente + residuo
crPlots(fit) # Te indica si la relacion es muy o poco lineal para cada VD
# Ceres plots indica lo mismo que el de antes 
ceresPlots(fit)

# Vemos que para wt la linea verde se ajusta bastante a la regresion
# Por contra disp o drat no se ajustan mucho.
# Esto al final es equivalente a cor o parcor


################################## EJEMPLO 4 ##############################################
# El gerente de una determinada empresa está estudiando las posibles relaciones entre las variables 
# X1: Beneficios anuales (millones de euros)
# X2: Gastos en publicidad anuales (millones de euros)
# X3: horas extraordinarias anuales de los empleados (100 h),
# utilizando para ello datos de estas tres variables proporcionadas
# por algunas empresas de su sector:

# Beneficios, gastos, horas
beneficios<-c(1.3,3.5,2.8,3,3.3,4,3.7)
gastos<-c(0.3,1.5,0.7,1.1,1.2,2,2)
horas<-c(4,9,6,5,8,7,8)

data<-as.data.frame(cbind(beneficios,gastos,horas))

# matriz de correlaciones.
cor(data)
plot(data)

# ¿Qué porcentaje de la varianza de los beneficios explicaría una función
# lineal de los gastos en publicidad?
rg1<-lm(beneficios~gastos)
summary(rg1)
# Con la regresion lineal:beneficios=1.4704+1.2849*gastos, se explicaria el 84,75% de la varianza

# ¿Qué porcentaje de la varianza de los beneficios explicaría una función
# lineal de las horas extraordinarias anuales de los empleados?
rg2<-lm(beneficios~horas)
summary(rg2)
# Con la regresion lineal:beneficios=0.3846*horas, se explicaria el 61,02% de la varianza


# Establecer una relación lineal que explique los beneficios 
# mediante los gastos y la horas extraordinarias
rg3<-lm(beneficios~gastos + horas)
summary(rg3)
# IMP El intercepto y las horas no son significativos: no entrarian en el modelo.
# En el caso de plantearlo obligatoriamente con horas, se plantearia:
# beneficios=0.9605+1.0356*gastos+0.1226*horas

# Estandarizo los residuos para ver realmente homogeneidad de varianzas
stand<-function(x){(x-mean(x))/sd(x)}
plot(beneficios,stand(rg3$residuals))
abline(0,0)
# Vemos claro que no hay homosteceidad entre los de abajo y los de arriba.


#Si una empresa destina 900.000 euros a publicidad y sus empleados realizan 500 horas
#extraordinarias al año, ¿cuál sería la estimación de los beneficios de dicha empresa?
# OPCION 1:
1000000*(rg3$coefficients[1]+rg3$coefficients[2]*0.9+rg3$coefficients[3]*0.5)
# OPCION 2:
1000000*(predict.lm(rg3,data.frame(gastos=0.9,horas=0.5)) )


# Hallar los coeficientes de correlación parcial de X1 con X2 y de X1 con X3
library(ggm)
parcor(cov(data))
# Muchisima relacion entre beneficios y gastos, y despreciable entre beneficios y horas

################################### EJEMPLO 5 #############################################
# Se tienen datos de una determinada empresa, en los que se reflejan:
# los precios (en miles) de los cinco productos que se comercializan, así como 
# el coste (en miles) de los factores que intervienen en la elaboración de estos productos. 
# Los datos reflejan que la política de precios de esta empresa está condicionada por
# causas externas a la misma (precios de la competencia, impuestos, precios de promoción, etc.).
producto<-c(14,24,25,45,47)
factores<-c(15,16,15,12,10)

# ¿se puede establecer una relación de tipo lineal que explique 
# el precio del producto en función del precio de los factores?
plot(producto,factores)
rg<-lm(producto~factores)
summary(rg)
# Si muy justo pero SI:
# producto=100.619 -5.119*factores

# nube de puntos
plot(factores,producto, ylab = "Producto", xlab = "Factores")
abline(rg, col="red")

# residuos
plot(predict(rg),rg$residuals) #variabilidad de los residuos entre dos valores
abline(0,0)
# Se ve la homocedesticidad de la varianzas


# Si la empresa piensa comercializar un producto en el que el coste de los factores es de
# 13000 ¿qué pronóstico haría usted para el precio de dicho producto?
# OPCION 1:
1000*(predict.lm(rg,data.frame(factores=13)) )
# OPCION 2:
1000*(rg$coefficients[1]+rg$coefficients[2]*13)
# Serian 34071.43, es decir 34 miles.



