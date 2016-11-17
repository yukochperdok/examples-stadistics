# Limpiar el workspace
rm(list = ls(all = TRUE))
library(scatterplot3d)
library(Rcmdr)


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



######################EJEMPLO 4: VARIABLES CATEGORICAS#########################
# Para tratar las variables categoricas tenemos que identificar de que tipo estamos hablando:
#   1. Var. con 2 categorias: Lo suyo es representarla con 0 y 1.
#      Ausencia y existencia. Y se puede estudiar toda ella. 
#      Si es ordinal se puede expresar como un factor y tratarla como una discreta mas.

# Por ejemplo la variable am de mtcars: Automatico/Manual
# 0: Automatico / 1: Manual
# Siempre trabajariamos con am o con factor (am)
am.f<-factor(am)

fit.am<-lm(mpg~am)
summary(fit.am)
#o
fit.am<-lm(mpg~am.f)
summary(fit.am)
# En am.f1 es igual que am porque solo tiene las categorias 0 y 1.
# Lo que esta diciendo es que un coche manual incrementa 7.245 veces el consumo frente
# a un automatico.

#   2. Var. con N categorias: 
#         a) Ordinal: Se puede trabajar con ella de forma unica: Haciendo factor y 
#            tratandola como una discreta

# Por ejemplo el corte en los diamantes:
library(ggplot2)
data(diamonds)
attach(diamonds)
# Mostramos datos
head(diamonds)
str(cut)
# Ord.factor w/ 5 levels "Fair"<"Good"<Very Good < Premium < Ideal
levels(cut)
# [1] "Fair"      "Good"      "Very Good" "Premium"   "Ideal"
# Los levels no son numericos eso nos devuelve algo inteligible:
fit.cut<-lm(price~cut)
summary(fit.cut)
#cut.L,cut.Q,cut.C,cut^4????

# SOlucion tratarla como una variable discreta:
cut.num<-as.numeric(cut)
tapply(price,cut.num,summary)
boxplot(price~cut.num)
fit.cut<-lm(price~cut.num)
summary(fit.cut)

# Otra opcion es dicotomizarla y tratarla por separado
contrasts(cut) <- contr.treatment(5) 
# Defines una tabla de contrastes par los valores:
#Fair      0 0 0 0
#Good      1 0 0 0
#Very Good 0 1 0 0
#Premium   0 0 1 0
#Ideal     0 0 0 1
# La categoria referencia es Fair y comprobaremos la relacion de los demas
# con price teniendo como referencia Fair.

fit.cut<-lm(price~cut)
summary(fit.cut)
#  cut2         -429.89     113.85  -3.776 0.000160 *** --> Good con ref a Fair
#  cut3         -377.00     105.16  -3.585 0.000338 *** --> V.Good con ref a Fair
#  cut4          225.50     104.40   2.160 0.030772 *   --> Premium con ref a Fair
#  cut5         -901.22     102.41  -8.800  < 2e-16 *** --> Ideal con ref a Fair

# Por ejemplo aqui sacamos como conclusion que el precio aumenta en 225 de un corte 
# Premium con respecto al Fair.


#         b) No ordinal: Se tiene que dicotomizar. Creando N-1 variables dummies (0,0,0,...,1)
#            y dejando una categoria (0,0,0...,0) como referencia.
#            Se tratan siempre por separado, y se analizan con respecto a la categoria de referencia.

# Recogemos csv publicado y vamos a analizar su variable race.
# La cual puede tener 4 categorias posibles: (1 = Hispanic, 2 = Asian, 3 = African American and 4 = Caucasian)

hsb2 = read.table('http://www.ats.ucla.edu/stat/data/hsb2.csv', header=T, sep=",")
head(hsb2)
str(hsb2$race) # Es un numerico de valores 1 a 4

# 1. Factorizamos:
hsb2$race.f = factor(hsb2$race, labels=c("Hispanic", "Asian", "African-Am", "Caucasian"))
hsb2$race.f
# Vemos la media de write por nuestro nuevo factor
tapply(hsb2$write, hsb2$race.f, mean)
# Podriamos explorar mucho mas la variable, bloxplot, plot, cor, etc...

#2. Tenemos 4 categorias, no ordinales: Creamos una matriz de categorias y
# se la asignamos como contraste al factor
contrasts(hsb2$race.f) = contr.treatment(4)
print(hsb2$race.f)

# Planteamos la regression
summary(lm(write ~ race.f, hsb2))
# race.f2       11.542      3.286   3.512 0.000552 ***
# race.f3        1.742      2.732   0.637 0.524613    
# race.f4        7.597      1.989   3.820 0.000179 ***

# CONCLUSIONES: 
# La categoria African-Am, no es significativa. No podemos sacar conclusiones de ella
# Los escritores Asiaticos escriben 11,5 veces mas que los Hispanos
# Los escritores Caucasicos escriben 7.5 veces mas que los Hispanos
# No se puede sacar una relacion de la variable total raza, porque no es Ordinal


# Existen otros tipos de codificacion en vez de dummies:
# 1.Codificacion simple: Es practicamente igual que la codificacion dummy, puesto 
# que cada categoria se compara con una categoria de referencia. La diferencia es el intercepto
# de la regresion en este caso es la media de las medias de las celdas. En la de dummy
# el intercepto corresponde a la celda media del grupo de referencia.

# Creamos la matriz de contrastes:
c<-contr.treatment(4)
#Repatimos las 4 categorias, generando una matriz de 1/4
my.coding<-matrix(rep(1/4, 12), ncol=3)
# Restamos a la matriz de contrastes la matriz de reparto
my.simple<-c-my.coding
my.simple
# Tendramos 1/k como referencia y -(k-1)/k como valor "1"

#Aplicamos esos contrastes al factor y miramos la regresion
contrasts(hsb2$race.f)<-my.simple
summary(lm(write~race.f, hsb2))

# Si nos fijamos tenemos los mismos coeficientes excepto el Intercepto por lo que comentabamos
# al principio. En este caso el intercepto es: 51.6784 = (46.45833 + 58 + 48.2 + 54.05517)/4
# Es decir la media de las medias de cada una.




# 2. Codificacion dummy ad-hoc: Si queremos hacer una codificacion dummy manualmente
# porque nos interesa otra dummy de referencia no Hispanico, por ejemplo Asiatico:
contrasts(hsb2$race.f) = matrix(byrow = T,data=c(c(0,0,1),c(0,0,0),c(0,1,0),c(1,0,0)), ncol = 3, nrow = 4)


hsb2$race.f<-relevel(hsb2$race.f, ref="Asian")
contrasts(hsb2$race.f)

summary(lm(write ~ race.f, hsb2))
# race.fHispanic   -11.542     3.286  -3.512 0.000552 *** --> Hispanico con ref Asiatico
# race.fAfrican-Am -9.800      3.388  -2.893 0.004251 **  --> Afri.Amer con ref Asiatico
# race.fCaucasian  -3.945      2.823  -1.398 0.163803     --> Caucasico con ref Asiatico




# 3. Codificacion ortogonal: Para variables que son numericas se puede usar este metodo
# que realmente te dice si un factor esta relacionado linealmente, cuadraticamente o cubicamente
# con la veriable dependiente:

hsb2$readcat<-cut(hsb2$read, 4, ordered = TRUE)
# Te divide en 4 grupos la variable discreta read y la agrupa por levels:
# Levels: (28,40] < (40,52] < (52,64] < (64,76]
table(hsb2$readcat)

tapply(hsb2$write, hsb2$readcat, mean)
#  (28,40]  (40,52]  (52,64]  (64,76] 
# 42.77273 49.97849 56.56364 61.83333

# El contraste polinomial se decine como contr.poly(4) para 4 categorias:
contr.poly(4)
# Donde L es lineal, Q cuadratico y C cubico

#Asignamos a la variable la matriz poligonal
contrasts(hsb2$readcat) = contr.poly(4)
summary(lm(write ~ readcat, hsb2))
# readcat.L    14.2587     1.4841   9.607   <2e-16 ***
# readcat.Q    -0.9680     1.2679  -0.764    0.446    
# readcat.C    -0.1554     1.0062  -0.154    0.877 

# CONCLUSION: El factor numerico y ordinal readcat se relaciona linealmente con
# write, ni cuadraticamente ni cubicamente.
# Por cada readcat hay 14,25 write.

