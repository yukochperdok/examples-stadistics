# Limpiar el workspace
rm(list = ls(all = TRUE))

# Cargar librarias
library(aod)
library(ggplot2)
library(Rcpp)
library(corrplot)
library(scatterplot3d)
library("MASS")
library(ROCR)

########################FORMULACION PROBLEMA##################
# Un investigador está interesado en conocer la relación variables como: 
# GRE (Graduate Record Exam puntajes),
# GPA (promedio de calificaciones) 
# rank, el prestigio de la institución de grado
# Y el efecto de la admisión en la escuela de graduados: admit 

# La variable respuesta, admitir / no admitir, es una variable binaria.


mydata <- read.csv("http://www.ats.ucla.edu/stat/data/binary.csv")
## Primeras filas
head(mydata)


########################EXPLORACION VARIABLES#########################
# distribución de las variables
summary(mydata)
# Con esto ya podemos observar que admit, toma valores 0 a 1, y sobre todo mas 0's
# pues su media esta en 0.31
# Las puntuaciones van de 220 a 800 y la puntuacion media es 587 (parece una normal)
# El promedio de calificaciones va desde 2.2 a 4 y su media esta relativamente alta
# 3.3 (tambien parece una normal)
# El prestigio sin embargo parece un factor de 1 a 4

sapply(mydata, sd)
# Con esta instruccion contabilizamos los admit y no admit por rank
xtabs(~admit + rank, data = mydata)

# Aqui podemos ver como se reparten
boxplot(gre~admit,data = mydata)
boxplot(gpa~admit,data = mydata)
# Tanto en uno como en otro el RIC es mas o menos el mismo, pero los admitidos estan
# un poco mas arriba puesto que sus calificaciones son mejores.
# Otra forma de verlo
xtabs(~admit + cut(gre,breaks = 4), data = mydata)
xtabs(~admit + cut(gpa,breaks = 4), data = mydata)

# Podemos representarlos
hist(mydata$gre[mydata$admit==0])
hist(mydata$gre[mydata$admit==1])
qplot(gre,gpa,data = mydata, color=admit, facets = rank~.)
# Vemos claramente que a medida que va aumentando gre y gpa van aumentando los admitidos
# Y de la misma forma con rank a menor rank hay mas admitidos. 
# Es cierto que ha admitidos con un un rank=4 o con bajos gre y gpa pero los menos
# Como ultima observacion vemos que hay muy pocos individuos con un rank=1 que tengan
# calificaciones bajas
M <- cor(mydata)
corrplot(M)


# generamos factor(rank) para tratar como categórica
mydata$rank <- factor(mydata$rank)

# vemos cómo va a codificar la variable rank
contrasts(mydata$rank)
# si quisieramos que otra categoría fuera la referencia
mydata$rank<-relevel(mydata$rank, ref="4")
contrasts(mydata$rank)
# si quisieramos volver a la inicial, hacemos lo mismo:
mydata$rank<-relevel(mydata$rank, ref="1")
contrasts(mydata$rank)


########################MODELO LOGARITMICO#########################

# Antes de hacer una glm podemos ver con un cdplot la funcion de densidad (0,1) 
# de la VD con respecto a cada VI:
cdplot(factor(admit)~gre, data=mydata) # A medida que aumenta gre, aumenta el 1 de admit
cdplot(factor(admit)~gpa, data=mydata) # A medida que aumenta gpa, aumenta el 1 de admit
cdplot(factor(admit)~rank, data=mydata) # A medida que aumenta el rank, aumenta el 0 de admit
# Se ven claramente los 4 rank

# Realmente se deberia haber ido viendo una a una la correlacion de las variables e ir
# incorporandolas, es decir usando un STEPWISE.
# Pero podemos incorporarlas todas de forma educativa para ver los resultados:
mylogit <- glm (admit ~gre + gpa + rank, mydata, family="binomial")
summary(mylogit)

# 1. Desviacion de los Residuos:
# Min       1Q   Median       3Q      Max  
# -1.6268  -0.8662  -0.6388   1.1490   2.0790

# Te indica como estan distribuidos los residuos alrededor del 0 vs fitted del modelo.
# En este caso tenemos una desviacion muy ajustada entre -2 y 2. Este modelo logistico
# esta muy bien ajustado

# 2. Coefficientes:
#               Estimate Std. Error z value Pr(>|z|)    
#(Intercept)   -3.989979   1.139951  -3.500 0.000465 ***
#  gre          0.002264   0.001094   2.070 0.038465 *  
#  gpa          0.804038   0.331819   2.423 0.015388 *  
#  rank4       -1.551464   0.417832  -3.713 0.000205 ***
#  rank2       -0.675443   0.316490  -2.134 0.032829 *  
#  rank3       -1.340204   0.345306  -3.881 0.000104 ***

# Te indica para cada coeficiente (incluido beta0), su estimacion, su desviacion,
# su valor del test de wald y su p-value. OJO Aqui no tenemos los Odds ratio
# Por el z-value y su p-value tenemos cada contraste de hipotesis de si el coeficiente
# es o no es 0. Y ademas tenemos la desviacion de su error.
# 
# En nuestro caso todos los coeficientes tienen desviaciones relativamente bajas y son significativos,
# es decir son <>0 por el test de wald. METODO MAXIMA VEROSIMILITUD.
# PARA INTERPRETAR LOS COEFFICIENTES EN NECESARIO CALCULAR LOS ODDS RATIO:
# Aqui solo podemos decir que gre y gpa aumentaran la razon de probabilidades (que no la probabilidad)
# de ser admitido o no, sin embargo para los rank 4,3 y 2 disminuira frente al rank 1 (disminuira el cociente de razones: p/q)
# En que porcentaje, no lo sabemos todavia.

# 3. Indicadores de ajuste:
# Null deviance: 499.98  on 399  degrees of freedom
# Residual deviance: 458.52  on 394  degrees of freedom
# AIC: 470.52
#
# Aqui nos indican varios indicadores para ajustar el modelo y saber como de bueno es:
# desviacion nula y desviacion de residuos. Te indica la desviacion solo con beta0 y con 
# los coefficientes. Por lo que vemos hay menor desviacion si incluimos nuestros coefficientes
# con lo cual estamos mejorando con respecto a no tenerlos
# Tambien nos indican el indicador AIC (analogo a BIC) que es inv. proporcional a la desviacion de residuos.
# Y cuanto menor es mejor es nuestro modelo.
# Tando la diferencia desv.resid - desv.null como AIC o BIC son indicadores de ajuste, 
# de lo bien que esta nuestro modelo. BONDAD DE AJUSTE


########### INTERVALOS DE CONFIANZA DE LOS COEFICIENTES.##############

# Usaremos "confint" para obtener los intervalos de confianza de los coeficientes (no de los odds).

# Nota: para los modelos logísticos, los intervalos de confianza están basados en la función
# de log-likelihood.--> LOG DE MAXIMA VEROSIMILITUD
confint(mylogit)

# Asumiendo Normalidad en la variable de salida:
# También se pueden obtener utilizando el método predeterminado.- WALD TEST
# Es decir utilizando el estadistico t y la desviacion de errores.
confint.default(mylogit)

# En este caso la diferencia no es mucha, pero normalmente es mas ajustado el primero
# Sobre todo para pocos datos


################### WALD TEST DE LOS COEFICIENTES.###################

#Podemos hacer pruebas para el efecto global de "rank" la función wald.test de la librería "aod"
# el orden en el que los coeficientes están en la tabla es el mismo orden de los términos del modelo.

# (Intercept)          gre          gpa        rank4        rank2        rank3  
# -3.989979         0.002264     0.804038    -1.551464    -0.675443    -1.340204 

#esto es importante porque la función wald.test se refiere a los coeficientes en el mismo orden del modelo.
#para la función wald.test: b son los coeficientes, Sigma es la matriz de varianzas de la covarianza de los
#términos de error y Terms dice a R qué términos en el modelo son los que va a testear, en este caso,
# del 4 al 6 de rank.

# Contrasta si los coeficientes son igual a 0 (Ho: coef.iguales a 0)
# Es decir si rank es significativo para un p-value bajo
wald.test(b=coef(mylogit), Sigma=vcov(mylogit), Terms=4:6)
# Chi-cuadrado de 20.9, con df=3 se asocia con un p-value 0.00011<0.05
# lo que significa que el efecto de rank es estadísticamente significativa.

# Se podria haber hecho de la misma forma con gre:
wald.test(b=coef(mylogit), Sigma=vcov(mylogit), Terms=2)
# X2 = 4.3, df = 1, P(> X2) = 0.038

# O de gre y gpa:
wald.test(b=coef(mylogit), Sigma=vcov(mylogit), Terms=2:3)
# X2 = 15.4, df = 2, P(> X2) = 0.00046

# Es decir, aunque en glm ya te lo hace para todos tu lo puedes explorar por partes.


################# COEFICIENTES - ODDS RATIO.###################
#Se puede exponenciar los coeficientes e interpretarlos como odds-ratios.
#R hará este cálculo. Pedimos a R los exponenciales (exp), y los objetos que
#queremos exponenciar son los coefficientes de mylogit (coef(mylogit)).
#Podemos usar la misma lógica para obtener las odds ratio y sus intervalos de confianza,
#por exponenciación de los intervalos de confianza anteriores.
#Para incluirlo todo en una tabla utilizamos "cbind" para unir columnas
# odds ratio (exponencial de los coeficientes)
exp(coef(mylogit))

# odds ratio para CI:
exp(confint(mylogit))

# TODO JUNTO: odss ratio, 95% IC
exp(cbind(OR=coef(mylogit), confint(mylogit)))

# Ahora si podemos interpretar los odds ratio:

# Para una unidad de incremento en gpa, el cociente de probabilidades de ser admitido
# a la universidad (frente a no ser admitido) aumento por un factor de 2.23 (un 123%). 
# ALTISIMO POTENCIADOR

# Para una unidad de incremento en gre, el cociente de probabilidades de ser admitido
# a la universidad (frente a no ser admitido) aumento por un factor de 1.002 (un 0.002%). 
# BAJISIMO POTENCIADOR

# El cociente de probabilidades de ser admitido a la universidad (frente a no ser admitido) 
# disminuye si tiene rank4 frente a rank1 por un factor de 0.21 (un 21%).
# ALTO DISMINUIDOR
# El cociente de probabilidades de ser admitido a la universidad (frente a no ser admitido) 
# disminuye si tiene rank2 frente a rank1 por un factor de 0.50 (un 50%).
# ALTISIMO DISMINUIDOR
# El cociente de probabilidades de ser admitido a la universidad (frente a no ser admitido) 
# disminuye si tiene rank3 frente a rank1 por un factor de 0.26 (un 26%).
# ALTO DISMINUIDOR

# En el caso de rank, se puede ver los odds de otra forma, cambiar la referencia es tan facil como
# el inverso del odds ratio:
# Por ejemplo para rank4, OR=0.2119375 con referencia a rank1
# Para rank1, OR=1/0.2119375 con referencia a rank4

1/exp(coef(mylogit)["rank4"]) #4.718371
# o tambien con el inverso de beta
exp(-coef(mylogit)["rank4"]) #4.718371

# Y se puede decir que el cociente de probabilidades de ser admitido a la universidad
# aumenta si tiene rank1 frente a rank4 por un factor de 4.71 (un 371%).
# ALTISIMO POTENCIADOR



##################### INFERENCIA (predict) ###################
# Si en vez de interpretar los cocientes de razon entre admitido y no admitido,
# tu quieres saber como se ve afectada la probabilidad con referencia a las variables.
# No puedes estudiarlas por separado como en la regresion lineal (que podiamos independizarlas)
# Es decir: en una regresion lineal para todos los valores de x siempre se ve afectada de forma
# lineal la y en funcion del coeficiente de x, es una recta.
# Pero en una logistica es una curva de probabilidad, se puede ver afectada de una forma
# para los sujetos con gre bajo y alta para gre alto (dependera de las otra variables). No se pueden independizar 
# las variables. Se tienen que estudiar de una forma comun.

# POR ESO SE HACEN PREDICCIONES: Se fijan los valores de j variables y se estudian las otras variables.
# Normalmente se fijan en la media.


# 1. Vamos a empezar prediciendo el valor de probabilidad de los rank, 
# fijando gre y gpa en sus medias, y con rank sus cuatro valores:1, 2, 3 y 4

# En primer lugar creamos un datafame newdata1.
# IMP:Estos objetos deben tener los mismos nombres que las variables en su regresión logística anteriormente 
# (en este ejemplo la media para gre debe llamarse gre, y la de gpa tambien gpa, idem para rank). 

newdata1 <- with(mydata, data.frame(gre = mean(gre), gpa = mean(gpa), rank = factor(1:4)))
newdata1

# Ahora ya con la trama de datos filtrada que queremos, asignamos a una nueva variable rankP
# el valor de las predicciones para el modelo mylogit. Utilizamos predict igual que en la reg.lineal.
# Utilizamos type=response para sacar las probabilidades. Podemos utilizar tambien terms, para sacar
# los fitted values de cada termino: 

newdata1$rankP <- predict(mylogit, newdata = newdata1, type = "response")
# Realmente lo que estamos calculando aqui es p=exp(Z)/1+exp(Z); Z=b0+b1*gre+b2*gpa+b3*rank
# donde gre=mean(gre), gpa=mean(gpa) y probamos con 1,2,3 y 4 para rank.
# Pero estamos calculando las prob de ser admitido para los 4 valores de rank, para unas calif medias de gpa y gre.

newdata1
#     gre    gpa rank     rankP
# 1 587.7 3.3899    1 0.5166016
# 2 587.7 3.3899    2 0.3522846
# 3 587.7 3.3899    3 0.2186120
# 4 587.7 3.3899    4 0.1846684

qplot(newdata1$rank, newdata1$rankP)

# CONCLUSIONES: 
# 1. A medida que rank aumenta, la prob de ser admitido baja.
# 2. La probabilidad de ser admitido para una univ de alto prestigio es del 51% frente
#    al 18% en una univ de bajo prestigio


# OBS: Puedo calcular los coeficientes de rank, y por consiguiente sus odds:
# terms<-predict(mylogit, newdata = newdata1, type = "terms")
# exp(terms[1,"rank"]) # Seria nuestro odd ratio para b3 con un rank=4
# exp(terms[4,"rank"]) # Seria nuestro odd ratio para b3 con un rank=1
# OBS2: Con predict tambien puedo calcular el link, para luego calcular la probabilidad usar plogis
# links<-predict(mylogit, newdata = newdata1, type = "link")
# plogis(links)
# Esto ultimo es igual a rankP


# 2. Se puede hacer para un valor en concreto: p.ej: fila primera
fila1<-mydata[1,c("gre","gpa","rank")]
nuevo$rankp<-predict(mylogit, newdata=fila1, type="response")
nuevo$rankp


# 3. Se puede estudiar tambien la variacion de probabilidad de gre y de gpa.
# Fijando el resto de valores:
newdata1 <- with(mydata, data.frame(gre = c(min(gre),mean(gre),max(gre)), gpa = mean(gpa), rank = factor(4)))
newdata1
newdata1$greP <- predict(mylogit, newdata = newdata1, type = "response")
newdata1
qplot(newdata1$gre, newdata1$greP)

newdata1 <- with(mydata, data.frame(gpa = c(min(gpa),mean(gpa),max(gpa)), gre = mean(gre), rank = factor(4)))
newdata1
newdata1$gpaP <- predict(mylogit, newdata = newdata1, type = "response")
newdata1
qplot(newdata1$gpa, newdata1$gpaP)

# CONCLUSIONES:
# Tanto gre como gpa aumentan la probabilidad de ser admitido a media que aumentan su valor


# 4. De la misma forma se pueden predecir todas las combinaciones del data frame
# Incluso podemos calcular un intervalo de confianza al 95%. para la estimacion

newdata <- cbind(mydata, predict(mylogit, newdata = mydata, type="link", se=TRUE))
head(newdata)
# Te devuelve en el valor fit, la probabilidad (0 a 100) y en se.fit la desviacion.
# Con plogis podemos calcular la probabilidad logistica: 0.5 a 1.0 (50% a 100%)
cbind(newdata$fit,plogis(newdata$fit))

# Luego podemos calcular intervalo de confianza para Z(alfa/2)
z<-qnorm(0.975,0,1, lower.tail = TRUE)

newdata <- within(newdata, {
  prob.pred <- plogis(fit)
  lower.limit <- plogis(fit - (z * se.fit))
  upper.limit <- plogis(fit + (z * se.fit))
})
head(newdata)
# Podemos ver las columnas:
#   prob.pred: probabilidad (admit/no admit) para cada fila de mydata
#   upper.limit: limite superior de probabilidad (0.975)
#   lower.limit: limite inferior de probabilidad (0.025)


# Si intentamos mostrarla en un 2D tendremos problemas porque son 4 dimensiones.
# Y diferentes combinaciones por rank, aunque ordenemos newdata
# Aunque nos deja ver claramente la tendencia de gre, rank e intuir gpa.
# Con sus correspondientes intervalos de prob.

ggplot(newdata[order(newdata$gre,newdata$rank,newdata$gpa),], aes(x = gre, y = prob.pred)) +
  geom_ribbon(aes(ymin = lower.limit, ymax = upper.limit, fill = rank), alpha = .2) +
  geom_line(aes(colour = gpa), size=1) + facet_wrap(~rank)#+ facet_grid(. ~ rank, scales = "free")

# Tendriamos que usar algo como esto:
scatterplot3d(newdata$gpa,newdata$gre,newdata$prob.pred, pch=16,
                   type="h", main="3D Scatterplot", color = newdata$rank, box = F) 

# Otra opcion seria fijar una variable y predecir la probabilidades e intervalos para el resto.
# Pero perdemos informacion.



####################SELECCION DE VARIABLES (STEPWISE)#################
# Al igual que en la regresion lineal, en la logistica se puede utilizar
# Stepwise para seleccionar un modelo lo mas parsimonioso posible y bien ajustado
# Como se decia en la regresion lineal, stepwise va haciendo un añadir variables (forward)
# por mayor correlacion y eliminando hacia atras (backward) por mayor p-value
# En realidad va utilizando como indicador de buen ajuste del modelo AIC



# modelo base (sin variables)
mod.ini<-glm(admit~1, data=mydata, family="binomial")
summary(mod.ini)

# OBS:
# Null deviance: 499.98  on 399  degrees of freedom
# Residual deviance: 499.98  on 399  degrees of freedom
# AIC: 501.98 --> IMP

# modelo con todas las variables
mod.full<-glm(admit~gre + gpa + rank, data=mydata, family="binomial")
summary(mod.full)

# OBS:
# Null deviance: 499.98  on 399  degrees of freedom
# Residual deviance: 458.52  on 394  degrees of freedom
# AIC: 470.52 --> IMP: Menor AIC y menor desviacion: 458.52, frente a 499.98 del modelo nulo
# Se puede acceder a esos datos:
with(mod.full,aic)
with(mod.full,deviance)
with(mod.full,null.deviance)
with(mod.full,df.residual)
with(mod.full,df.null)
indAIC<-with(mod.full,aic)
indLsqd<-with(mod.full,null.deviance-deviance)
indLsqd
diff.df<-with(mod.full,df.null-df.residual)
diff.df


# STEPWISE
mod.stp <- stepAIC(mod.ini, scope = list(lower = mod.ini,upper = mod.full), direction = "both")
# resumen del modelo con menor AIC
mod.stp$anova
# Modelo con las variables minimas
summary(mod.stp)



# BONDAD DE AJUSTE DE MI MODELO: 
# Utilizamos la resta entre el modelo nulo y el modelo ajustado(o cualquier modelo)
# para saber como de ajustado es. Y lo comparamos con una chisq.
# Eso nos devolvera un p-value, si es menor que 0.05 es un buen ajuste.
with(mod.stp, pchisq(null.deviance-deviance,df.null-df.residual, lower.tail = FALSE))
# 7.578194e-08 --> MUY BUEN AJUSTE. Nos dice que nuestro modelo es significativamente mejor que el modelo nulo


# Tambien se puede hacer asi: Si es proximo a 1, buen ajuste
#pchisq(deviance(mod.stp),df.residual(mod.stp)) # 0.9863465


# A veces se nos habla de la verosimilitud (likelihood) de un modelo frente a otro:
# Verosimititud de modelo nulo:
logLik(mod.ini)
# Verosimilitud de modelo ajustado:
logLik(mod.stp) # Cuanto mayor likelihood tenga mas ajustado es



######################### COMPARACION MODELOS ####################
# Puedo comparar 2 modelos: Con Chisq (Wald test) o LRT (likelihood)
mylogit <- glm (admit ~ gre + gpa+ rank, mydata, family="binomial")
mylogit2 <- glm (admit ~ gre + rank, mydata, family="binomial")
anova(mylogit,mylogit2, test="Chisq")
# Me indica que el segundo es mas exacto. De hecho me devuelve el p-value que me devuelve
# el summary de glm para la variable que estamos quitando gpa

# Tambien lo puedo hacer comparando el log verisimilitud: diferencia entre el modelo vacio y con la variable:
anova(mylogit,mylogit2, test="LRT")
# Practicamente el mismo valor. 0.01419
# Y es que Wald test es una aproximacion a likelihood


######################### LINEALIDAD ####################
# Un ultimo apunte: La regresion logistica no necesita normalidad en su VI,
# ni homocedesticidad en sus residuos, lo unico que tiene que cumplir son 2 condiciones:
# 1. Aditividad de las VI: lo cumplen por definicion del glm.
#     Quiere decir que todos los betas suman su efecto para predecir VD. No multiplicativo

# 2. Linealidad de la VD con respecto a las VI (solo las continuas):
#     Quiere decir que cada cambio continuo en una VI afecta linealmente a la VD.

# Se puede ver con esta funcion:
crPlots(mylogit) # Te indica si la relacion es muy o poco lineal para cada VD
# En este caso el grafico para rank no interesa


######################### TABLA DE CONFUSION ####################
# Nosotros tenemos:
#     z= b0+b1x1+b2x2+....+bpxp
#
#     p=exp(Z)/1+exp(z)
#
#     P(Y=1)=p  y  P(Y=0)=1-p

Z<-predict(mod.stp) # OJO sin datos nos devuelve los Z (predichos)
p=(exp(Z)/(1+exp(Z))) # Calculamos las probabilidades de esos Z

prob.frontera<-0.3
# Para los p>prob.frontera -> Y=1
# Para los p<=prob.frontera -> Y=0
y<-ifelse(p>prob.frontera,1,0)

# Enfrento los valores reales de admit con los calculados de mi modelo
# Y los guardo en una tabla de frecuencias
tab=table(y,mydata$admit)
tab

# CUIDADO: columnas VS filas -> fila(real/observado) VS columna(predicho modelo)
# estim\real    0   1
#     0       254  97
#     1        19  30
length(which(y==0 & mydata$admit==1)) # ¿Cuantos eran 1 y he estimado 0? #97
length(which(y==1 & mydata$admit==0)) # ¿Cuantos eran 0 y he estimado 1? #19


# Compruebo la tasa de acierto de mi modelo
tasa.acierto=100*sum(diag(tab))/sum(tab) # Acierta 284 de 400 casos=> 71%
tasa.acierto

# Compruebo especificidad:
# la marca--> He estimado un 1 cuando era un 0 (falsos positivos):
falsos.positivos<-tab[2,1] # 19 casos
verdaderos.negativos<-tab[1,1] # 254 casos 
tasa.esp=100*verdaderos.negativos/(verdaderos.negativos+falsos.positivos)
tasa.esp # 93% de especificidad --> La calidad del modelo en clasificar los negativos

# Compruebo sensibilidad:
# la marca--> He estimado un 0 cuando era un 1 (falsos negativos):
falsos.negativos<-tab[1,2] # 97 casos
verdaderos.positivos<-tab[2,2] # 30 casos 
tasa.sen=100*verdaderos.positivos/(verdaderos.positivos+falsos.negativos)
tasa.sen # 23% de sensibilidad --> La calidad del modelo en clasificar los positivos

# CONCLUSION:
# Nuestro modelo tiene una tasa de especificidad muy alta, se equivoca muy pocas veces con
# el 0, pero tiene una tasa de sensibilidad muy muy baja, se equivoca muchisimas veces en
# el 1. Realmente no es un gran modelo, sobre todo si lo utilizaramos para el sector salud.
# Para un sector de seguros por ejemplo nos podria ayudar a saber aquellos que estan a punto
# de irse, esos 19 que mi modelo ha predicho que si pero todavia no se han ido (en real tienen un 0).
# Aun asi no me ayuda a predecir bien los que se van a ir. Si los que no se han ido, pero esto me ayuda de poco.
# En el caso de la salud me ayudaria muy bien a predecir los que no van a estar enfermos
# pero muy mal a los que si van a estar.

# La realidad es que especificidad y sensibilidad son dos factores opuestos y dependen, a parte
# de lo bien ajustado que este el modelo, de la prob.frontera.
# Por ejemplo si cambiamos prob.frontera=0.3 tendre:
# estim\real    0   1
# 0             83  18
# 1             190 109
#
# y tasa.esp=30.40293 y tas.sen=85.82677
# Muchos mas sensibilidad, ojo aunque la tasa de aciertos baja al 48%.
# ¿Quien decide esa prob.frontera?--> Depende de mi negocio y de que quiero buscar
#
# Para medir esa sensibilidad VS especificidad esiste la curva ROC que basicamente
# Va probando con x prob.frontera y te va diciendo como actuan los 
#  tasa.verdaderos.positivos (Sensibilidad) eje vertical
#                   VS
#  tasa.falsos.positivos (Reciproco de Especificidad) eje horizontal


# Para representarla:
# Calulamos el estimado de admit para nuestro modelo
Z<-predict(mod.stp)
p=(exp(Z)/(1+exp(Z)))
# Creamos un objeto prediccion que tiene todas las metricas de esp, sen, etc..
pr <- prediction(p, mydata$admit)

# Enfrentamos Sensibilidad VS 1-Especificidad
prf <- performance(pr, measure = "tpr", x.measure = "fpr")
plot(prf)
abline(c(0,1))
# Vemos como en un primer momento, para prob.frontera bajas aumenta la sensibilidad
# y va aumentando los falsos positivos tambien. Cuanto mas pegada al eje vertical sea
# mas sensibilidad sin repercutir los falsos positivos --> Zona Sensibilidad

# Luego vendria el punto de curvatura entre sensibilidad y 1-especificidad. En el nuestro
# Es imperceptible porque no es un buen modelo --> Colocacion optima de prob.frontera

# Por ultimo tendriamos otra region donde irian aumentando los falsos positivos y creciendo
# muy poco la sensibilidad. Esto seria para prob.frontera altas --> Zona Especificidad

# CONCLUSION: Cuanto menos se ajuste la ROC a la linea 0,1, mejor sera nuestro modelo.
#             La mejor prob.frontera seria justo en la curva

# Par medir el area bajo la curva de ROC se usa auc.
# Obvio es decir que cuanto mayor sea auc, mejor sera nuestro modelo.
auc <- performance(pr, measure = "auc")
auc <- auc@y.values[[1]]
auc

# CONCLUSION: Hay muchos modelos logisticos buenos que son adaptables y con una buena
# bondad de ajuste, pero el optimo es aquel que incluya las variables interesantes para el
# negocio y aquel que maximice sensibilidad y/o especificidad segun el modelo de negocio
# quiera.


#############################################################################
##########################REGRESION LOGISTICA EXACTA#########################
#############################################################################
# La regresion logistica necesita mas datos que la regresion lineal, por las
# tecnicas que aplica de estimacion (Maxima Verosimilitud).
# Cuando ueremos predecir una variable categorica de 2 categorias y no tenemos suficientes
# datos para trabajar con una logistica es necesario trabajar con una elrm (reg. log. exacta)
require(elrm)

# Supongamos que queremos predecir una variable admitido no admitido pero con
# muy pocos datos, con muy pocos sujetos:
dat <- read.table(text = "
female  apcalc    admit       num
0        0        0         7
0        0        1         1
0        1        0         3
0        1        1         7
1        0        0         5
1        0        1         1
1        1        0         0
1        1        1         6",
                  header = TRUE)

# admit--> si ha sido admitido o no
# female--> si es mujer o no
# apcalc--> Si hizo o no el examen de AP calculo
# num--> frecuencia de pesos

# Este ultimo campo sirve para expandir el dataset y ver la tablas de frecuencia
# Repetimos cada fila por el numero de frecuencia que comenta num
dat <- dat[rep(1:nrow(dat), dat$num), -4]
dat

#################EXPLORACION DE DATOS##################
# Vemos las tablas de frecuencia
xtabs(~female + apcalc, data = dat)
xtabs(~female + admit, data = dat)
xtabs(~apcalc + admit, data = dat)
# Lo vemos en total:
xtabs(~female + apcalc + admit, data = dat)
# Se ve claramente que haber hecho el examen es un indicador de ser admitido
# En cuanto a ser mujer parece que hay un ligero aumento cuando no lo es para ser admitido.

# Observar que para mujer que haya hecho el examen, no tenemos ningun dato de no admitidos
# ¿Sera porque no se admite, un 0 rotundo? o ¿sera porque no tenenmos datos, ausencia de dato?
# La unica opcion es una regresion logistica exacta.

#################MODELO LOGISTICO EXACTO##############
# Para plantear un modelo exacto no nos vale con incluir las variables tal cual
# Hay que contabilizar el numero de admitidos por combinacion de las variables indep:

# 1. Vemos las inteacciones de las varaibles que resultado producen en admit:
x <- xtabs(~admit + interaction(female, apcalc), data = dat)
x

# Para mujer y que no haya hecho el examen: tenemos 5 no admitidos y 1 admitido: total 6

# Luego para cada combinacion de mujer, examen vemos cuantos admitidos fueron 
# y el numero total: admit + no admit = nTrials
cdat <- data.frame(female = rep(0:1, 2), apcalc = rep(0:1, each = 2), 
                           admit = x[2, ], ntrials = colSums(x))
cdat 
# Por ejemplo para hombre que hubiera hecho el examen fueron admitidos 7 de 
# los 10 intentos, por lo tanto 3 no fueron admitidos

# Planteamos el modelo:
# Denotar que para este modelo se utilizan cadenas de iteracion: cadenas de Markov
# Cuantos mas efectos (variables indep) tengamos mayor debe ser las cadenas de iteracion.

# Para una variable utilizaremos 22000 iteraciones, de las cuales 2000 se van a desechar.
# por lo tanto tendremos una cadena de 20000.
# Para 2 variables utilizaremos 5000000. Esto se debe a los efectos de interaccion entre variables.

# Siempre incluimos como variable a predecir la relacion entre admitidos y totales:
# admit/ntrials

# 1. admit/ntrials VS female
m.female <- elrm(formula = admit/ntrials ~ female, interest = ~female, iter = 22000, 
                 dataset = cdat, burnIn = 2000)
summary(m.female)
# Visualizamos histograma e iteraciones de la prueba:
plot(m.female)
# Se identifica como el modelo ha ido probando con valores de female de 2 hasta 10,
# y vemos una dispersion de las iteraciones que ha ido haciendo.
# junto con los valores de admitidos que ha ido obteniendo: histograma

# 2. admit/ntrials VS apcalc
m.apcalc <- elrm(formula = admit/ntrials ~ apcalc, interest = ~apcalc, iter = 22000, 
                 dataset = cdat, burnIn = 2000)
summary(m.apcalc)

# Visualizamos histograma e iteraciones de la prueba:
plot(m.apcalc)


# 3. admit/ntrials VS female y apcalc
# Tenemos en cuenta el aumento en las cadenas para iter y burnin
# Y ademas marcamos r=2 para que las cadenas tomen pequeña frecuencia de pasos.
results<- elrm(formula = admit/ntrials ~ female + apcalc, interest = ~female + apcalc
               , r = 2, iter = 5005000, dataset = cdat, burnIn = 5000)
       
summary(results)

# Results:
#        estimate p-value p-value_se mc_size
# joint        NA 0.00054    0.00001 5000000
# female  1.30065 0.34385    0.01411    1617
# apcalc  3.12394 0.00031    0.00002 1116279

# 95% Confidence Intervals for Parameters

#           lower    upper
# female -1.129684 5.241906
# apcalc  1.089554 6.715551


# En un primer luegar tenemos los resultados de la estimacion.
# joint es la interacion entre ambas variables, y no tiene estimador.
# su p-value contrasta la hipotesios nula de si ambas coeficientes son 0.
# El p-value_se es el error estandar para el p-value por la ejecucicon iterada.
# Y por ultimo el mc_size es el tamaño de la cadena de Markov, hay que fijarse que para 
# la iteracion se a utilizado muchisimo mas que para las variables.
# Esto es porque cada cadena necesita ser lo suficientemente grande para poder hacer
# una prediccion estable.
# Se puede observar que el reparto de la cadena ha perjudicado a la estimacion de female,
# por lo tanto para tener una buena estimacion de female deberia incrementarse iter y burnin
# Para apcalc la cadena es mas larga de lo neesario: bastaria con 20000.
# Los estimadores para female y apcalc son 1.30065 y 3.12394 respectivamente,
# pero el primero de ellos aun no es significativo para la longitud de la cadena.

# Tras presentar los resultados se indica un CI para cada parametro.



# Visualizamos histograma e iteraciones de la prueba de ambas variables:
plot(results) # OJO Tarda muchisimo en ejecutar.
