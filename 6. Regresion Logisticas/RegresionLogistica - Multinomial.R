# Limpiar el workspace
rm(list = ls(all = TRUE))

# Cargar librarias
require(foreign)
require(nnet)
require(ggplot2)
require(reshape2)

########################FORMULACION PROBLEMA##################
# Cuando tu quieres predecir una variable categorica con mas de 2 categorias,
# no puedes usar la glm binomial, logit o probit.
# Ni tampoco una regresion logistica exacta: erlm
# Debes usar una Regresion Logistica Multinomial:
# Para predecir aquellas variables que tienen N categorias, cada categoria no 
# deja de ser una binomial (0,1) hemos de usar: multinom

# La regresion multinomial es usada para variables de salida categoricas
# en las cuales la relacion de proporciones p/q de la salida son modeladas
# como una combinacion lineal de las variables predictoras
# Cuando la variable de salida tiene un patron de orden (las categorias se pueden ordenar)
# es mas logico hacer una regresion logistica ordinal: polr.



# Supongamos que queremos predecir el tipo de programa de 200 estudiantes: 
#     General, Academico, Vocacional
# en funcion a la puntuacion de escritura (write) y su estatus 
# social economico (ses: baja, alta o media)


mydata <- read.dta("http://www.ats.ucla.edu/stat/data/hsbdemo.dta")
## Primeras filas
head(mydata)


########################EXPLORACION VARIABLES#########################
# distribución de las variables
summary(mydata[,c("ses","write","prog")])
# Rapidamente vemos que 
# write es una variable continua que acumula sus valores desde 45 a 60
# ses es una categorica con 3 categorias (low=47,middle=95,high=58)
# Y prog es una variable de 3 categorias (general=45,academic= 105,vocation=50)

# Vamos como se relacionan las variables
table(mydata$ses, mydata$prog)

with(mydata,do.call(rbind, tapply(write, prog, function(x) c(M = mean(x), SD = sd(x)))))
# La media de puntuacion en la escritura es mayor para el programa academico,
# despues vendria el general y por ultimo el vocacional.
# Ademas la sd para vocacional y general es mucho mas alta que para el academico.
# Da la sensacion que la puntuacion esta muy relacionado con la categoria academica.


# Marcamos como categoria de referencia academic
mydata$prog2 <- relevel(mydata$prog, ref = "academic")
levels(mydata$prog2)
contrasts(mydata$prog2)
#            general vocation
# academic       0        0
# general        1        0
# vocation       0        1
# Tenemos dos dummies: general y vocacional en referencia a academico

########################MODELO LOGARITMICO#########################

test <- multinom(prog2 ~ ses + write, data = mydata)
# Nos muestra como va comprobando a traves de iteraciones la mejor estimacion
# basandose en log-likelihood. Al que como modelo final llega a tener 179.98

# weights:  15 (8 variable)
# initial  value 219.722458 
# iter  10 value 179.982880
# final  value 179.981726

# Mostramos los resultados:
summary(test)

# 1. Coefficientes:

#          (Intercept)  sesmiddle    seshigh      write
# general     2.852198 -0.5332810 -1.1628226 -0.0579287
# vocation    5.218260  0.2913859 -0.9826649 -0.1136037

# Tenemos dos ecuaciones. Una para cada categoria: general y vocacional enfrentada a
# academico.
# Para cada categoria tenemos como se relacionan sus variables con el cociente de razones
# para esa categoria en referencia a la categoria base.
# Es decir:
#    log(P(prog=general)/P(prog=academic)) = b10+b11(ses=mid)+b12(ses=high)+b13write
#    log(P(prog=vocation)/P(prog=academic))= b20+b21(ses=mid)+b22(ses=high)+b23write

# Por ejemplo:
# Cada unidad de incremento sobre write decrementa el cociente de razones de general con
# referencia a academico en un 5%.

# De igual forma para el resto. Teniendo en cuenta que seshigh y sesmiddle se refieren al
# incremento o decremento con relacion a seslow.

# 2. Tenemos tambien los errores estandar para cada beta:
#           (Intercept) sesmiddle   seshigh      write
# general      1.166441 0.4437323 0.5142196 0.02141097
# vocation     1.163552 0.4763739 0.5955665 0.02221996

# 3. Y por ultimo lo bueno o malo que es el modelo:
# Residual Deviance: 359.9635 
# AIC: 375.9635
# De cara a la comparacion con otros modelos. OBS: que la desviacion residual es 
# 2 * log-likelihood conseguido(179.98)

# IMP: Aqui no nos viene su p-value, entonces no sabemos como de significante es cada
# beta. Lo tenemos que calcular manualmente, sabiendo que cada beta sigue una distribucion
# normal.

z <- summary(test)$coefficients/summary(test)$standard.errors
z # Estadistico
p <- (1 - pnorm(abs(z), 0, 1))*2
p # p-value

# Podemos observar que excepto: sesmiddle y seshigh para vocation (que entra raspado 0.09)
# El resto serian significativos:
#           (Intercept) sesmiddle    seshigh        write
# general  0.0144766100 0.2294379 0.02373856 6.818902e-03
# vocation 0.0000072993 0.5407530 0.09894976 3.176045e-07

#    log(P(prog=general)/P(prog=academic)) = b10+b12(ses=high)+b13write
#    log(P(prog=vocation)/P(prog=academic))= b20+b23write


########################COMPARAR MODELOS#########################
# Si nos jijamos aqui no tenemos una null desviance y residual desviance
# Lo tenemos que hacer a mano. Para ello planteamos diferentes modelos y comparamos sus
# desviaciones con modelo nulo, aquella que tenga menor desviacion sera
# el modelo optimo para las variables de partida. 
# Es decir aquella que tenga mayor diferencia con el modelo nullo.
mod.null <- multinom(prog2 ~ 1, data = mydata)
mod.1 <- multinom(prog2 ~ ses, data = mydata)
mod.2 <- multinom(prog2 ~ write, data = mydata)
mod.todos <- multinom(prog2 ~ ses + write, data = mydata)
mod.comb <- multinom(prog2 ~ ses + write + ses * write, data = mydata)

deviance(mod.null) - deviance(mod.1) #16.78297 
deviance(mod.null) - deviance(mod.2) #37.17167
deviance(mod.null) - deviance(mod.todos) #48.2299
deviance(mod.null) - deviance(mod.comb) #51.69222

# Nuestro mejor modelo seria mod.comb, pero ojo tampoco añade muchisima variacion.
# Nos podriamos quedar tambien con mod.todos y ser mas parsimoniosos.

########################RISK RATIOS#########################
# La mejor forma de interpretar las probabilidades es atra ves de los risk ratios
# que no son mas que la exponencial de los coeficientes:
exp(coef(test))
#          (Intercept) sesmiddle   seshigh     write
# general     17.32582 0.5866769 0.3126026 0.9437172
# vocation   184.61262 1.3382809 0.3743123 0.8926116

# El relativo riesgo o probabilidad de un programa general frente al academico
# cada vez que incrementa una unidad en write es del 0.94. Es decir disminuye un 90%.
# O dicho de otra forma el incremento de probabilidad de un programa academigo frente al
# programa general por unidad de incremento de write es de 1.06=1/0.94, i.e 6%

# El riesgo relativo o probabilidad de aumento de pertenecer al programa vocacional
# con respecto a pertenecer al programa academico es del 33% (1.33) si el sujeto pertenece a
# un sector medio frente a pertenecer a un sector bajo.
# Y asi con el resto....

########################PREDICCION#########################
# De igual forma que la logistica binomial aqui podemos predecir, para cada
# categoria y visualizarlo en un ggplot

# 1. fitted(test) te daria los valores estimados de las categorias usando el modelo test
# Es decir las probabilidades de pertenecer a cada categoria para cada fila del dataset de
# entrada
head(pp <- fitted(test))

# 2. Puedes ver la variacion de las probabilidades fijando valores:
# por ejemplo podemos fijar en la media de write y probar con los valores de ses
dses <- data.frame(ses = c("low", "middle", "high"), write = mean(mydata$write))
predict(test, newdata = dses, "probs")
# De esta forma vemos claramente, que fijando la media de write:
#   1. la probabilidad de pertenecer a academico es lineal con el aumento de ses
#   2. la prob. de perten. a general o vocacional es inversamente lineal al aumento de ses.


# 3. Ahora vamos a probar diferentes valores de write para cada categoria de ses:
dwrite <- data.frame(ses = rep(c("low", "middle", "high"), each = 41),
                     write = rep(c(30:70), 3))

# Guadamos las probabilidades estimadas para cada combinacion
pp.write <- cbind(dwrite, predict(test, newdata = dwrite, type = "probs", se = TRUE))

#Calculamos la media de probabilidades dentro de cada categoria de ses.
by(pp.write[, 3:5], pp.write$ses, colMeans)

# pp.write$ses: high
# academic   general  vocation 
# 0.6164315 0.1808037 0.2027648 
 
# pp.write$ses: low
# academic   general  vocation 
# 0.3972977 0.3278174 0.2748849 
 
# pp.write$ses: middle
# academic   general  vocation 
# 0.4256198 0.2010864 0.3732938 

# Vamos a entrentar la probabilidad (distribuida por programa) con write, 
# y vamos a dividirlo por ses en 3 lineas.

# Para ello nos creamos una variable probability, agrupando por ses y write:
lpp <- melt(pp.write, id.vars = c("ses", "write"), value.name = "probability")
head(lpp)

# Luego a partir de este data.frame lpp enfrentamos los valores de write con los 
# valores de probability, y distribuyendo como ses por color.
# la variable "variable" contiene el prog y con ella podemos hacer un facet
ggplot(lpp, aes(x = write, y = probability, colour = ses)) +
  geom_line() +
  facet_grid(variable ~ ., scales="free")


# CONCLUSION:
# Para academic y para vocational esta clarisima la representacion:
# 1. La probabilidad de estar en un programa academico, crece a medida que se tiene mayor
# puntuacion de escritura y a su vez mayor sector economico (aunque entre bajo y medio no hay mucha diferencia)
# 2. La probabilidad de estar en un programa vocacional, decrece a medida que se tiene mayor
# puntuacion de escritura y a su vez mayor sector economico.
# 3. En el caso de los programas generales parece que el efecto es que hasta las 40-50 de puntuacion
# de escritura la probabilidad aumenta, sin embargo a partir de ahi disminuye claramente, en favor de academic.
# Y parece que un ses bajo aumenta claramente la probabilidad, pero entre el ses alto y medio
# no esta claro, dependera de la puntuacion en las escrituras.
