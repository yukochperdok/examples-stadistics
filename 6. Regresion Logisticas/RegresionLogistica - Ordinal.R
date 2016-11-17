# Limpiar el workspace
rm(list = ls(all = TRUE))

# Cargar librarias
require(foreign)
require(ggplot2)
require(MASS)
require(Hmisc)
require(reshape2)

########################FORMULACION PROBLEMA##################
# Cuando la variable a predecir es categorica con K categorias, y se pueden ordenar,
# de menor a payor. podemos trabajar con una logit ordinal. Con probabilidades acumuladas.
# De forma que la P(Y<=1|x)<P(Y<=2|x)<P(Y<=3|x)<....<P(Y<=K|x)=1

# Donde: 
# si x:(x1,x2) tenemos 2 variables ind.
# Y 3 categorias para y (1,2 y 3)
#   log(P(Y<=1|x)) = b01 + b1x1+b2x2
#   log(P(Y<=2|x)) = b11 + b1x1+b2x2
#   log(P(Y<=3|x)) = b21 + b1x1+b2x2
# Por lo tanto lo que se modifica de una a otra es el intercepto.

# Si no se tiene un orden logico tenemos que usar una regresion multinomial.



# Queremos predecir la probabilidad de una variable apply: con 3 categorias
# 1:unlikely, 2:somewhat likely, 3:likely
# En funcion de las variables:
#   1.pared:0/1 indica si al menos alguno de los padres tiene graduado
#   2.public:0/1: 1--> graduado en instituto publico, 0--> en privado
#   3.gpa: media de graduados


dat <- read.dta("http://www.ats.ucla.edu/stat/data/ologit.dta")
head(dat)


########################EXPLORACION VARIABLES#########################
# distribución de las variables
summary(dat)
str(dat)
# Rapidamente vemos que 
# apply, variable a predecir con 3 categorias. Abunda categoria 1.
# pared: Variable 0/1, muchos mas 0 que 1's
# public: Variable 0/1, muchos mas 0 que 1's
# gpa: var. continua de 1.9 a 4

#Revisamos relaciones:
lapply(dat[, c("apply", "pared", "public")], table)
# Mas casos en pared=0 y public=0

ftable(xtabs(~ public + apply + pared, data = dat))
# Segun disminuye pared a 0 y public a 0, va disminuyendo la categoria a 1:unlikely

summary(dat$gpa)
sd(dat$gpa)
# Muy poquita desviacion, la mayoria de los datos se concentran entre: 2.7 y 3.2

#Vamos a ver los 4 en un grafico:
ggplot(dat, aes(x = apply, y = gpa)) +
  geom_boxplot(size = .75) +
  geom_jitter(alpha = .5) +
  facet_grid(pared ~ public, margins = TRUE) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1))

# Tenemos un boxplot para cada combinacion de apply, public(0,1,all) eje horizontal superior
# y pared(0,1,all) eje vertical derecho.
# En cada combinacion podemos ver como se reparten los datos sobre gpa.
# En general se ve que excepto para pared=0, public=0 el resto no tiene mucha dispersion, pero
# si medias diferentes.
# En el caso de pared=0 y public=0 tenemos bastante dispersion de datos de gpa, sobre todo
# en unlikely. Aunque las medias son practicamente iguales.
# Para el all,all, tenemos las medias practicamente iguales. Da la sensacion que gpa no afecta mucho a apply.


########################MODELO LOGARITMICO ORDINAL#########################
# Utilizamos polr--> Proparcional odds logistico modelo
m <- polr(apply ~ pared + public + gpa, data = dat, Hess=TRUE)

#Ver el modelo
summary(m)

# Como vemos ha calculado los coeficientes para cada variable, que es compartida
# para cada categoria a predecir. Junto al valor del beta, viene su desviacion y su 
# estadistico de t (no p-value)

# Luego viene una tabla de interceptos, cada uno para la categoria corespondiente
# Es decir categoria somewhat con referencia a unlikely, y la very likely con 
# referencia a somewhat. SON ACUMULATIVAS. Y sus odds proporcionales.
# Si nos fijamos estos interceptos son puntos de corte que diferencian los saltos
# que da la regresion cuando se pasa de una categoria a otra.

# Al final tenemos unos estadisticos para saber el ajuste del modelo, para compararlo 
# con otros.

# Si Hess se ha marcado como true se ha calculado la matrix de informacion observada
# en el proceso de generacion del modelo
m$Hessian

# Nosotros podemos calcular facilmente los p-values:
(ctable <- coef(summary(m)))
p <- pnorm(abs(ctable[, "t value"]), lower.tail = FALSE) * 2

#Juntamos todo en una tabla
(ctable <- cbind(ctable, "p value" = p))

# Coeficientes pared, public y gpa
#                                   Value Std. Error    t value      p value
# pared                        1.04769010  0.2657894  3.9418050 8.087072e-05
# public                      -0.05878572  0.2978614 -0.1973593 8.435464e-01
# gpa                          0.61594057  0.2606340  2.3632399 1.811594e-02

# Interceptos
#                                   Value Std. Error    t value      p value
# unlikely|somewhat likely     2.20391473  0.7795455  2.8271792 4.696004e-03
# somewhat likely|very likely  4.29936315  0.8043267  5.3452947 9.027008e-08

# IMP:
# Aunque aqui nos aparezcan con expornente los podemos trasformar rapidamente:
# pared:  8.087072e-05 --> 0.0000808
# public: 8.435464e-01 --> 0.843 OJO CON ESTE
# pared:  1.811594e-02 --> 0.0181
# interc1: 4.696004e-03 --> 0.00469
# interc1: 9.027008e-08 --> 0.00000009

# Todos son altamente significativos exceptuando el de public

# OBS2: Solo tendremos 2 interceptos, por que la categoria mas baja en orden, es marcada
# siempre como categoria de referencia en una variabla categorica ordinal.
# Por lo tanto las interpretaciones siempre seran en referencia a esta. En nuestro caso unlikely


########################COMPARAR MODELOS#########################
# Si nos jijamos aqui no tenemos una null desviance y residual desviance
# Lo tenemos que hacer a mano. Para ello planteamos diferentes modelos y comparamos sus
# desviaciones con modelo nulo, aquella que tenga menor desviacion sera
# el modelo optimo para las variables de partida. 
# Es decir aquella que tenga mayor diferencia con el modelo nullo.
mod.null <- polr(apply ~ 1, data = dat)
mod.1 <- polr(apply ~ pared, data = dat)
mod.2 <- polr(apply ~ public, data = dat)
mod.3 <- polr(apply ~ gpa, data = dat)
mod.4 <- polr(apply ~ pared + public, data = dat)
mod.5 <- polr(apply ~ public + gpa, data = dat)
mod.6 <- polr(apply ~ gpa + pared, data = dat)
mod.todos <- polr(apply ~ pared + public + gpa, data = dat)

deviance(mod.null) - deviance(mod.1) #18.41498
deviance(mod.null) - deviance(mod.2) #0.4140527
deviance(mod.null) - deviance(mod.3) #8.605321
deviance(mod.null) - deviance(mod.4) #18.53148
deviance(mod.null) - deviance(mod.5) #8.606772
deviance(mod.null) - deviance(mod.6) #24.14149
deviance(mod.null) - deviance(mod.todos) #24.18041

# Como vemos public no aporta practicamente nada.
# De hecho deberiamos coger el mod.6, que es mas parsimonioso.


####################INTERVALOS CONFIANZA#########################
# Se pueden hallar de dos formas (para el 95% CI):

# 1. A traves de la funcion de verosimilitud (que es lo suyo):
(ci <- confint(m)) 

# 2. Asumiendo normalidad en la variable de salida, y utilizando para ello 
# los errores estandar:
confint.default(m)


# OBS: Son muy parecidos pero es mas seguro el primero.
# OBS2: No son simetricos, aunque normalmente suelen estar muy cercanos a la simetria.
# OBS3: El intervalo de public pasa por 0, el resto no.
# Si el 95% del CI no pasa por 0, la variable es significativa. En este caso nos confirma que public 
# no es significativo y no se debe considerar en el modelo.

########################ODDS RATIOS#########################
# Para interpretar mejor la probabilidad utilizamos los odds ratio mejor
# que los coeficientes en los que solo podemos interpretar el coeficiente de razones
exp(coef(m))

# pared    public       gpa 
# 2.8510579 0.9429088 1.8513972


# Incluyendo CI:
exp(cbind(OR = coef(m), ci))

#               OR     2.5 %   97.5 %
# pared  2.8510579 1.6958376 4.817114
# public 0.9429088 0.5208954 1.680579
# gpa    1.8513972 1.1136247 3.098490

# Omitiendo public que ya hemos dicho que no aporta nada.
# Los otros dos OR nos dan el incremento por unidad de la variable predictora sobre
# LA DIFERENCIA DE UNA CATEGORIA CON EL RESTO DE CATEGORIAS MENORES/MAYORES EN ORDEN.
# Es decir que las categorias de apply se diferencian en 2.85 veces cuando hay un incremento en pared
# en este caso de 0 a 1. Por lo tanto cuando pared=1 (padres con estudios) hay un aumento
# de probabilidad del 185% (2.85 veces mas) de tener un apply very likely frente a no tenerlo (apply somewhat o unlikely)
# Y de igual forma hay un 185% (2.85 veces mas) de decremento de tener un apply unlikely frente a no tenerlo (apply somewhat o very likely)

# CONCLUSION: LAS 3 CATEGORIAS ESTAN SEPARADAS POR 2.85 POR UNIDAD DE INCREMENTO EN PARED.
# DE AHI QUE SE LLAME REGRESION LOGISTICA ORDINAL "PROPORCIONAL". Porque dados cualquier par de categorias contiguas
# sufren el mismo incremento por unidad de incremento en las variables predictoras.
# Totalmente diferente a la regresion multinominal, en la cual la proporcion de una categoria iba con respecto
# a la categoria base.

# Para gpa pasaria igual. Cada vez que gpa aumente en 1 unidad suya, tendriamos 1.85 veces mas entre 
# likely frente al resto y 1.85 veces menos de unlikely frente al resto.

########################TEST PRPOPORCIONALIDAD#########################
# Para asegurarnos de lo dicho antes: cada par de grupos de categorias de salida, que esten ordenados tienen que 
# guardar una proporcionalidad, es decir la categ. mas baja con respecto al resto mas altas guarda la misma prop.
# que la siguiente mas baja con resperto a sus mayores, y asi sucesivamente hasta llegar a la cat. mas alta que
# guardará la misma prop. con todas las mas bajas.

# Existen test para testear la proporcionalidad, pero se les acusa de rechazar bastante la H0, por eso
# se aconseja utilizar esten metodo grafico:
# Basicamente lo que se va a hacer es calular los log odds para en nuestro caso apply>=2 y apply>=3, para cada variable 
# predictora. Si la diferencia de sus log odds es la misma, cumpliramos la proporcionalidad en las categorias.

# IMP: Si no importamos Hmisc, no reconocera la funcion summary como queremos
# Creamos una funcion que dado un conjunto de valores predichos de apply calcula la media por tramos (para cada categoria)
# y le aplica qlogis para recoger el valor del log que transforma probabilidad a log.
sf <- function(y) {
  c('Y>=1' = qlogis(mean(y >= 1)),
    'Y>=2' = qlogis(mean(y >= 2)),
    'Y>=3' = qlogis(mean(y >= 3)))
}

# Hacemos un summary creando una funcion de apply sobre las tres variables dependientes, esto calcula los estadisticos
# descriptores para la formula y se los pasa a la funcion sf, los resultados de sf se los asigna a la tabla s.
(s <- with(dat, summary(as.numeric(apply) ~ pared + public + gpa, fun=sf)))

# Si tu usas sin funcion esto te da calclula la media de apply por los diferentes grupos de variables dependientes
# nosotros se lo hemos aplicado con una funcion propia sf
with(dat, summary(as.numeric(apply) ~ pared + public + gpa))

# Para la media:
#  +-------+-----------+---+-----------------+
#  |       |           |N  |as.numeric(apply)|
#  +-------+-----------+---+-----------------+
#  |pared  |No         |337|1.486647         |
#  |       |Yes        | 63|1.888889         |
#  +-------+-----------+---+-----------------+
#  |public |No         |343|1.536443         |
#  |       |Yes        | 57|1.631579         |
#  +-------+-----------+---+-----------------+
#  |gpa    |[1.90,2.73)|102|1.460784         |
#  |       |[2.73,3.00)| 99|1.525253         |
#  |       |[3.00,3.28)|100|1.560000         |
#  |       |[3.28,4.00]| 99|1.656566         |
#  +-------+-----------+---+-----------------+
#  |Overall|           |400|1.550000         |
#  +-------+-----------+---+-----------------+
# Nos esta dando que la media de apply (1,2 o 3) para pared=0 es 1.48, y para pared=1 es 1.88
# Es decir va agrupando por grupos de las variables dep. y te calula una funcion

# Para sf:
#  +-------+-----------+---+----+-----------+---------+
#  |       |           |N  |Y>=1|Y>=2       |Y>=3     |
#  +-------+-----------+---+----+-----------+---------+
#  |pared  |No         |337|Inf |-0.37833644|-2.440735|
#  |       |Yes        | 63|Inf | 0.76546784|-1.347074|
#  +-------+-----------+---+----+-----------+---------+
#  |public |No         |343|Inf |-0.20479441|-2.345006|
#  |       |Yes        | 57|Inf |-0.17589067|-1.547563|
#  +-------+-----------+---+----+-----------+---------+
#  |gpa    |[1.90,2.73)|102|Inf |-0.39730180|-2.772589|
#  |       |[2.73,3.00)| 99|Inf |-0.26415158|-2.302585|
#  |       |[3.00,3.28)|100|Inf |-0.20067070|-2.090741|
#  |       |[3.28,4.00]| 99|Inf | 0.06062462|-1.803594|
#  +-------+-----------+---+----+-----------+---------+
#  |Overall|           |400|Inf |-0.20067070|-2.197225|
#  +-------+-----------+---+----+-----------+---------+
# Aqui agrupando por pared=0 tenemos que el log para el valor medio de apply>=1 --> Inf
#                                                                      apply>=2 --> -0.37
#                                                                      apply>=3 --> -2.44
# Y asi podriamos seguir con el resto de grupos hechos (pared=1, public y gpa).
# Realmente nosotros estamos calculando aqui los puntos de corte: cutponits de cada variable.
# Es decir el punto de corte donde pasa de categoria 1 a 2 para pared=0. Es -0.37
# El punto de corte donde pasa de categoria 2 a 3 para pared=0. Es -2.44

####COMPROBACION
# Lo cual se puede comprobar viendo el intercepto en una glm de apply>=categoria con respecto a pared
# El intercepto es el punto de corte, es decir el coefficiente que pasa al siguiente categoria.

# ¿Cual es el coefficiente que pasa de cat1 a cat2 para pared=0?
# Calculamos la relacion logistica entre ser mayor o igual a 2 (valor=1 (TRUE)), es decir categorias somewhat y very likely
# y no serlo (valor=0 (FALSE)) es decir categoria unlikely
# en referencia a la var. predictora que deseamos estudiar, en este caso pared.
as.numeric(dat$apply) >= 2

# I(x) es justamente para asegurarte que los valores de x van a ser tratados como booleanos, no se van a convertir.
# pero devuelve el mismo valor 
I(as.numeric(dat$apply) >= 2)

# Lanzamos la glm
glm(I(as.numeric(apply) >= 2) ~ pared, family="binomial", data = dat)
# Coefficients:
#    (Intercept)   pared  
# -->-0.3783       1.1438



# ¿Cual es el coefficiente que pasa de cat2 a cat3 para pared=0?
# Calculamos la relacion logistica entre ser mayor o igual a 3 (valor=1 (TRUE)), es decir categoria very likely
# y no serlo (valor=0 (FALSE)) es decir categorias somewaht likely y unlikely
# en referencia a la var. predictora que deseamos estudiar, en este caso pared.
glm(I(as.numeric(apply) >= 3) ~ pared, family="binomial", data = dat)
# Coefficients:
#    (Intercept)   pared  
# -->-2.441        1.094

# El intercepto de una logictica de 2 categorias con respecto a la 3 en orden ascendente o descendente
# es igual a los interceptos o cutpoints en una logistica ordinal.
# Es decir una logistica ordinal esta planteado como N logisticas normales encuandrando a las catedorias.
# por un orden.

####FIN COMPROBACION
# Volviendo a la comprobacion, si nos fijamos solo en pared:
#  +-------+-----------+---+----+-----------+---------+
#  |       |           |N  |Y>=1|Y>=2       |Y>=3     |
#  +-------+-----------+---+----+-----------+---------+
#  |pared  |No         |337|Inf |-0.37833644|-2.440735|
#  |       |Yes        | 63|Inf | 0.76546784|-1.347074|
#  +-------+-----------+---+----+-----------+---------+
# Tanto para el valor 1 como para el valor 0, la diferencia de los log es practicamente igual:
-0.37833644--2.440735 #2.062399
0.76546784--1.347074 #2.112542
# Entonces se puede concluir que hay proporcionalidad en los puntos de corte para pared. 

# Generalizamos:
s[, 4] <- s[, 4] - s[, 3]
s[, 3] <- s[, 3] - s[, 3]
s
# Indices que gpa por tramos es relativamente igual para sus tramos.
# Sin embargo para public vemos una diferencia bastante grande: entre -2.140211 y -1.371672. Eso
# nos hace pensar que el efecto de public frente a private school es diferente para los diferentes transiciones
# entre categorias de apply=> Por eso no es significativo.
# Mostramos las diferencias en un plot:
plot(s, which=1:3, pch=1:3, xlab='logit', main=' ', xlim=range(s[,3:4]))
# Aqui hay que ver la diferencia entre el triangulo y la cruz debe ser aprox la misma
# para todas las lineas de una variable.
# En pared practicamente la misma
# Para rangos escogidos al azar de gpa muy parecida (somos menos restrictivos)
# Para public muy diferente.

# CONCLUSION: Estudiar la proporcionalidad nos dice si una variable debe entrar en el modelo o no.
# Y a la vez si el modelo ordinal tiene o no tiene sentido para esta variable dependiente.

########################PREDICCION#########################
# Una vez que tenemos claro cual es nuestro modelo y que se cumple la proporcionalidad para un
# modelo logistico ordinal y por ello es ajustable, pordemos predecir la variable dependiente.

#1. Combinamos valores de las diferentes variables (OJO sabemos que public no entraria en nuestro modelo, pero como prueba lo usaremos)
newdat <- data.frame(
  pared = rep(0:1, 200),
  public = rep(0:1, each = 200),
  gpa = rep(seq(from = 1.9, to = 4, length.out = 100), 4))

newdat <- cbind(newdat, predict(m, newdat, type = "probs"))
head(newdat)
# Se devuelve la probabilidad para cada una de las categorias (unlikely, somewhat likely, very likely)

# Cramos dos nuevas variables que seran level (categoria de apply) y probability (la probabilidad asociada al level)
# partiendo de los datos de var. predictoras y los generados en newdata. Con paquete reshape2
lnewdat <- melt(newdat, id.vars = c("pared", "public", "gpa"),
                variable.name = "Level", value.name="Probability")
head(lnewdat)

# Y mostramos en un plot las diferentes combinaciones de var.dep que prob generan por level (color)
ggplot(lnewdat, aes(x = gpa, y = Probability, colour = Level)) +
  geom_line() + facet_grid(pared ~ public, labeller="label_both")


# CONCLUSION: Lo primero que se puede observar es que public no es significativa.
# Sea 0 o 1, su valor de probabilidad en el grafico no varia.
# Por otro lado gpa es un factor importante a la hora de predecir la probabilidad de darse cada
# categoria. A medida que va aumentando gpa, las probabilidades de unlikely van disminuyendo,
# mientras que las de las otras 2 caracteristicas aumentan.
# El aumento y disminucion de estas probabilidades tambien viene dado por pared, de la cual si pared=0
# el aumento en somewhat y very likely es mucho mas pronunciado que en pared=1. Y en el caso de unlikely
# la pendiente es mas o menos igual pero la probabilidad para valores bajos de gpa es mucho mayor
# en unlikely.

# COSAS A TENER EN CUENTA: 
# 1. Es una prediccion perfecta, puesto que con un unico valor de una variable predictoras, 
# tenemos un unico valor de probabilidad.
# 2. Al igual que la multinomial, necesita de un tamaño de muestra alto.
# 3. Combinaciones de las var. predictoras bajo puede acarrear un modelo inestable.

