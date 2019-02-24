
#
# Versión que se conecta a la BD de Azure en forma de tabla, no lee los blobs
rm(list=ls())
setwd("D:/Pedro/Master MBIT/Proyecto Master/PROYECTO CODERE/Exploracion Datos/Data Mining")
ficheros <- list.files()

is.installed <- function(paquete) is.element (
  paquete, installed.packages())

if(!is.installed("clickstream"))
  install.packages("clickstream")

require(clickstream)

# Probamos la libreria:

###### Crear el clickStream:
# Creacion a medida de una estructura clickstream
cls <- list(Session1 = c("P1", "P2", "P1", "P3", "P4", "Defer"),
            Session2 = c("P3", "P4", "P1", "P3", "Defer"),
            Session3 = c("P5", "P1", "P6", "P7", "P6", "P7", "P8", "P7", "Buy"),
            Session4 = c("P9", "P2", "P11", "P12", "P11", "P13", "P11", "Buy"),
            Session5 = c("P4", "P6", "P11", "P6", "P1", "P3", "Defer"),
            Session6 = c("P3", "P13", "P12", "P4", "P12", "P1", "P4", "P1", "P3","Defer"),
            Session7 = c("P10", "P5", "P10", "P8", "P8", "P5", "P1", "P7", "Buy"),
            Session8 = c("P9", "P2", "P1", "P9", "P3", "P1", "Defer"),
            Session9 = c("P5", "P8", "P5", "P7", "P4", "P1", "P6", "P4", "Defer"))
cls
class(cls) <- "Clickstreams"
cls
str(cls)

# Creando aleatoriamente, partiendo de 
# 1. los estados de partida
# 2. vector de probabilidades de inicio
# 3. Matriz de transicion para el siguinete estado: por ejemplo: P(P1 a P2)=0.2
# 4. media de longitud y numero de sesiones
cls2 <- randomClickstreams(states = c("P1", "P2"),
                          startProbabilities = c(0.5, 0.5),
                          transitionMatrix = matrix(c(0.2, 0.8, 0.4, 0.6), nrow = 2),
                          meanLength = 10, n = 100)
cls2


# O utilizando un archivo CSV separado por comas:
# Session1,P1,P2,P1,P3,P4,Defer
# Session2,P3,P4,P1,P3,Defer
# Session3,P5,P1,P6,P7,P6,P7,P8,P7,Buy

# cls3 <- readClickstreams(file = "sample.csv", sep = ",", header = TRUE)
# cls3

##### Resumen del ClickStream:
# Podemos ver una tabla de frecuencias y el numero de paginas
summary(cls)

##### Guardar el ClickStream:
writeClickstreams(cls, "sample.csv", header = TRUE, sep = ",")



##### Ajustando a un modelo de cadenas de Markov:
# Podemos calcular los parametros del modelo.
# Si por ejemplo queremos calcular para 2 pasos (k=2) tendremos que indicar order=2.
# Y por lo tanto tendremos una matriz de probabilidades para cada paso --> lag=1 y lag=2 
# y cada uno tendra su propia lambda que sera su coeficiente de transicion calculado por el modelo.

# lambda es el peso de cada paso en el modelo. Y el sumatorio de los lambdas debe ser 1 para los k pasos
# y la matriz de transicion expresa la probabilidad de transicion de cada paso

# En conclusion la probabilidad en el paso siguiente 
# es la suma de las probabilidades de los k pasos anteriores por la matriz correspondiente al paso y el peso(lambda) del paso.

# Lo que hace el modelo es intentar ajustar ese lambda para hacer minima la diferencia entre la realidad y lo predicho
# este ajuste de lambda lo puede solventar o de forma lineal o de forma cuadratica:

mc_lineal <- fitMarkovChain(clickstreamList = cls, order = 2, control = list(optimizer = "lineal"))
mc_lineal
mc_quadratic <- fitMarkovChain(clickstreamList = cls, order = 2, control = list(optimizer = "quadratic"))
mc_quadratic

# En este caso vamos con el optimizador cuadratico las probabilidades de entrada y las probabilidades de final
# y para cada uno de los pasos cada lambda y su matriz de transicion

# Podemos ver sus datos:
str(mc_quadratic)
show(mc_quadratic)
# Estados
mc_quadratic@states
# Numero pasos
mc_quadratic@order
# Transiciones
mc_quadratic@transitions
# Para cada paso
mc_quadratic@transitions$'1'
mc_quadratic@transitions$'2'
# Param. lambda
mc_quadratic@lambda
# Estimador de maxima verosimilitud
mc_quadratic@logLikelihood
# Observaciones
mc_quadratic@observations
# Probabilidades de inicio 
mc_quadratic@start
# Probabilidades de fin
mc_quadratic@end
# Estados de transicion
mc_quadratic@transientStates
# Estados absorbidos
mc_quadratic@absorbingStates
# Probabilidades de estados absorbidos desde estados de transicion
mc_quadratic@absorbingProbabilities

# El summary a parte de darte un estimador de maxima verosimilitud te da dos indicadores mas:
# AIC --> criterio de Akiake
# BIC --> criterio de Bayes
# estos dos sirven para comparar modelos.
summary(mc_quadratic)
# El log-likehood realmente se basa en la suma para los k pasos de las matrices de transicion de cada paso
# por su peso por el score entre la matriz de transicion de dicho paso y el producto entre ella misma y la matriz identidad simetrica.
# sum 1->k(lambda_k * matriz_tansicion_k * log(matriz_tansicion_k/matriz_tansicion_k * 1s))

# Se puede representar este modelo como un grafo dirigido:
plot(mc_quadratic)
plot(mc_quadratic, order=2)

######### Clusterizacion
# Basandose en una matriz de transicion de k pasos con el algoritmo de kmeans vemos las distancias que hay entre secuencias.
# Le indicamos el numero de pasos 1, y el numero de centroides del kmeans 3
clusters <- clusterClickstreams(clickstreamList = cls, order = 1,centers = 3)

# Nos devuelve una lista de listas (que en realidad son a su vez ClickStreams por si quieres volver a ajustar el modelo o volver a clusterizar)
str(clusters)
clusters
# Y podemos ver sus matrices de transicion para cada cluster.
# Con sus estimadores: total suma de cuadrados, suma de cuadrdos de cada cluster, etc...
summary(clusters)


# Obviamente se pueden hacer varias pruebas:
# Con 2 centroides
clusters2 <- clusterClickstreams(clickstreamList = cls, order = 1,centers = 2)
clusters2
# Con varios pasos:
clusters3 <- clusterClickstreams(clickstreamList = cls, order = 4,centers = 2)
clusters3
# Con 0 pasos:
clusters4 <- clusterClickstreams(clickstreamList = cls, order = 0,centers = 2)
clusters4

# Incluso indicandole parametros al kmeans
clusters4 <- clusterClickstreams(clickstreamList = cls, order = 0, centers=2, iter.max=20, algorithm="Lloyd")
clusters4

# Incluso se podria hacer una simulacion con 1 a N centroides a ver cual tiene mayor SS:
clusterPaths <- function(clusterCount){
  clusters <- clusterClickstreams(cls, order = 1, centers = clusterCount)
  withinss = clusters$tot.withinss
  return(withinss)
}

for (clusterCount in 2:8 ) {
  withinss = clusterPaths(clusterCount)
  print (c(clusterCount, withinss))
}


######## Prediccion
# Basandome en el modelo podemos predecir la probabilidad de los siguientes pasos.
# Y derivado de eso de los pasos finales o absorbidos:
pattern <- new("Pattern", sequence = c("P9", "P2"))
resultPattern <- predict(mc_quadratic, startPattern = pattern, dist = 1)
resultPattern
# Lo que quiere decir que si el usuario ha navegado por las paginas P9 y P2 en un 66% de probabilidad navegara 
# a la pagina P1. De momento no tenemos probabilidades de estados absorbidos.

# Podemos tambien indicarle una distancia mayor:
resultPattern_d3 <- predict(mc_quadratic, startPattern = pattern, dist = 3)
resultPattern_d3
# Y te dira que los siguientes movimientos: son a un 7% de probabilidad llegará a posponer la compra.

# De momento no tenemos probabilidades absorbidas porque en nuestro patron no lo hemos indicado
# Tenemos que indicar probabilidades generales de compra y de posponer la compra en nuestra pagina.
# Podemos ver nuestras frecuencias de Buy y de Defer
frequencies(cls)[,"Buy"] # 3 frente a 9
frequencies(cls)[,"Defer"] # 6 frente a 9
# Y mas facilmente
mc_quadratic@end

# Se lo indicamos:
pattern2 <- new("Pattern", sequence = c("P9", "P2"),
               absorbingProbabilities = data.frame(Buy = 0.333, Defer = 0.667))
resultPattern_d3 <- predict(mc_quadratic, startPattern = pattern2, dist = 3)
resultPattern_d3

# Y se puede hacer de forma generica
pattern2 <- new("Pattern", sequence = c("P9", "P2"),
                absorbingProbabilities = as.data.frame.matrix(rbind(mc_quadratic@end)))
resultPattern_d3 <- predict(mc_quadratic, startPattern = pattern2, dist = 3)
resultPattern_d3

# Y con otras distancias
resultPattern_d2 <- predict(mc_quadratic, startPattern = pattern2, dist = 2)
resultPattern_d2
# Esto quiere decir que nuestro usuario tiene una posibilidad de un 5% de comprar con los 2
# pasos que ha dado y que la probabilidad de avanzar a la pagina 3 será del 26% en 2 pasos.

# Si nosotros tambien conocemos a priori las probabilidades de compra o no por un estudio de la webpage
# o estudio de mercado. Y sabemos que el 50% de las personas que hacen logg-in compran, entonces podemos
# calcular para el camino que lleva las probabilidades de compra:
absorbingProbabilities <- c(0.5, 0.5)
sequence <- c("P9", "P2")

for (s in sequence) {
  absorbingProbabilities <- absorbingProbabilities *
  data.matrix(subset(mc_quadratic@absorbingProbabilities, state == s,
  select = c("Buy", "Defer")))
}
# subset(mc_quadratic@absorbingProbabilities, state == 'P9', select = c("Buy", "Defer"))
#          Buy     Defer
# 15 0.3509431 0.6490569

# absorbingProbabilities <- (0.5*0.3509431) y (0.5*0.6490569) = (0.175) y (0.32)

# subset(mc_quadratic@absorbingProbabilities, state == 'P2', select = c("Buy", "Defer"))
#          Buy     Defer
#  8 0.3509431 0.6490569

# absorbingProbabilities <- (0.175*0.3509431) y (0.32*0.6490569) = (0.0612) y (0.21)

# Acumulo probabilidades en cada paso

absorbingProbabilities
#         Buy     Defer
#  0.06158054 0.2106374

# Y divido por el total de la probabilidad, que es la suma de ambas.
# Con ello consigo poner los porcentajes en relacion al 100%
absorbingProbabilities <- absorbingProbabilities / sum(absorbingProbabilities)
absorbingProbabilities
#       Buy     Defer
# 0.2262178 0.7737822

# Por lo tanto si un usuario da esos 2 pasos (P9 y P2) tendra un 6% de probabilidad de compra
# y un 21% de posibilidad de no comprar.

######### Predecir en Base a clusterizacion
# Tambien se puede predecir el comportamiento en base a la distancia de su secuencia con respecto
# a los clusteres.
# Es decir puede predecir en que cluster va a estar ese usuario
clusters <- clusterClickstreams(cls, order = 0, centers = 2) 
clusters
pattern <- new("Pattern", sequence = c("P9", "P2")) 
resultPattern_cluster <- predict(clusters, pattern)
resultPattern_cluster # Cluster 2

pattern <- new("Pattern", sequence = c("P1", "P11")) 
resultPattern_cluster <- predict(clusters, pattern)
resultPattern_cluster # Cluster 1



########################EJEMPLO###############################
# Se deja una semilla
set.seed(123)
# Creamos aleatoriamente los clicks
cls <- randomClickstreams(
  states = c("P1", "P2", "P3", "P4", "P5", "P6", "P7", "Defer", "Buy"),
  startProbabilities = c(0.2, 0.25, 0.1, 0.15, 0.1, 0.1, 0.1, 0, 0),
  transitionMatrix = matrix(c(0.01, 0.09, 0.05, 0.21, 0.12, 0.17, 0.11, 0.2, 0.04,
        0.1, 0, 0.29, 0.06, 0.11, 0.13, 0.21, 0.1, 0,
        0.07, 0.16, 0.03, 0.25, 0.23, 0.08, 0.03, 0.12, 0.03,
        0.16, 0.14, 0.07, 0, 0.05, 0.22, 0.19, 0.1, 0.07,
        0.24, 0.27, 0.17, 0.13, 0, 0.03, 0.09, 0.06, 0.01,
        0.11, 0.18, 0.04, 0.15, 0.26, 0, 0.1, 0.11, 0.05,
        0.21, 0.07, 0.08, 0.2, 0.14, 0.18, 0.02, 0.08, 0.02,
        0, 0, 0, 0, 0, 0, 0, 0, 0,
        0, 0, 0, 0, 0, 0, 0, 0, 0), nrow = 9),
  meanLength = 50, n = 100000)

summary(cls)
# Tenemos 100000 observaciones de las cuales 22087 acaban sin comprar y 77813 comprando

# Ajustamos el modelo que mejor se ajuste y mejor prediga. Para ello comparamos AIC y BIC
maxOrder <- 5
result <- data.frame()
for (k in 1:maxOrder) {
  mc <- fitMarkovChain(clickstreamList = cls, order = k)
  result <- rbind(result, c(k, summary(mc)$aic, summary(mc)$bic))
}
names(result) <- c("Order", "AIC", "BIC")
result
# maxOrder sera el numero de pasos con los que empezaremos a probar e iremos decreciendo
# hasta llegar a 1.
# maxOrder deberia ser la longitud minima de al menos el 50% de la muestra.
# El AIC y BIC mas bajo determina el orden. En este caso k=2 pasos.

# OPCION 1: Entonces ajustariamos el modelo con k=2
mc <- fitMarkovChain(clickstreamList = cls, order = 2)
mc
summary(mc)
plot(mc)

# Podriamos predecir para un camino P1,P4,P6 con distancia 2 y las probabilidades de absorcion que tienen los caminos finales
pattern <- new("Pattern", sequence = c("P1", "P4", "P6"),
               absorbingProbabilities = data.frame(Buy = 0.22, Defer = 0.78))
resultPattern <- predict(mc, startPattern = pattern, dist = 2)
resultPattern
# Los usuarios tienen una posibilidad del 7% de seguir el camino P5,P2
# Y en general tienen una probabilidad del 1.8% de comprar y del 98% de no comprar.

# OPCION 2: Puede que nos interesen analizar por cluster:
# Clusterizamos 5 grupos para el siguiente paso: order=1
# Por motivos de rendimiento nos quedamos con los 1000 primeros, habria que hacerlo para todos.
clusters <- clusterClickstreams(clickstreamList = cls[1:1000], order = 1,centers = 5)
clusters
# Vemos que en el cluster 1 tenemos 117 secuencias
summary(clusters$clusters[[1]])
# De las cuales 35 acaban en compra y 81 no

# Podemos ajustar un modelo exclusivamente para este cluster:
mc_cluster <- fitMarkovChain(clickstreamList = clusters$clusters[[1]], order = 2)
summary(mc_cluster)
# Y predecir el pattern P1,P4,P6 para este cluster:
pattern <- new("Pattern", sequence = c("P1", "P4", "P6"),
               absorbingProbabilities = data.frame(Buy = 0.22, Defer = 0.78))
resultPattern <- predict(mc_cluster, startPattern = pattern, dist = 2)
resultPattern
# Posibles 2 siguientes pasos: P2 y P7 al 15%
# Posibilidad de compra del 4.7%