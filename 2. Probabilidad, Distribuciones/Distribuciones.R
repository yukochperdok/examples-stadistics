# Limpiar el workspace
rm(list = ls(all = TRUE))

## EJERCICIOS

# El departamento de Matemática Aplicada propone un examen de test consistente en 25 cuestiones.
# Cada cuestión tiene 5 respuestas listadas, siendo correcta sólo una de ellas. 
# Si un estudiante no conoce la respuesta correcta de ninguna cuestión y prueba suerte, queremos saber: 
# a) ¿Cuál es la probabilidad de responder exactamente 7 respuestas correctas?. 
# b) ¿Cuál es la probabilidad de acertar como máximo 9 respuestas?. 
# c) Si se aprueba el examen cuando se responden correctamente 13 cuestiones, 
#    ¿cuál es la probabilidad de que pase el alumno que ha probado suerte? 
# d) Cuál es el conjunto de números menores posibles de aciertos, con probabilidad de alcanzarse 
#    en torno a 0.95?
# Estamos ante un experimento en el cual se dan dos opciones (éxito o fracaso) a n=25 repeticiones 
# de una prueba (cuestión) que consiste en acertar o no la respuesta adecuada. Puesto que tenemos 
# 25 cuestiones con 5 respuestas listadas la probabilidad de acertar cada una es p=1/5. Por lo tanto
# estamos ante una distribución binomial Bi(n=25, p=1/5=0.2). 





# a) Pr(X=7) 
dbinom(7, size=25, prob=0.2)
#[1] 0.1108419

# b) Pr(X<=9) 
pbinom(9, size=25, prob=0.2)
#[1] 0.9826681

# c) Pr(x>=13) 
pbinom(13, size=25, prob=0.2, lower.tail = FALSE)
#[1] 7.629717e-05

# d) Pr(X < x) 
qbinom(0.95, size=25, prob = 0.2)
#[1] 8

#Comprobamos
pbinom(8, size=25, prob=0.2)
#[1] 0.9532258


###########
# La centralita telefónica de un hotel recibe un nº de llamadas por minuto que sigue una ley de Poisson
# con parámetro lambda=0.5. Determinar las probabilidades: 
# a) De que en un minuto al azar, se reciba una única llamada. 
# b) De que en un minuto al azar se reciban un máximo de dos llamadas. 
# c) De que en un minuto al azar, la centralita quede bloqueada, sabiendo que no puede realizar
#    más de 3 conexiones por minuto. 
# d) Se reciban 5 llamadas en dos minutos. 

# a) P(x = 1)
dpois(1, lambda = 0.5)
#[1] 0.3032653

# b) P(x <= 2)
ppois(2, lambda = 0.5)
#[1] 0.9856123

# c) P(x > 3)
ppois(3, lambda = 0.5, lower.tail = FALSE)
#[1] 0.001751623

# d) P(x = 5)
dpois(5, lambda = 1)
#[1] 0.003065662

#El numero de llamadas maximo que se haria con una probabilidad de 0.70 por minuto: lambda=1
qpois(0.70,1)
# Se realizarian un maximo de 1 llamada




###########
# Hallar la probabilidad de que la resistencia a la compresión simple X, de una probeta de hormigón
# sea mayor que 100 Kg/cm2, sabiendo que la resistencia citada es una variable N(200,40) en Kg/cm2.
pnorm(100, mean=200, sd=40, lower.tail=FALSE)
#[1] 0.9937903
# Si hubieramos tipificado:
pnorm(-2.5, mean=0, sd=1, lower.tail=FALSE)


###########
# El contenido de un bote de cerveza se distribuye normalmente con media 30 cl y 
# desviación típica de 2 cl. 
# a) ¿Cuál es la probabilidad de que un bote determinado tenga más de 33 cl.?
# b) En un conjunto de 6 botes ¿cual es la probabilidad de que el contenido líquido 
#    total sea inferior a un litro y tres cuartos?

# a) x>33
pnorm(33, mean=30, sd=2, lower.tail=FALSE)

# b) x < 1.75 l = 175 cl
# por aditividad la distribución de 6 botes es N(6*30,sqrt(4*6))
pnorm(175, mean=180, sd=sqrt(24), lower.tail=TRUE)

#IMP: No es igual que dividir 175/6 y aplicar pnorm
pnorm(29, mean=30, sd=2, lower.tail=TRUE)

###########
# Crear un data frame compuesta de dos muestras simuladas: ambas con distribución normal de 100 casos 
# una con media 7 y desviación 2, y la otra con media 10 y desviación 4
simula=data.frame(muestra1=rnorm(100,mean=7,sd=2), muestra2=rnorm(100,mean=10,sd=4))

# resumen de cada muestra
library(Rcmdr)
numSummary(simula[,c("muestra1","muestra2")],statistics=c("mean","sd","quantiles"))

# representación de cada muestra
boxplot(simula$muestra1)
boxplot(simula$muestra2)

# dibujar los histogramas
hist(simula$muestra1)
hist(simula$muestra2)

# muestra los gráficos para valorar la normalidad de las muestras
plot(density(simula$muestra1))

# Te indica como sobre los valores N(0,1) se van dispersando los quantiles desde 3 hasta 10
# Si nos fijamos la gran mayoria de los puntos esta en el RIC-->(5.8,8.2) con mediana en 7
qqnorm(simula$muestra1)
qqline(simula$muestra1, col=2)



