# Limpiar el workspace
rm(list = ls(all = TRUE))

setwd("D:/Pedro/Master MBIT/Temario/Modulo4/Sesion 38-42 - Analisis Estadistico - Lourdes Servan/Taller R/1. Introduccion R")
# 1. Utiliza la función rep para calcular los siguientes vectores:
# a) (4, 5, 6, 4, 5, 6, . . . , 4, 5, 6)  donde hay 10 ocurrencias de 4,5 y 6. 
rep(4:6,10)
# b) (4, 4, 5, 5, 6, 6,  . . . 4, 4, 5, 5, 6, 6) donde hay 8 ocurrencias de cada uno.
rep(rep(4:6,each=2),8)
help("sort")

# 2. Suponga que se toma una muestra de 20 taxistas que trabajan en el centro
# de la ciudad de Heredia durante setiembre de 2011. 
# ID: Identificación del taxista
# NP: Número de pasajeros transportados por turno
# GC: Gasto diario en combustible (en miles de colones)
# AS: Años de servicio como taxista
# Los datos obtenidos para los 20 taxistas se describen a continuación:

id=c(1:20)
gc=c(8.0,7.6,9.7,12.6,12.9,10.2,14.2,8.4,14.0,13.9,8.7,9.4,7.4,13.4,11.1,13.5,8.6,13.6,9.7,11.6)
np=c(11,9,9,10,8,9,10,8,13,12,12,12,7,11,13,10,10,11,10,11)
as=c(9,7,1,7,1,9,6,5,3,3,5,1,8,4,7,9,1,4,7,5)


# 1.	Crea  un data frame con los datos anteriores.
dataframe=data.frame(id,gc,np,as)
dataframe
# 2.	Modifica los nombres de las variables, de manera que se entienda mejor.
names(dataframe)<-c("código","pasajeros","gastodia","antiguedad")
colnames(dataframe)<-c("código","pasajeros","gastodia","antiguedad")
dataframe

# 3.	Seleccione los valores del primer y último  taxista 
dataframe[c(1,20),]
dataframe[c(1,length(dataframe$código)),]
dataframe[c(1,dim(dataframe)[1]),]
dataframe[dataframe$código==1|dataframe$código==20,]


#  y por otro lado tan sólo, los primeros diez taxistas y ordénalos de menor a mayor GC.

## diez primeros
diez<-dataframe[1:10,]
#Los 10 primeros gastos dia
diez$gastodia
# Vector con la posicion si estuvieran ordenados
# Por ejemplo el 11 ocuparia la posicion 5 en el vector ordenado
order(diez$gastodia)
# Si le pasamos el vercor de posiciones ordenado nos lo ordena
diez[order(diez$gastodia), ]

#todo en una línea
diez[order(diez$gastodia), ]



# 4.	Seleccione los taxistas cuyo número de pasajeros transportados esté entre 1 y 8

subTaxistas1<-dataframe[dataframe$pasajeros>=1 & dataframe$pasajeros<=8,]
subTaxistas1

# y sus años de servicios como taxistas sean mayor que 5
subTaxistas2<-dataframe[dataframe$pasajeros>=1 & dataframe$pasajeros<=8 & dataframe$antiguedad>5,]
subTaxistas2
subTaxistas2<-subset(dataframe,dataframe$pasajeros>=1 & dataframe$pasajeros<=8 & dataframe$antiguedad > 5)
subTaxistas2


# y exportar los resultados a un csv.
write.table(subTaxistas2, file = "D:/Pedro/Master MBIT/Temario/Modulo4/Sesion 38-42 - Analisis Estadistico - Lourdes Servan/Ficheros Datos/subTaxistas2.csv", sep = ",", col.names = NA, qmethod = "double")


# 5.	Construya dos vectores y denótelos GC y NP para guardar los datos de Gasto de combustible y Número de pasajeros.  
GC<-as.vector(gc)
GC
NP<-as.vector(np)
NP
# 6.	Calcule una tabla de frecuencias para las variables  NP y AS.
table(NP)
table(as)

table(NP,as)


# 7.	Calcule la moda, la mediana, la media, desviación estándar, varianza y los cuartiles, 
# para los datos de GC y NP y las medias agrupadas por los años de servicio como taxista 
library("modeest", lib.loc="C:/R/R-3.3.1/library")
#Para gasto combustible
GC
mfv(GC)
summary(GC)
quantile(GC)
sd(GC)
mean(GC)
var(GC)
ric<-quantile(GC,probs = .75)-quantile(GC,probs = .25)
ric

as
table(GC,as)
tapply(GC,as,mean)

#Para num pasajeros
mfv(NP)
summary(NP)
quantile(NP)
sd(NP)
mean(NP)
var(NP)
ric<-quantile(NP,probs = .75)-quantile(NP,probs = .25)
ric

as
table(NP,as)
tapply(NP,as,mean)

detach("package:modeest", unload=TRUE)

# 8. Crea una variable, que se llame gasto, de tal manera que si la variable gasto diario
# es mayor que 10 tome el valor "alto" y si no "bajo". 
gasto<-ifelse(GC>10,"alto","bajo")
gasto
# 9. Representar gráficamente las variables GC y NP, por separado mediante histogramas
# y boxplot y mediante otro gráfico para ver la relación entre variables, cambiando el
# color de los gráficos que vienen por defecto y añadiendo etiquetas a los ejes.
hist(GC, probability = T)
boxplot(GC)
hist(NP)
boxplot(NP)
boxplot(NP~gasto)
plot(GC,NP,main="Distribución del gasto de combustible", 
     xlab="Gasto diario", ylab="Número de pasajeros",
     bg="black",col="red", xlim = c(5,15),ylim = c(5,15))
abline(lm(NP~GC), col="blue")

#######
# 3. Partiendo de los datos que vienen por defecto en "iris". Contiene la longitud y
# anchura del sépalo y del pétalo de las flores iris of iris (setosa, versicolor y virginica).

# a) ¿Cuántos casos hay en los datos?
iris
dim(iris)# 150 casos

# b) ¿Cuántas variables numéricas hay en los datos? Indica cuáles y si son continuas o discretas. 
summary(iris)
str(iris) # 5 variables 4 de ellas numericas

# c)  ¿Cuántas variables categóricas hay y cuáles son? Indica los correspondientes levels (categorías)
str(iris) # 1 variable factor --> Species
levels(iris$Species) #--> "setosa"     "versicolor" "virginica"

#######
# 4. El juego de datos meteo70.txt contiene varias medidas de variable atmosférica entre 1994 y 2007.
# La temperatura media del aire está en la variable X211. 

# a) Lee los datos: 
fname <- "http://stat.ethz.ch/education/semesters/ss2014/regression/uebungen/meteo70.txt" 
d.meteo <- read.table(fname, header=T) 

#Los missings están codificados con el valor 32767. 
#Cambia este valor a NA, elimina los missings y renombra la variable X211 a temp.
head(d.meteo)
dim(d.meteo)

# en una matriz nueva 
meteo<-rep(rep(NA,dim(d.meteo)[1]),dim(d.meteo)[2])
met<-matrix(meteo, nrow=dim(d.meteo)[1], ncol=dim(d.meteo)[2])
dim(met)

for (i in 1:dim(d.meteo)[1]){
  for (j in 1:dim(d.meteo)[2]){
    met[i,j]<-ifelse(d.meteo[i,j]==32767,NA,d.meteo[i,j]) 
  }
}
head(met)


# elimina filas con NA's
suma<-NULL
for (i in 1:dim(d.meteo)[1]){
  s<-sum(ifelse(is.na(met[i,])==TRUE,1,0))
  suma<-c(suma,s) }

mt<-as.data.frame(cbind(met,suma))
head(mt)
subset(mt, mt$suma==0)


# sustituir NA's por valor medio
for (i in 1: dim(met)[1]){
  for (j in 1: dim(met)[2]){
    met[i,j]<-ifelse(is.na(met[i,j])==TRUE,mean(met[,j],na.rm = TRUE),met[i,j])
  }
}
head(met)


# b) Calcula la media de temp para cada mes del año.
aggregate(d.meteo$X211, by=list(d.meteo$MO), mean) 
aggregate(met)

## otra forma
tapply(d.meteo$X211,d.meteo$MO, mean)


# c) Representa gráficamente la media de temp para cada año.
plot(tapply(d.meteo$X211,d.meteo$MO, mean))

#######
#5. 
# a) Crea un vector con 12 integers.
v<-as.vector(seq(1,12))
v

# b) Convierte el vector en una matriz 4x3. Crea la matriz a partir de dos vectores también.
m<-matrix(v,nrow=4,ncol=3)
## a partir de dos vectores
m<-matrix(c(seq(1,6),seq(7,12)),nrow = 4, ncol = 3)
m

v1<-as.vector(seq(1,6))
v2<-as.vector(seq(11,6))
v1v2<-matrix(cbind(v1,v2),nrow=4,ncol=3)
v1v2

# c) Cambia los nombres de las columnas a x, y, z  y los nombres de las filas a a, b, c, d. 
colnames(m)<-c("x","y","z")
row.names(m)<-c("a","b","c","d")
m

# d) Obtén la matriz traspuesta y el determinante de la matriz.
t(m)
t(v1v2)

#determinante (matriz cuadrada)
det(m[1:3,])# la matrix tiene que ser cuadrada
det(v1v2[1:3,])# la matrix tiene que ser cuadrada

# b) Obtén la primera fila de la matriz.
m[1,]

# c) Obtén la tercera columna.
m[,3]

# d) Obtén el valor de la tercera fila y segunda columna.
m[3,2]

#######
# 6. Construir una matriz de 5x6 y suma y calcula la media de los valores de cada columna y de cada fila.
# (apply(X,2 ,sum))
cc<-matrix(seq(0.5,30), nrow=5, ncol=6)
cc
# suma de filas y columnas
apply(cc,1,sum)
apply(cc,2,sum)
sum(cc)
sum_total<-sum(cc)
sum_total
# media de filas y columnas
apply(cc,1,mean)
apply(cc,2,mean)
media_total<-mean(cc)
media_total

#######
# 7. Escribir una función que compute el valor mínimo de cada columna de una matriz de cualquier tamaño 
#    (cualquier número de columnas).

apply(cc, 2, min)

#######
# 8. Calcula la suma de las columnas del objeto cars con las funciones lapply y sapply:
cars
lapply(cars,sum)
sapply(cars,sum)

#######
# 9.	Elaborar gráficos de comparación de cuantiles con identificación de algunos valores de las variables
# del conjunto de datos de cars. (q-q plot)
qqplot(cars$speed,cars$dist)

#######
# 10.	Intenta reproducir en lo posible la siguiente gráfica. Si tienes cualquier duda consulta la ayuda de las funciones plot y par.
n<-seq(1,10)
x<-cos(n)
y<-sin(n)
plot(x)
plot(y)

#######
# 11.	Combinando un loop con if/else clasifica los números del 1 al 10 en 2 grupos:
# Del 1 al 5 pequeño y mayor de 5 grande. De modo que el resultado sea:

# clasif
#[1] "1 pequeño" "2 pequeño" "3 pequeño" "4 pequeño" "5 grande"  "6 grande"  "7 grande"  "8 grande" 
#[9] "9 grande"  "10 grande"

for(i in 1:10){
  tam[i]<- paste(as.character(i),ifelse(i>=5,"grande","pequeño"))
}
tam
