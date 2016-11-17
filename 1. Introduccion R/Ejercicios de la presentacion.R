#############NOMBRES EN CONFLICTO ###############
# No usar variables reservadas
mean=10
mean
# conflicts() te dice las variables a no usar 
conflicts()

# Limpiar el workspace
rm(list = ls(all = TRUE))

#############AYUDA ###############
#help te ayuda sobre un comando y lo muestra en pestaña help
help(head)
#example te muestra por consola ejemplos del comando
example(tail)
#help.search proporciona comandos relacionados con un tema y los muestra en pestaña
# de ayuda
help.search('sample')
# Para abrir ayuda en HTML
help.start()
#lista de paquetes disponibles
library()
#lista de datos dispoibles
data()

#AREA Y DIRECTORIO DE TRABAJO
#getwd() te dice en que directorio estas, setwd lo establece
getwd()
setwd("D:/Pedro/Master MBIT/Temario/Modulo4/Sesion 38-42 - Analisis Estadistico - Lourdes Servan/Taller R/1. Introduccion R")


#############CARGA FICHEROS EXTERNOS ###############
# Carga y ejecuta un script de R externo
source("miscript_importado.R")
x
y


#############REDIRECCION DE LA SALIDA ###############
#Saca la salida de consola a un fichero salida.txt.
# append=TRUE, va añadiendo; append=FALSE, sobreescribe 
sink("salida.txt", append=TRUE, split=TRUE)

reg<-lm(y~x)
summary(reg)

# Devuelve la salida a la consola
sink()

#borramos txt
unlink("salida.txt")


#############REDIRECCION DE GRAFICOS ###############
# Exportamos el grafico que hacemos a continuacion a PDF
# Para que devuelva el control hay que hacer dev.off()
# Se puede hacer tambien:
# win.metafile()
# png()
# jpeg()
# bmp()
# postscript()
pdf("migrafico.pdf")
hist(x)
dev.off()
unlink("migrafico.pdf")

# Otro ejemplo
z2 <- c(1,2,3,4,5,6)
z3 <- c(6,8,3,5,7,1)
jpeg("migrafrico.jpg")
plot(z2, z3)
plot(z3, z2)
title("Mi primer grafico")
dev.off()
unlink("migrafrico.jpg")

############ REUTILIZACION RESULTADOS ###############
# Se puede calcular una regresion lineal (lm)y se le puede asignar a una variale.
# luego se pueden ver sus atributos como un objeto y luego se puede graficar
lm(mpg~wt, data=mtcars)
# asignando a un valor
fit <- lm(mpg~wt, data=mtcars)
# lo que contiene 
str(fit)
str(fit$residuals)
str(fit$coefficients)
str(fit$fitted.values)
# los atributos y detalles de este objeto se pueden revisar accediendo a la sesccion
# values de la ayuda
help(lm)
# un grafico de los valores contenidos
plot(fit$residuals, fit$fitted.values)



# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
######################## ESTRUCTURAS SIMPLES ##############################
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 

############### NUMEROS ###############
x<-1
y<-2.5
z<-(x+y^3)-.7
z

############### BOOLEANOS ###############
verdad<-TRUE
falso<-FALSE
and<-verdad&falso
and
or<-verdad|falso
or
condicion<-(z<=x+y)
condicion

############### CADENAS ###############
#Concatenar cadenas
cat("cadena!\n","caracteres",sep="")
#Concatenar variables
paste(x,y,collapse = ",")
#Cuenta el tamaño de una cadena
nchar("Hello")

#Separar la variable por el separador ','
# Obviamente X sera un vector de elementos
S="foo, bar, biz"
X=strsplit(S, ",")
X

#Expresiones regulares
#^a--> Palabras que empiezan por a

# Con value por defecto (=FALSE)
#indica la posicion de la palabra que empieza por "a"
s = c("hola","adios")
help(grep)
grep("^a", s)
# Con value=TRUE
# muestra la palabra que empieza por "a"
grep("^a", s, value=TRUE)

#regexpr busca una expresion en una cadena
regexpr("^a","adios")

#gsub remplaza una expresion por otra
gsub(",",";","1,2,3,4")



##################FECHAS ###############

#valor desde el 1/1/1970
2016-09-09

#devuelve la fecha de hoy  
Sys.Date()

#devuelve la fecha y hora actual
date() 


#convierte una cadena a fecha.	
a<-as.Date("01/02/03",format="%y/%m/%d")
a

#muestra la fecha actual mas 10 dias.    
a<-Sys.Date() + 10  
a

#extrae el mes de una fecha. 
format (Sys.Date(), format="%m")

#convierte a un formato determinado.
format (Sys.Date(), format="%A, %d %B %Y")

#secuencia de fechas
dates<-seq(as.Date("2005-01-01"), as.Date("2005-07-01"), by="month")
dates

#Recorre la secuencia
for (i in 1:length(dates)) { 
  #convertir o se visualizara un numero
  d = as.Date(as.character(dates[i]))	
  #paste: concatena texto y cat: concatena e imprime
  cat(paste(d, "\n", sep=""))    
} 

# Convertir lista de cadenas a lista de fechas aplicandoles un formato
strDates <- c("01/05/1965", "08/16/1975")
dates <- as.Date(strDates, "%m/%d/%Y") 

#Convertir de nuevo a cadena
strDates <- as.character(dates) 

# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
######################## ESTRUCTURAS COMPLEJAS ############################
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #

# Operaciones estrusturas complejas
datos = "data"	#carga el conjunto en memoria y se denomina datos.
ls()		#listar lo que existe en memoria.
rm(datos)	#elimina lo que hay en memoria.
ls()
str(x)		#explica la estructura de x.
class(x)		 #clase o tipo de un objeto.
fix(x)		#edita directamente el objeto como texto.
x

############## VECTORES ###############
x = c(1, 2, 3)	#Crear un vector con los datos indicados.
x

x = c(a=1, b=2, c=3)	#Datos y nombres simultaneamente.
x

x = seq(-1, 1, by=.1)		 #Vector con numeros desde -1 al 1 sumando 0.1
x

# Asigno a los nombres de cabecera de x las letras del abecedario
letters
names(x) = letters[1:length(x)] 	
x

rep(1,10)         #Repite el valor n veces.

rep(1:5,3)        #Repite del 1 al 5, 3 veces.

rep(1:5,each=3)   #Repite del 1 al 5 cada elemento 3 veces.

x["a"]        #Muestra el elemento "a"
x[1]	        #Muestra el elemento 1
x[5:10]   		#Muestra los elementos 5 al 10
x[c(5,7:10)] 	#Muestra los elementos: 5 y del 7 al 10
x[-(5:10)]   	#Muestra todos los elementos menos los del 5 al 10
x[ x>0 ]  		#Muestra los valores superiores a 0
x>0 		       #Devuelve vector con el resultado de aplicar la condicion. 
x=rnorm(5, mean=1,sd=2)  #Genera valores aleatorios de distribucion normal
x
sort(x) 		  #Ordena de menor a mayor. 
rev(sort(x)) 	#Orden inverso. 
o = order(x)	#Ordena de menor a mayor, pero devuelve el indice de los elementos  
o
x [1:3]		#Muestra los valores de las posiciones indicadas.
x=c(1,4,4,1,5,1,4,3,3)
unique(x) 	 #Valores que toma la variable sin repetir.

#Consultar los valores iguales a un numero y su posicion.
x.test = c(1, 2, 0, 3, 2, 0, 0, 1)
x.test == 0 
which(x.test == 0) 

#Concatenar dos vectores: 
c(c(1,2,3), c(4,5,6))

#O sumar los elementos de dos vectores: 
c(c(1,2,3) + c(4,5,6)) 

x=c(1,4,4,1,5,1,4,3,3)
#Minimo de un vector
min(x)		             
#Maximo de un vector
max(x)

#Maximo visto al llegar a cada elemento - maximo acumulado
cummax(c(5,5,4,7,7,3))
#Minimo visto al llegar a cada elemento
cummin(c(5,5,4,7,7,3))

#Suma de elementos del vector
sum(x)	
#Longitud del vector
length(x)	
#Redondeo de cada elemento
round(x)
#Redondeo de cada elemento a 2 decimales
round(x, digits=2)

#Devuelve la posicion de cada elemento si estuviese ordenado el vector de menor a mayor.
# Le da como un ranking a los numeros el 1 tendra rank 2, el 4 tendra rank 7, etc..
# Si hay varias posiciones calcula la media.
rank(x)		             

#Calcula Mediana del vactor
median(x)
#Calcula media aritmetica del vector
mean(x)
# Calcula la varianza del vector
var(x)
# Calcula la desviacion tipica del vector
sd(x)	
# Calcula desviacion media normalizada respecto a la mediana
#(mas resistente a outliers/valores extremos que la desviacion estandar) 
mad(x)
#Correlacion por coeficiente de Pearson.
cor(x, x)	 
#Cuartiles
quantile(x)
#Resumen de la variable (Min.,1st Qu., Median, Mean, 3rd Qu., Max.).
summary(x)
#Diferencia entre el primer y tercer cuantil (Q1 y Q3). -- Rango intercuantil
IQR(x)		   

#FACTOR - Es un vector para datos cuantitativos
#Creamos una muestra aleatoria de 20 elementos de la cadena c
var_factor<-sample(c("Yes", "No", "Perhaps"), 20, replace=T)
var_factor
#Creamos un factor del vector y asi conocera los 3 levels o modalidades: No,Yes,Perhaps
x<-factor( var_factor ) 
x

# Sumarizamos (numero de ocurrencias de cada factor). Funciona con ambas 
table(x)
table(var_factor)

# Listamos los valores posibles. 
# Observar que levels para var_factor no funciona (es un vector sin mas)
levels(x)
levels(var_factor)

#Limpia los valores NA de un vector, y devuelve un vector limpio
na.omit(x) 
#Comprueba si es NA (TRUE/FALSE). Y devuelve un vector de bool
is.na(x)

#Valora de infinito muestra -Inf
log(0) 
# Te dice si un valor o un vector es infinito
is.infinite(log(0)) 
is.infinite(x)



############## MATRICES ###############
m<-matrix( c(1,2,3,4), nrow=2 )	
m
m<-cbind( c(1,2), c(3,4) ) 		#Otra forma.
m
m<-rbind( c(1,3), c(2,4) ) 		#Otra forma.
m

#Producto (Fila1: 1*2 + 3*1, Fila2: 2*2 + 4*1 )
m %*% matrix( c(2,1), nrow=2 )

#Determinante (1*4 - 2*3)
det(m)

#Transposicion
t(m) 

#Inversa
solve(m)

#Valores de la diagonal
diag(c(1,2))

#Matriz identidad.
diag(rep(1,2))	               
diag(c(1,1))


############## DATA FRAMES ###############
# Vectores de diferente tipo
peso = c(75, 98, 67, 80) 
altura = c(1.75, 1.89, 1.61, 1.77) 
sexo = c("F","F","M","F")

# Construye un dataframe con los 3 vectores y 
# establece como nombre de columna el de las variables. 
personas = data.frame(peso,altura,sexo) 
personas 
colnames(personas)

# Mostrar la fila n.
personas[2,]
# Mostrar la columna m.
personas[,1]
personas[,"peso"]

# Se puede cambiar colnames y rownames
rownames(personas)<-c("Maria","Jose","Luis","Margarita")
personas

# Valor de fila y columna
personas["Maria","altura"] 
personas[1,2]

# Acceder a una variable --> en este caso accede al vector de columna altura
personas$altura	            

# Condicion de fila--> Solo muestr los que cumplan una condicion para la fila
personas[personas$sexo == 'F',]  
personas[personas$sexo == 'F' & personas$altura > 1.75,]
# Condicion para mostrar solo unas columnas
personas[,c("sexo","altura")]

# Entrar en la estructura personas, te ahorras el $
attach(personas) 
personas[sexo == 'F' & altura > 1.75,] 
table(sexo)	       #Contabiliza ocurrencias por sexo.
table(peso,sexo)	 #Contabiliza ocurrencias por sexo y peso. 
detach(personas)


# Sin nombres de columna. Si no son variables no coge el nombre de columna
df<-data.frame(c(1, 2, 3), c(7, 3, 4)) 		          
df
# A no ser que se lo especifiques tu
df = data.frame(col1 = c(1, 2, 3), col2 = c(7, 3, 4)) 	
df

# Mostrar la columna n
df$col1
df[,1]
df[["col1"]]

#Mostrar rangos
df[1:2,]

# Numero de filas y columnas
dim(df)
#Num filas
dim(df)[1]
#Num columnas
dim(df)[2]


# Nombres de columnas
names(df)
colnames(df)

# Nombres de filas  (Por defecto se enumeran del 1..n).
row.names(df)
rownames(df)

attach(df) 	  # Acceso a las columnas como variables. 
col1 
detach(df)

#Joins
x = data.frame(id = c(1, 2, 3, 4), datosx = c(70, 30, 40, 100)) 
x
y = data.frame(id = c(1, 2, 3, 5), datosy = c(.5, .3, .2, .1)) 
y


#Une los dos conjuntos por la variable comun --JOIN por defecto
# con id que este en ambos conjuntos
merge(x,y)

#Te dice la columna/s que tiene nombre igual en dos df
intersect(names(x),names(y))
# En realidad hace lo mismo que el anterior
merge(x,y, by=intersect(names(x),names(y)))	

#Igual que merge(x,y, by=intersect(names(x),names(y)))
merge(x,y, by="id") 

# Inner Join.
#igual que el anterior, las que esten en ambos df
merge(x, y, by=c("id"))

# Left Join.
# Todaslas que esten en left (x)
merge(x, y, by=c("id"), all.x = TRUE) 
# Right Join.
# Todas las que esten en right (y)
merge(x, y, by=c("id"), all.y = TRUE) 
# Outer Join.
# Todas las combinaciones de x e y
merge(x, y, by=c("id"), all = TRUE) 



## Datos resumidos
z = data.frame(id = c(1, 2,2, 4,5,4), datos1 = c(70, 30, 40, 100,22,33), datos2 =runif (6)) 
z
#Aggrega por el campo id, haciendo la funcion suma. Puedes utilizar mean, etc...
aggregate(z[,2:length(z)], by=list(z$id), FUN=sum ) 
aggregate(z[,2:length(z)], by=list(z$id), FUN=mean ) 

#apply
# Aplica una funcion a un df, por fila (1), o por columna (2)
apply( z, 1, FUN=mean)	       	#Media por fila. Hace la media de cada fila
apply( z, 2, FUN=mean) 	      	#Media por columna. Hace la media de cada columna
apply( z, 1, mean, na.rm=TRUE)	#Media por fila eliminando los NA.
apply( z, 2, mean, na.rm=TRUE)  #Media por columna eliminando los NA.

#Otro ejemplo de apply
#rnorm(20) genera 20 numeros de una distribucion normal
x <- matrix(rnorm(20), ncol=5)
x
#Me defino la funcion f1 que devuelve un unico valor
f1 <- function(x) { 2*x - 25 }
f1
# Aplico la funcion f1 a cada fila
f1.fila <- apply(x, 1, f1)
f1.fila <- apply(x, 1, function(x) {2*x - 25 } )
# Devuelvo la misma df pero aplicandole la funcion. Solo tiene un dato devuelto la f1
f1.fila

#Defino la funcion f2 que devuelve un valor de un vector
f2 <- function(y) { c( mean(y),  var(y))}
# Aplico la f2 a cada fila, por lo tanto me devolvera para cada fila 
# 2 valores (media y varianza), cada una en una fila diferente
apply( x, 1, f2)


#Definir parameters como un bucle.
# parameter sera una matriz de columnas mean y sd y de valores de filas desde 
# el -2 al 2 y desde el 2 al 6
parameters <- cbind(mean = -2:2, sd = 2:6)
parameters
z <- matrix(rnorm(5 * 2), nrow = 2)
z
# data se lo define como z multiplicandole por la columna sd de parameters
# y sumandole la columna mean de parameters
data <- (z * parameters[,2]) + parameters[,1] 
data

# Aplica la media para las 2 filas
apply(data, 1, mean)
# Aplica la sd para las 2 filas
apply(data, 1, sd)


#sapply, lapply
#sapply (x, f(x)) y lapply(x, f(x)) son como apply(x, i, f(x)) 
#sin especificar el indice i=2; 
#Es decir trabajan con columnas aplicando una funcion 
#sapply  - simplifica el resultado a un vector o a una matriz. 
#lapply  - siempre devuelve una lista de atributos cuyo valor es el resultado
data(airquality) 
head(airquality)

#devuelve un vector (cuantos NA hay por columnas (i=2 en apply)).
sapply( airquality, function(x)sum(is.na(x)) )
# o si no totalizas devuelve todo el vector
sapply( airquality, function(x)is.na(x) )

#devuelve una lista (cuantos NA hay por columnas (i=2 en apply)).
# pero cada columna con su valor se convierte en un atributo
lapply( airquality, function(x)sum(is.na(x)) )
# Si no totaliza cada variable que viene de columna tendra un vector de resultados
lapply( airquality, function(x)is.na(x) )


#tapply tapply(x,y,f(x)) 
#calcula la funcion especificada f(x) sobre el objeto x agrupando por los
# valores (categorias) de y

x<-c(19, 14, 15, 17, 20, 23, 19, 19, 21, 18) 
trat<-c(rep("A",5),rep("B",5)) 
x
trat
x.media<-tapply(x,trat,mean) 
x.media
#OBS x y trat tienen que ser del mismo tamaño


############## LISTAS ###############
#Una lista es una coleccion de elementos que pueden ser de distintos tipos y que generalmente
#estan identificados por un nombre. Puede contener vectores, matrices y data frames:
  
Lst <- list(hombre = "Pedro", mujer = "Maria", casados = TRUE, numero.hijos = 3, edad.hijos = c(4, 7, 9))
Lst

#Para obtener los elementos de una lista
Lst$hombre 	
#Para obtener sublistas
Lst[c("hombre", "numero.hijos")] 		
Lst[c(TRUE, FALSE, FALSE, TRUE, FALSE)]
Lst[c(1, 4)]

# Indices negativos para no obtener los datos de las posiciones
Lst[c(-2, -3, -5)]	

h<-list()
h
# Se crea "a" e incluye el valor 1
h[["a"]] = 1 			     
h
# Se crea "b" incluye el valor el vector
h[["b"]] = c(1, 2, 3) 
h
# Borra el elemento "a"
h[["a"]] = NULL 	     
h


############## VALORES NULOS ###############
#Los valores missing se denotan en R con NA o NaN ("not a number").
#NaN es un NA pero su conversion is not true.
#sin embargo NA no es un NaN
# Por lo tanto NA es un valor numerico nulo
# y NaN es un valor numerico que si es NA (null)

x<-c(1,10,NA, 10,3)
is.na(x)
is.nan(x) 

x<-c(1,2,NaN,NA,4) 
is.na(x) 
is.nan(x)

# queremos sustituir el valor 75 por NA en la variable "peso"
# seleccionamos las filas donde "peso" es 75 y modificamos la  columna "peso"
personas[personas$peso==75,"peso"]<-NA # fila "peso==75" y columna "peso"
personas

#En otras ocasiones podemos querer eliminar valores nulos para evitar problemas con algunas funciones
#Las funciones aritmeticas sobre valores NA devuelven NA
x <- c(1,2,NA,3)
mean(x)	            # devuelve NA
mean(x, na.rm=TRUE) # devuelve 2

#OJO: el rm no lo elimina de x
x

#complete.cases() 
#devuelve un vector de valores logicos indicando que filas no tienen valores  nulos
personas
complete.cases(personas)
personas[complete.cases(personas),]

# muestra las filas que tienen valores nulos
personas[!complete.cases(personas),] 


#na.omit()
#devuelve una copia del objeto donde se  han eliminado los valores nulos
# creamos un nuevo objeto sin valores nulos
newdata <- na.omit(personas)
newdata

# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
########################## IMPORTAR/EXPORTAR ##############################
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
############## IMPORTACION DATOS ###############
### Importar fichero csv
setwd("D:/Pedro/Master MBIT/Temario/Modulo4/Sesion 38-42 - Analisis Estadistico - Lourdes Servan/Ficheros Datos")

#Cargamos en un dataframe el fichero Titanic.csv, incluyendo cabecera y con separador ','.
# Los nombres de las columnas seran los del header
mydata <- read.table("Titanic.csv", header=TRUE, sep=",")

# Si indicamos row.names = c("PassengerId"), pasaran a ser los nombres de las filas y perderemos esa columna
#mydata <- read.table("Titanic.csv", header=TRUE, sep=",", row.names = c("PassengerId"))

head(mydata)


### Crear fichero de longitud fija en dir temporal
nom_fichero <- tempfile()
cat(file = nom_fichero, "123456901112131412", "987654987654586745", sep = "\n") # "\n" nueva lÃ?nea
# separar en columnas
fich<-read.fwf( nom_fichero, widths = c(5, 10, 3) )	
nom_fichero
# Editor de R -- OJO si no asignamos de nuevo a fich no se guarda
fich<-edit(fich)


###colClasses
#para indicar el tipo de elemento.
a<-read.table("test.csv", header=TRUE, sep = ",", colClasses="character") 
a
aa<-read.table("test.csv", header=TRUE, sep = ",",colClasses= c("Date" , "character",rep("numeric",10)))
aa

### importar datos txt desde una web
#Si en la web las columnas del fichero txt no tienen nombre:
Web <- "http://people.cst.cmich.edu/lee1c/spss/V16_materials/DataSets_v16/Diseaseoutbreak.txt"
datosWeb1 <- read.table(Web)  # o puede escribirse la direccion directamente dentro
head(datosWeb1)               # un vistazo para ver como son los datos

#Asignar nombre a las columnas del fichero:
Web <- "http://people.cst.cmich.edu/lee1c/spss/V16_materials/DataSets_v16/Diseaseoutbreak.txt"
nombres <- c("ID", "edad", "hijos", "coches", "yates", "motos")
datosWeb2 <- read.table(Web, col.names = nombres)
head(datosWeb2)
write.table(datosWeb2, file = "datosWeb2.xls", sep = ",", col.names = NA, qmethod = "double")


### copy y paste
#copiar preferiblemente de excel (datosWeb3.xls) a mano con Ctr+C y luego lanzamos esta instruccion
datosWeb3 <- read.delim('clipboard')
datosWeb3
# OBS:En R el Ctr+V no funciona hay que lanzar el read.delim



############## EXPORTACION DATOS ###############

df = data.frame(runif(10), runif(10), runif(10)) 
names(df) = c("dato1", "dato2", "dato3")
df

#Con id/nombres de filas: 
write.table(df, file = "dataframe1.csv", sep = ",", col.names = NA, qmethod = "double")	

#Sin id/nombres de filas:
write.table(df, file = "dataframe2.csv", sep = ",", row.names = FALSE, qmethod = "double") 


#para SPSS, SAS o Stata  podemos usar el paquete "foreign" o "Hmisc"
# De momento los comentamos
#library(foreign)
#empleados1 <- read.spss("Empleados_400.sav", to.data.frame = TRUE)
#library(Hmisc)
#empleados2 <- spss.get('Empleados_400.sav')

#para Excel, podemos usar el paquete "xlsx"
library(xlsx)

res <- read.xlsx2("datosWeb3.xlsx", 1)  # read first sheet
head(res)

#Importante!!! R usa por defecto el '.' como separador de decimales, 
#en España se usa ',' como separador de decimales y '.' como separador 
#de miles ¿Como arreglarlo?
#En excel, ir a Herramientas, Internacional..., 
#En R basta con indicarlo cuando se esta exportando:
car_dec<-sapply(cars,function(x){x/5})
car_dec
write.table(car_dec, "cars.csv", dec = ",")   
write.table(car_dec, "cars.txt", sep="\t", dec=",")

setwd("D:/Pedro/Master MBIT/Temario/Modulo4/Sesion 38-42 - Analisis Estadistico - Lourdes Servan/Taller R/1. Introduccion R")
getwd()

# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
######################## LIBRERIAS ############################
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
############## POR DEFECTO ###############
search() #para ver que paquetes hay cargados en un momento dado

# OTROS PAQUETES INTERESANTES
#Amelia: missing data
#arm: regresion y modelos jerarquicos
#astsa: series temporales
#base: Funciones  basicas.
#car:	Algunas aplicaciones y calculos para la regresion.
#caret: Regresion y clasificacion
#DAAG:	Herramientas para el analisis  grafico de datos.
#data.table: extension del dataframe
#dummies: codificar variables a dicotomicas
#foreign: Libreria para importar datos, principalmente de SPSS.
#forecast: series temporales y modelos lineales
#gregmisc:	Herramientas estadisticas varias.
#ggplot2: Graficos
#KernSmooth: Funciones para inferencia no parametrica a traves de estimadores tipo nucleo.
#lars: mÃ©todo lasso en regresion
#lattice:	Libreria con gran cantidad de funciones graficas implementadas.
#lmtest: contraste en regresion
#MASS:  Libreria bastante variada, tiene desde funciones avanzadas para la realizacion de histogramas, hasta transformaciones box-cox y funciones para el analisis discriminante.
#plyr: dividir y combinar datos
#reshape: transformacion de datos entre los formatos de ancho y largo
#RODBC: Importar datos de bases usando ODBC.
#scales: funciones de escala para visualizacion
#scatterplot3D: Libreria para hacer graficos de dispersion en 3D.
#stats: Funciones  estadisticas.
#stepfun: Libreria para representar funciones escalonadas (funciones de distribucion empiricas).
#survival: Funcion de supervivencia


############# LIBRERIA data.table ###########
library(data.table)

DF=data.frame(x=c("b","b","b","a","a"),v=rnorm(5))
DF

DT=data.table(x=c("b","b","b","a","a"),v=rnorm(5)) #como data.frame
DT #los nombres de filas llevan 2 puntos


#convertir data.frame en data.table
CARS=data.table(cars)
head(CARS)

#listado de objetos data.table en memoria
tables()
#IMP: el resultado es una tabla pero no tiene relacion con la funcion "table()"
#MB permite hacer una evaluacion rapida sobre la memoria usada
#y detectar si hay alguna tabla redundante que se pueda eliminar, liberando memoria.
#Algunos usuarios prefieren trabajar con mas de 20 tablas en memoria como una bbdd


#ver el tipo de columnas
sapply(DT,class) 
#Tambien se puede hacer con data-frame
sapply(DF, class)

##KEY
#la clave consiste en una o mas columnas. Estas pueden ser enteros, factores, numerico o caracter.
tables()

DT[2,]
#no es necesario el prefijo data$
DT[x=="b"]

#al no haber nombres de fila no se puede hacer
DT["b",]
cat(try(DT["b",],silent=TRUE)) 

#Añadimos la clave, sera x
setkey(DT,x) 
DT    #los datos aparecen ordenados por la columna x
haskey(DT) #consultamos si dispone de clave
key(DT) #informa cual es la columna clave
attributes(DT) #name var, name row, clase, orden

tables() #ahora informa tambien sobre la clave
DT["b",] #ahora aplica el valor "b" a la clave, igual que DT["b"] (omite coma)
DT["b",mult="first"] # mult: indica "first" o "last" y muestra el primero o ultimo registro con la condicion

##############EJEMPLOS AVANZADOS data.frame, data.table##########

#####Diferencia entre un vector y una busqueda binaria

#Creamos un DF inventado
#Tamaño del grupo
grpsize=ceiling(1e7/26^2) #redondea al valor mas alto
grpsize

tt<-system.time( DF<-data.frame(x=rep(letters,each=26*grpsize), y=rep(letters,each=grpsize),v=runif(grpsize*26^2), stringsAsFactors=FALSE) )
tt

#Se ha creado un DF de 3 columnas y ha insertado 10.000.068 registros en 0.457 segundos,
#insertando 21.881.986 filas por milisegundo

head(DF,3) #muestra los tres primeros registros
tail(DF,3) #muestra los tres ultimos registros
dim(DF)


#extraemos un grupo arbitrario de DF
tt=system.time(ans1 <- DF[DF$x=="R" & DF$y=="h",])  #vector scan
tt

head(ans1,3)


#cuando usamos "==" escaneamos la entrada de "x" 
identical(DT[list("R","h"),], DT[.("R","h"),]) #vemos si los dos conjuntos son iguales



####Haciendo JOINS con data.tables

# sample X and Y data.tables
library(data.table)

X <- data.table(t=1:4,a=(1:4)^2)
setkey(X,t)
X

Y <- data.table(t=3:6,b=(3:6)^2)
setkey(Y,t)
Y
tables()

# Right outer join -- Todas las filas de Y
X[Y]  # default
X[Y,nomatch=NA]  # same as above
merge(X,Y,by="t",all.y=TRUE)  # same as above
identical(X[Y],merge(X,Y,by="t",all.y=TRUE))

# Left outer join - Todas las filas de X
merge(X,Y,by="t",all.x=TRUE)


# Full outer join - Todas las filas de X y de Y
merge(X,Y,by="t",all=TRUE)


# Inner join (nomatch=0, solo las que coinciden entre X e Y)
X[Y,nomatch=0]  
merge(X,Y,by="t")  # la opcion por defecto
merge(X,Y,by="t",all=FALSE)  # poniendo all a FALSE
identical( X[Y,nomatch=0], merge(X,Y,by="t",all=FALSE) )



############# LIBRERIA plyr (Para dividir y combinar datos) ###########
library(plyr)

#Join: Para unir dos data frame. Se utiliza para los mismo casos  que solemos utilizar el sql join.
A <- data.frame(id = c("A", "B", "C", "D"), age = c(24, 25, 17, 19), height = c(1.8, 1.9, 1.75, 1.65))
B <- data.frame(gender = c("M", "M", "F", "F"), id = c("A", "B", "C", "D"))
C <- join(A, B, by ="id", type = "left")
A
B
C


#Summarise: Para obtener datos agrupados.
str(baseball)
summarise(baseball,duration = max(year) - min(year),nteams = length(unique(team)))

#Interesante para hacer calculos en subconjuntos
#la primera letra del nombre de la funcion indica el "input", la segunda el "output".

#ddplyr (entrada: data frame y salida: data frame)
#para cada id, obtenemos su calculo
ddply(baseball, "id", summarise,duration = max(year) - min(year), nteams = length(unique(team)))

#aaply
#divide el array, aplica la funcion y devuelve una matriz
dim(ozone)
str(ozone)
ozone[1,,]
ozone[,1,]
ozone[,,1]
# Calcula la media para todas las latitudes.  Media de todas las lat de todas las long y time.
aaply(ozone, 1, mean)
aaply(ozone, 1, mean, .drop = FALSE)
# Calcula la media para todos los tiempos.  Media de todas los time de todas las long y lat
aaply(ozone, 3, mean)
# Calcula la media para todos las latitudes y longitudes
aaply(ozone, c(1,2), mean)


#Laply: Divide la lista, aplica la funcion apply y devuelve un vector. 
#Para cada elemento de la lista aplica la funcion apply y despues combina los elementos en un array.
laply(baseball, is.factor) #consulta si es factor en cada columna y devuelve un array con la respuesta. 
ldply(baseball, is.factor) #consulta si es factor en cada columna y devuelve un dataframe con la respuesta
v<-colwise(is.factor)(baseball) #consulta si es factor en cada columna
v

#No confundir con lapply. Que hace lo mismo pero divide en lista de variables
lapply(baseball, is.factor)

identity
laply(seq_len(10), identity) #identity devuelve el mismo valor --> function (x) {x}
sapply(seq_len(10), FUN=identity) # igual que sapply o lapply, apply no porque necesita dimension (array o matriz)
laply(seq_len(10), rep, times = 4) # Repite 4 veces la secuencia y luego la junta en una matriz

# Aplica una matriz de 2 col y 2 filas y se lo aplica a la secuencia. Por lo tanto crea una matriz de 3 dimensiones.
matriz3D<-laply(seq_len(10), matrix, nrow = 2, ncol = 2) 
matriz3D


#Maply: llama una funcion con varios argumentos tomando los valores de las columnas
#de un dataframe o array, y devuelve un array
maply(cbind(mean = 1:5, sd = 1:5), rnorm, n = 5)
maply(expand.grid(mean = 1:5, sd = 1:5), rnorm, n = 5)
maply(cbind(1:5, 1:5), rnorm, n = 5)
