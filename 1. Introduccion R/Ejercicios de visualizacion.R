# Limpiar el workspace
rm(list = ls(all = TRUE))

setwd("D:/Pedro/Master MBIT/Temario/Modulo4/Sesion 38-42 - Analisis Estadistico - Lourdes Servan/Taller R/1. Introduccion R")
########Graficos de una variable:##############
table(cars$speed)
#Grafico de barras
barplot(cars$speed)

#Histograma
hist(cars$speed)
#Marca las ocurrencias en el histogrma
rug(cars$speed)
#Histograma con probabilidades
hist(cars$speed, probability = TRUE)
#Marca linea de densidad sobre el ultimo histograma
lines(density(cars$speed),col="red", lwd=3)

#grafico de cajas y bigotes
boxplot(cars$speed)

#Grafico de tallo y hojas
a<-c(1,3,4,3,5,6,2,3,4)
stem(a)



########Graficos de dos variables:##############

fumador = c("S","N","N","S","N","S","S","S","N","S") 
estudio = c(1,2,2,3,3,1,2,1,3,2) 
t = table(fumador,estudio) 
t

# Definimos barras de porcentaje para fumador y para estudio
pie(table(estudio))
pie(table(factor(fumador)))

# Diagrama de barras para la tabla de contingencia entera
barplot(table(estudio,fumador),
        main="Horas de estudio segun fumador/no fumador", 	
        beside=TRUE, legend.text=c("Menos 5 horas","5-10","Más de 10 horas")) 





#Marcar puntos en el plano
punto.espacio = data.frame(x=c(1, 5, 10), y=c(200, 50, 100)) 
punto.espacio
plot(punto.espacio)





#X: tabletas de chocolate que comemos.
#Y: grado de felicidad.
#Regresión lineal : cual será mi grado de felicidad si como 20 tabletas de chocolate.
x.chocolate = c(0, 1, 2, 3, 4, 5, 6, 7, 8, 9) 
y.felicidad = c(4, 7, 10, 6, NA, NA, NA, 1, 2, 1)
plot(x.chocolate, y.felicidad) 
abline(lm(y.felicidad ~ x.chocolate), col="red") 





#Función rlm - Regresion lineal resistente a extremos frente a la regresion lineal normal
library(MASS) 		                                           
x.chocolate = c(0, 1, 2, 3, 4, 5, 6, 7, 8, 9) 
y.felicidad = c(4, 7, 10, 6, NA, NA, NA, 1, 2, 1)
plot(x.chocolate, y.felicidad) 
abline(lm(y.felicidad ~ x.chocolate), col="red") # Linea de regresión .
abline(rlm(y.felicidad ~ x.chocolate), col="red", lty=2) 





#Boxplot para valores continuos con sus categorias o tratamientos
x = runif(10) 		#Valores.
y = c(1, 1, 1, 2, 2, 1, 2, 3, 3, 2) 	#Categorias.
#Creamos un data frame para ver los valores repartidos por categorias
df<-data.frame(cat=y,datos=x)
df
# Mostrmos grafico de cajas de x para las diferentes categorias
boxplot(x ~ y)

# Aplicamos un summary sobre cada fila agrupando por categoria y podemos revisar el boxplot
tapply(x,y,summary)





#Identificar puntos en el espacio
x = runif(100) 
y = runif(100)   
plot(x,y)

#Lanzar la siguiente instruccion y hacer click en el gráfico y 
#devuelve el índice del punto 
identify(x, y, n=1) 		
#Seleccionamos el 61 y lo indicamos en la siguiente instruccion.
# nos da las coordenadas del punto
c(x[61],y[61]) 		

#OBS: Esto vale solamente para coordenadas, no para hist, boxplot, etc...






#Gráficos de alto nivel
#Para relacionar con puntos varias variables se utiliza pairs
pairs(iris[1:4], main = "Anderson's Iris Data -- 3 species",
      pch = 21, bg = c("red", "green3", "blue")[iris$Species],
      oma=c(4,4,6,12))

#Mapas de calor. Relacionan dos variables continuas en funcion de frecuencia 
data(volcano)
filled.contour(volcano, color = terrain.colors, asp = 1)

#Graficos en 3D (x,y,z). Z es el relieve sobre las coordenadas x e y-
z <- 2 * volcano        # Relieve
x <- 10 * (1:nrow(z))   # (S to N) coordenadas de 10 en 10
y <- 10 * (1:ncol(z))   # (E to W) coordenadas de 10 en 10
## Dejamos el border=NA, sin borde, y fondo gris
par(bg = "slategray")
persp(x, y, z, theta = 135, phi = 30, col = "green3", scale = FALSE,
      ltheta = -120, shade = 0.75, border = NA, box = FALSE)


# Variable que almacena por tipo de poblacion y tramo de edad los fallecimientos
data(VADeaths)
str(VADeaths)
barplot(VADeaths)
barplot(VADeaths, beside=T)# De ese modo obtendremos diagramas de barras 				por grupos de edad para cada tipo de población.
boxplot(VADeaths) # Trata todos los datos como una sola variable
boxplot(data.frame(VADeaths)) 	# Un diagrama de caja por cada categoría


#Histograma de una distribucion normal, generada aleatoriamente
x = rnorm(1000) 			# Genera 1000 datos de una normal
hist(x)





# Incluir estilo a las graficas
library(boot)
data(salinity)
attach(salinity)
# El grafico mas simple
plot(dis,sal)
# Un grafico con opciones y parámetros
plot( dis, sal, col.axis='blue', xlab='Caudal', ylab='Salinidad', pch=15,bty='l',
main='Salinidad en función del caudal', col='red')
# Representamos (con doble grosor) la recta de mínimos cuadrados
abline(lm(sal~dis),lwd=2)



#Libreria lattice (tambien valida con grid)
# Contienen otras muchas graficas
data(iris)
library("lattice")
densityplot(iris$Petal.Length)


########################
#crear nuestros propios ejes
# Datos
x <- c(1:10); y <- x; z <- 10/x

# Creamos un margen extra en el lado derecho utilizando par()
par(mar=c(5,4,4,8)+0.1)

# plot x vs. y
# Ponemos los puntos (x,y) en un plano combinados por una linea (type=b), le añadimos color y tipo de punto
# (col y pch) y eliminamos el eje de las y (yaxt=n)
plot(x, y,type="b", pch=21, col="red", yaxt="n", lty=3, xlab="", ylab="")

# add x vs. 10/x=z
# Añadimos nueva linea (type=b) que relaciona x con z
lines(x, z, type="b", pch=22, col="blue", lty=2)

# Añadimos el eje de la x
axis(2, at=x,labels=x, col.axis="red", las=2)
# Añadimos un nuevo eje para Z en el lado derecho
axis(4, at=z,labels=round(z,digits=2), col.axis="blue", las=2, cex.axis=0.7, tck=-.01)
# Añadimos un texto para el eje de la z
mtext("y=10/x", side=4, line=3, cex.lab=1,las=2, col="blue")

# Añadimos el titulo primcipal y las etiquetas a los ejes
title("An Example of Creative Axes", xlab="X values", ylab="Y=X")

abline(v=1)

#### leyendas
attach(mtcars)
# Relacionamos mpg con cyl de mtcars, colocando titulo (main), etiqueta para la y, y color a las cajas (col)
boxplot(mpg~cyl, main="Milage by Car Wieght", yaxs=mtcars$yaxs,xlab="Milage",horizontal=TRUE, col=terrain.colors(3))
# Incluimos leyendas
legend("topright", inset=.05, title="Number of  Cylinders", c("4","6","8"),  fill=terrain.colors(3), horiz=TRUE)



# 4 figuras en forma de matriz
# Gracias a la funcion par(mfrow=) podemos meter n figuras. En este caso 4.
par(mfrow=c(2,2))
plot(wt,mpg, main="Scatterplot of wt vs. mpg")  
plot(wt,disp, main="Scatterplot of wt vs disp")  
hist(wt, main="Histogram of wt")
boxplot(wt, main="Boxplot of wt")


par(mfrow=c(3,1))  
hist(wt)
hist(mpg)  
hist(disp)


# Una figura en la fila 1 y dos figuras en la fila 2
layout(matrix(c(1,1,2,3), 2, 2, byrow = TRUE))  
hist(wt)
hist(mpg)  
hist(disp)


# Una figura en la fila 1 y 2 figuras en la fila 2
# La fila 1 tiene 1/3 de la altura de la fila 2 y
# la columna 2 ¼ de la anchura de la columna 1
layout(matrix(c(1,1,2,3),2,2,byrow = TRUE),widths=c(3,1),heights=c(1,2))
hist(wt)  
hist(mpg)
hist(disp)
detach(mtcars)

