# Limpiar el workspace
rm(list = ls(all = TRUE))
############ESTIMACIÓN POR INTERVALO###########################

#Se ha obtenido una muestra de 25 alumnos de una Facultad para estimar la calificación media
#de los expedientes de los alumnos de la Facultad. Se sabe por otros cursos que la desviación
#típica de las puntuaciones en dicha Facultad es de 2.01 puntos y la media fue de 4.9 puntos.

z<-qnorm(c(0.95),mean=0,sd=1,lower.tail=TRUE)
# Es lo mismo que esto:
z<-qnorm(c(0.05),mean=0,sd=1,lower.tail=FALSE)

z
n<-25
mean<-4.9
sd<-2.01 

izq<-mean-z*sd/sqrt(n)
drch<-mean+z*sd/sqrt(n)

izq    #4.238769
drch   #5.561231




# Un administrador de una planta industrial generadora de energia desea estimar por intervalo,
# la cantidad de carbon que se consumio por termino medio semanalmente el año pasado.
# Toma una muestra de 10 semanas y ve que su media fue de 11.400 toneladas y su desviacion
# de 700 toneladas. ¿Cual seria el intervalo de confianza al 95% para el consumo medio del 
# año pasado?
n<-10
media_muestral<-11400
desv_muestral<-700
# Quiero hallar al 95% un intervalo para la media poblacional
# n<30, y desconozco la desv.poblacional->t student con df=10-1,alfa=0.025
t<-qt(c(0.025), df=9, lower.tail=FALSE)  #2.262

izq<-media_muestral-t*(desv_muestral/sqrt(n))
drch<-media_muestral+t*(desv_muestral/sqrt(n))

izq    #10899.25
drch   #11900.75




# Durante un año y medio las ventas han disminuido de manera coherente en los 1500
# establecimientos de una cadena de comida rapida. Una empresa de consultoria ha determinado que
# el 30% de una muestra de 95 establecimientos tiene claros signos de mala administracion
# construir un intervalo de confianza al 95% para esa porcion?

# La probabilidad de mala administracion es de 0.3 -> Prob. de exito (p)
# Se puede inferir esto a toda la poblacion
# Se sigue una Binomial de n=95,p=0.3 --> con n>30 podemos usar N(0,1)

n<-95
p_muestral<-0.3
z <- qnorm(c(0.025), mean=0, sd=1, lower.tail = FALSE)

p_pbl_izq<-p_muestral-z*sqrt((p_muestral*(1-p_muestral))/n)
p_pbl_drch<-p_muestral+z*sqrt((p_muestral*(1-p_muestral))/n)


p_pbl_izq  #0.2078499
p_pbl_drch #0.3921501 

############CONTRASTE DE MEDIA PARA UNA MUESTRA####################

#Las puntuaciones en un test que mide la variable creatividad siguen, en la población general
#de adolescentes, una distribución Normal de media 11.5. En un centro escolar que ha implantado 
#un programa de estimulación de la creatividad una muestra de 30 alumnos ha proporcionado las siguientes puntuaciones:
# 11, 9, 12, 17, 8, 11, 9, 4, 5, 9, 14, 9, 17, 24, 19, 10, 17, 17, 8,23, 8, 6, 14, 16, 6, 7, 15, 20, 14, 15.
#A un nivel de confianza del 95% ¿Puede afirmarse que el programa es efectivo?

x<-c(11,9,12,17,8,11,9,4,5,9,14,9,17,24,19,10,17,17,8,23,8,6,14,16,6,7,15,20,14,15)
# sd poblacional desconocida, parametro a estimar media poblacional (n=30 se podria haber cogido una Z)
# Se leindica como condicion H1(alternativa) H1:media>11.5

t.test(x, alternative="greater", mu=11.5)  
#Devuelve el valor del estadistico t=0.99
# Tambien te indica un p-value=0,1634 > 0.05 nuestra confianza--> Fuera del Rechazo (Se acepta H0)
# Te confirma que H1 es cierta, porque dice que la media es mayor que 11.5
# Te devuelve un intervalo de confianza para la media
# Te hace una estimacion de la media = 12.46667

# Confirmamos nosotrs buscando el valor nulo de la distribucion t
qt(c(0.05), df=29, lower.tail=FALSE)
# o tambien qt(c(0.95), df=29, lower.tail=TRUE)


############CONTRASTE DE MEDIA PARA DOS MUESTRAS RELACIONADAS################

#Para comprobar la utilidad de una técnica de enriquecimiento motivacional un investigador pasa una prueba de
#rendimiento académico a una muestra de 16 sujetos. Después aplica su técnica de enriquecimiento y tras ello, 
#vuelve a pasar la prueba de rendimiento. Los resultados fueron los siguientes
# 8     12   14    11   16     6     11     9    10     10      19     12      17       8       13     12
# 9     16   23    21   17   10     14     8    11     12      19     16      16     13       17     11
#A un nivel de confianza del 95%, ¿Podemos rechazar que los rendimientos académicos son iguales antes que después
#frente a la alternativa de que se produce una mejora?

a<-c(8,12,14,11,16,6,11,9,10,10,19,12,17,8,13,12)
b<-c(9,16,23,21,17,10,14,8,11,12,19,16,16,13,17,11)
d<-b-a

mean(d)
sd(d)
# Se hace la H1:media<>0 para una t de student--> Con dos colas -> alfa=0.025
t.test(d, alternative="two",mu=0) 
# t=3.41, 
# Nos dice que la media no es igual a 0 -->se rechaza H0. 
# valor de media muestra 2.81, con un intervalo de confianza al 95% de [1.05,4.56]

# Comprobamos valor nulo de la distribucion para 15 grdos de libertad
qt(c(0.025), df=15, lower.tail=FALSE) # 2.13145
# Tambien
qt(c(0.975), df=15, lower.tail=TRUE)


############CONTRASTE DE MEDIA PARA DOS MUESTRAS INDEPENDIENTES###############
#Las notas obtenidas en Análisis de Datos de 5 individuos elegidos al azar del grupo T1 y de 6 individuos,
#elegidos también al azar, del grupo T2 son las siguientes:
T1<-c(10,6,4,5,4) 
T2<-c(4,8,6,6,2,3)
#¿Puede concluirse a un nivel de confianza del 95% que las puntuaciones medias de ambos grupos son iguales?
#o por el contrario que hay diferencia entre ambas.

mean(T1)
mean(T2)
sd(T1)**2
sd(T2)**2

# H1: dif medias<>0 por t student
t.test(T1,T2, alternative="two",mu=0) 
# t=0.6722 
# Aunque dice que la diff de medias es diferente a 0--> NO ES CORRECTO
# Puesto que el estadistico cae dentro de la region de aceptacion --> se acepta H0, 
# valor de medias 5.8 y 4.83

# Comprobamos la region de aceptacion
qt(c(0.025), df=9, lower.tail = FALSE)

# O tambien:
qt(c(0.975), df=9, lower.tail = TRUE)


############CONTRASTE DE PROPORCIONES################

#Entre los pacientes con cáncer de pulmón, el 90% o más muere generalmente en el espacio de tres años.
#Como resultado de nuevas formas de tratamiento, se cree que esta tasa se ha reducido. 
#En un reciente estudio sobre 150 pacientes diagnosticados de cáncer de pulmón, 128 murieron en el espacio de tres años.
#¿Se puede afirmar que realmente ha disminuido la tasa de mortalidad al nivel alfa = 0.1?

# H0:p>=0.90 y H1:p<0.90
proporcion<-c(128/150)
desv<-sqrt(0.9*0.1/150)

z=(proporcion-.9)/desv
z

z.teorico<-qnorm(0.1, mean=0, sd=1, lower.tail=TRUE)
z.teorico

#Otra opcion seria con la cola hacia el otro lado:
qnorm(0.9, mean=0, sd=1, lower.tail=FALSE)

# Rechazamos H0 ya que z>z.teorico


###Solución tratada con Chi-cuadrado
# Las probabilidades se pueden probar con la Chisq
# La prop.test le damos un vector de datos exitosos (x) y un vector de todos los datos (n)
# Y la probabilidad p0 con la que queremos comparar.
# La Chisq no funciona bien con observados menores que 5.
# Si incluimos en x un valor menor que 5 tenemos que poner correct=TRUE--> Asi agrupa por la correccion de Yates
# Sino ponemos correct=FALSE

# H1:p<0.9 --> 128/150<0.9 
prop.test(x=128,n=150,p=0.9,alternative="less",correct=FALSE)
# Te indica que p<0.9
# Te da un p-value= 0.02838 relativamente bajo con lo cual 
# el estadistico Chisq entrara en zona de rechazo --> Se rechaza H0



############CONTRASTE DE PROPORCIONES independientes#############

#Se desea contrastar si fuman más las chicas que los chicos.
#Se toma una muestra de 20 chicas y se encuentra que fuman 12.
#En una muestra de 18 chicos fuman 8. Realizar el contraste con ??=0.01?
n1<-20
n2<-18
p1<-12/20
p2<-8/18

#calculo de p (n1p1+n2p2)/(n1+n2)
p<-(n1*p1+n2*p2)/(n1+n2)
p

#calculo de Z (p1-p2)/raiz(p*(1-p)*((1/n1)+(1/n2))
Z<-(p1-p2)/sqrt(p*(1-p)*((1/n1)+(1/n2)))
Z   #0.9589

#valor de Z al 99%:
qnorm(0.005, mean = 0, sd = 1, lower.tail = FALSE) #2.57
# o:
qnorm(0.995, mean = 0, sd = 1, lower.tail = TRUE)  

#Utiliza Chi-cuadrado
x<-c(12,8)    #valores de éxitos
n<-c(20,18)   #valores de muestras
prop.test(x,n, alternative="two",correct=FALSE, conf.level=0.1)
# No se corrige por yates por que las frecuencias obs. son 12 y 8
# Te devuelve un p-value muy alto, mayor que 0.001 
# entonces el estadistico caera en zona de aceptacion --> Se acepta H0
# Las proporciones son iguales en hombres que en mujeres


# De forma avanzada se puede hacer con ajuste de bondad.
# Planteando H0:p1=p2=1/2
chisq.test(x=c(12,8),p=c(1/2,1/2), correct = FALSE, simulate.p.value = 0.1)
# Me sale un valor bastante alto 0.5057 con lo cual puedo aceptar H0.
############CONTRASTE NO PARAMETRICOS - Bondad de ajuste Chisq##########

#Prueba de Bondad de ajuste de Chi-cuadrado con BINOMIAL

#Sea X la cantidad de goles por partido en el mundial de fútbol de Francia 98.
#Contrastar si se distribuye como una poisson
k<-5
x<-c(1,2,3,4,5)
pro<-c(0.2596,0.2488,0.2188,0.1445,0.1283)
ni<-c(15,13,19,11,6)
n<-sum(ni)
npi<-n*(1/k)
y.estimado<-sum((ni-npi)^2/npi) #7.25
y.estimado

# Calculo el valor nulo para una Chisq con df=4.
chisq<-qchisq(0.05,k-1, lower.tail = FALSE) #9.487729

#Como y.estimado<chisq--> Nuestro estadistico entra en zona de aceptacion--> Se acepta H0


# Se puede hacer con un chisq.test (que sirve para tablas de contingencia y para bondad de ajuste no parametrico).
# Es decir te dice con un nivel de confianza si una muestra se distribuye como una distribucion.

# En nuestro ejemplo hacemos una bondad de ajuste chisq - para determinar si la muestra se distribuye como una poisson
# Le incluimos la muestra y las probabilidades esperadas (siempre normalizadas de 0 a 1 sino hay que marcar rescale.p=TRUE) 
# para la distribucion que queremos probar en H0
# En nuestro caso H0:X ~ P(lambda=(15+13+19+11+6)/5=12.8). Es decir la p=1/5=0.2
prop.null=rep(1/k,5)
prop.null
chisq.test(ni,p=prop.null, correct = FALSE)
# Te devuelve siempre el valor teorico X-squared y el p-value. Si ese p-value es mayor que 0.5 (95%) o 0.1 (90%) o incluso 0.01 (99%)
# Entonces aceptamos la H0:X ~ P(lambda), sino rechazamos H0.
# En este caso p-value = 0.1094 > 0.05 => Se acepta H0 => X ~ P(lambda=(15+13+19+11+6)/5=12.8) para las prob. dadas
# IMP: Si los observados estan entre 5-0. Hay que marcar correct=TRUE o agrupar los datos.
# IMP: Si tenemos una muestra muy pequeña podemos marcar simulate.p.value=TRUE y te prueba B=2000 casos.
# IMP: Si las probabilidades estan en frecuencias en vez de normalizadas => rescale.p=TRUE

# En nuestro caso mejor hacer una simulacion pues nuestr muestr es muy pequeña:
chisq.test(ni,p=prop.null, correct = FALSE, simulate.p.value = 0.05)
# p-value = 0.1314 >>0.05 => X~P(12.8)



#Prueba de Bondad de ajuste de Chi-cuadrado con BINOMIAL 

# Queremos ver si la representacion de alumnos de una clase es igual. Es decir si X es la representacion
# de la clase queremos ver si tienen una proporcion 1/n.
x<-c(25,32,18,20)
n<-length(x)
null.p<-rep(1/n,n)

# Mi H0 X distribucion de probabilidad 1/n, es decir 0.25 y n=4 , X~B(4,0.25)
chisq.test(x,p=null.p,correct = FALSE)
# Como me sale un p-value mas alto que 0.05, para las probabilidaddes de la H0 
# => la H0 no puede ser rechazada. Entonces X~B(4,0.25) para cada tipo de estudiante

# Esto es lo mismo que :
chisq.test(x,correct = FALSE)
# Porque por defecto: p = rep(1/length(x), length(x))


# Ahora queremos ver si el numero de estudiantes de tipo1 y tipo2 son 2 veces en proporcion
# a los de tipo3 y tipo4. Osea queremos ver si Xtipo1=Xtipo2~B(n=4,p=1/3) y Xtipo3=Xtipo4~B(n=4,p=1/6)
x<-c(25,32,18,20)
p<-c(1/3,1/3,1/6,1/6)

results<-chisq.test(x,p=p,correct = FALSE)
results
# De igual forma no podemos rechazar H0 puesto que el p-value=0.4235>0.05
# => por lo tanto tambien es cierto que Xtipo1=Xtipo2~B(n=4,p=1/3) y Xtipo3=Xtipo4~B(n=4,p=1/6)
# Y puedes ver:
# Valores observados
results$observed
# Valores esperados
results$expected
# Cual se desvia mas a la prob nula
results$residuals

# Si ahora nos dicen que este año hemos tenido 30,28,28,11 
# y queremos compararlo con la proporcion de los ultimos años
freq.actual<-c(30,28,28,11)
freq.ant<-c(25,32,18,20)

prop.ant<-freq.ant/sum(freq.ant)
chisq.test(freq.actual,p=prop.ant,correct = FALSE)
# Nuestro p-value ahora es menor a 0.05 => Podemos rechazar H0 => La proporcion nueva es diferente a la antigua


# Para el paquete survey hay una enquesta de cuanto es el habito de fumadores de una escuela.
# ¿Se puede decir que la proporcion de no fumadores es del 70% y del resto se divide el otro 30% de forma equanime?
data(survey, package="MASS")
str(survey)
table(survey$Smoke)
# Parece que tenemos muchos de Never, pero tanto como para el 70%??
chisq.test(x=table(survey$Smoke),p=c(1/10,7/10,1/10,1/10),correct = FALSE)
# Tenemos un p-value = 0.004862<<0.05 => Rechazamos H0 => No siguen esa proporcion
# ¿Cual se desvia mas?
chisq.test(x=table(survey$Smoke),p=c(1/10,7/10,1/10,1/10),correct = FALSE)$resid
# Parece que el que mas se desvia es Heavy


# Una tambien sencilla es: con xchisq.test
require(mosaic)
library(mosaic)
xchisq.test(x=table(survey$Smoke),p=c(1/10,7/10,1/10,1/10),correct = FALSE)
# Te muestra la misma info pero en forma de tabla:
# MUESTRA-->                            11      189       19       17   
# ESPERADOS-->                       ( 23.60) (165.20) ( 23.60) ( 23.60)
# CONTRIBUCION AL ESTADISTICO. X2-->  [6.73]   [3.43]   [0.90]   [1.85] 
# RESIDUOS-->                         <-2.59>  < 1.85>  <-0.95>  <-1.36> 
# Y los resultados: X-squared = 12.898, df = 3, p-value = 0.004862

