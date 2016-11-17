# Limpiar el workspace
rm(list = ls(all = TRUE))
library(e1071)
library(prettyR)

##################### FORMULACION DEL PROBLEMA.
# Con objeto de comprobar la contaminaci?n del aire en una determinada ciudad, 
# se ha realizado un estudio en el que se han analizado las concentraciones de mon?xido de carbono (CO)
# durante cinco d?as de la semana (lunes, martes, mi?rcoles, jueves y viernes).
# estamos interesados en contrastar el efecto de un solo factor, que se presenta con cinco niveles, 
# sobre la variable respuesta.Nos interesa saber si las concentraciones medias de mon?xido de carbono
# son iguales en los cinco d?as de la semana
###############################################.


###########Muestras
# Concentraciones de Co por dia de la semana
lco<-c(420,390,480,430,440,324,450,460)
mco<-c(450,390,430,521,320,360,342,423)
xco<-c(355,462,286,238,344,423,123,196)
jco<-c(321,254,412,368,340,258,433,489)
vco<-c(238,255,366,389,198,256,248,324)

co<-c(lco,mco,xco,jco,vco)

# Dias de la semana de la muestra
dia<-c("Lunes","Lunes","Lunes","Lunes","Lunes","Lunes","Lunes","Lunes",
"Martes","Martes","Martes","Martes","Martes","Martes","Martes","Martes",
"Miercoles","Miercoles","Miercoles","Miercoles","Miercoles","Miercoles","Miercoles","Miercoles",
"Jueves","Jueves","Jueves","Jueves","Jueves","Jueves","Jueves","Jueves",
"Viernes","Viernes","Viernes","Viernes","Viernes","Viernes","Viernes","Viernes")

dia<-factor(dia) #creamos factores
split(co, dia) #visualizamos los datos para cada d?a


###########Calculo de frecuencias
n<-length(co)
fab<-table(co) #frecuencia absoluta
frel<-fab/n #frecuencia relativa
fabcum<-as.table(cumsum(fab))  #frecuencia absoluta acumulada
frelcum<-as.table(cumsum(frel))  #frecuencia relativa acumulada


####### Representacion grafica
#histograma
hist(co)
#funcion densidad
dens<-density(co)
plot(dens, lwd=3,col="blue")
# Da la sensacion de una Normal

####### Caracteristicas de asimetria de apuntamiento

# skew de una variable te devuelve:
# 0 si es totalmente simetrica
# [0..1] si es asimetrica posistiva. Cola por la derecha
# [-1..0] si es asimetrica negativa. Cola por la izquierda
skew(co) #-0.4229678
# Asimetrica negativa

# kurtosis de una variable te devuelve el coeficiente de kurtosis (apuntamiento)
# 0 seria normal perfecta (Mesocurtica)
# valores negativos mas aplanada (Platicurtica)
# valores positivos mas apuntalada (Leptocurtica)
kurtosis(co)   #-0.6618016
# Es Platicurtica --> Mas plana de lo normal
# => Menor concentracion de valores alrededor de la media

###### Relaciones por dia
# Vemos relaciones por dia
boxplot(co~dia)
pie(table(dia))
barplot(table(co,dia))
# Pocos valores atipicos, medias relativamente cercanas

##### Distribucion
# Revisamos si tiene una distribucion normal en toda la muestra y por cada "tratamiento" (dia)

shapiro.test(co) # 0.2868>0.05
# Se acepta que la distribucion es Normal (Shapiro-Will para muestras 30-50)

plot(density(lco), lwd=3,col="blue")
shapiro.test(lco) # 0.3094>0.05
plot(density(mco), lwd=3,col="blue")
shapiro.test(mco) # 0.8677>0.05
plot(density(xco), lwd=3,col="blue")
shapiro.test(xco) # 0.9532>0.05
plot(density(jco), lwd=3,col="blue")
shapiro.test(jco) # 0.7874>0.05
plot(density(vco), lwd=3,col="blue")
shapiro.test(vco) # 0.3081>0.05
# Se acepta que la distribucion es Normal en cada dia de la semana


# Podemos juntar una normal y co y ver como se parecen
# grafico de valores observados y esperados
d<-rnorm(length(co),mean=mean(co),sd=sd(co))
qqplot(co,d)
abline(c(0,1))
# Aunque ya lo tenemos claro podemos usar kolgomerov-smirov
ks.test(co,d, "two") # 0.9883>0.05
# Co es normal


######Contraste de independencia
# Vemos su tabla de contingencia
table(co,dia)

# H0 son independientes
chisq.test(table(co,dia),simulate.p.value=TRUE)
# o lo que es lo mismo:
chisq.test(co,dia,simulate.p.value=TRUE) 
# p-value=1>>0.05 => Son independientes

######Igualdad de varianzas
library(car)
# Testeamos levene:
# H0 todas las varianzas por tratamiento son iguales
leveneTest(co, group=dia) #p-value=0.1379>0.05=> Todas las varianzas son iguales


#si las varianzas son desiguales, habria que transformar Co 
# para elegir la transformacion hayar el poder de transformacion(p): Ln Co para p=0, sino Co**p

#muestra el valor del poder de transformacion
spreadLevelPlot(co, by=dia, robust.line=FALSE, xlab="Nivel CO") # 2.269316

#Valor (multiplo de 0.5) mas cercano al poder de transformacion es 2.5 
co.trans<-(co)**2.5
hist(co.trans)
boxplot(co.trans~dia)

#######ANOVA
#visualizamos descriptivos x dia
tapply(co,dia,summary) 
# Medias parece relativamente dispersas (entre 280 y 420)

#ANOVA
p.aov<-aov(co ~ dia)  
summary(p.aov) 
# Las medias no son iguales por dia => Las concentraciones de Co no se distribuyen igual por dia
# Es independiente el dia de la semana para saber la concentracion de Co

#Otra forma de ANOVA (Usando regresion lineal)
g.lm<-lm(co ~ dia)
anova(g.lm)
summary(g.lm) # Ojo con los factores en la lm, ha cogido el Jueves como Intercepto.... 
# el summary no tiene sentido usarlo para un analisis de la varienza por reg.lineal

########ENCONTRAR DIFERENCIAS
#el test de Tuckey compara las posibles medias dos a dos con el mismo tamaño de tratamientos
tukey<-TukeyHSD(p.aov) #los intervalos que no contienen el valor 0 son significativos, p-value<0,05
tukey
plot(tukey) #se puede ver en el gr?fico
#   Viernes-Lunes     -140.000 -253.69836 -26.301644 0.0095230
# y Viernes-Martes    -120.250 -233.94836  -6.551644 0.0337946

###### ESTIMACION DE EFECTOS Y MEDIAS
#Se puede hacer una estimacion de los efectos que tiene cada tratamiento
# y de las medias de cada tratamiento
model.tables(p.aov, type = "effects") 
#Lunes y viernes son los de mayores efectos

model.tables(p.aov, type = "mean") 
#la concentraci?n media de CO es mayor los lunes y menor los viernes.

# Lo podemos representar
boxplot(co ~ dia)

############## CONCLUSION
# Observamos que las cajas correspondientes a los miircoles, jueves y viernes estan practicamente
# superpuestas, de hecho el valor mediano del miercoles (linea negra dentro de las cajas) esta a un
# nivel interno dentro de la caja del jueves y de la caja del viernes. Este criterio se utiliza para
# comparar grupos y en este caso nos indica que hay homogeneidad o que no hay diferencias significativas
# en ese grupo de medias. 
# Sinembargo observamos que el lunes tiene una concentracion superior a los demas, por lo
# que concluimos que la concentracion de CO es mucho mayor este dia de la semana.



##################### FORMULACION DEL PROBLEMA.
# Ritmo cardiaco medio durante una prueba estresane en compañia de un perro, 
# amigo o control.
###############################################.
########### MUESTRA
grupo=c('p','a','p','c','c','p','a','a','p','a','c','c','c','a','a','p','c','p','c','c','a','a','c','p','p','a','a','c','a','p','c','c','p','p','p','c','c','c','a','a','a','p','p','a','p')
ritmo=c(69.169,99.692,70.169,80.369,87.446,75.985,83.400,102.154,86.446,80.277,90.015,99.046,75.477,88.015,92.492,
        68.862,87.231,64.169,91.754,87.785,91.354,100.877,77.800,97.538,85,101.062,97.046,62.646,81,72.262,
        84.738,84.877,58.692,76.622,69.231,73.277,84.523,70.877,89.815,98.2,76.908,69.538,70.077,86.985,65.446)
d=as.data.frame(cbind(grupo,ritmo))
head(d)

########### VER VARIABLES
# Medias y desviaciones por grupo
tapply(ritmo,grupo,mean)
tapply(ritmo,grupo,sd)
mean(ritmo)
sd(ritmo)
# Cajas
boxplot(ritmo~grupo)
# Ramas y hojas
stem(ritmo[grupo=='a'])
stem(ritmo[grupo=='p'])
stem(ritmo[grupo=='c'])

#######NORMALIDAD E IGUALDAD VARIANZAS
# Se acepta que la distribuci?n es Normal (Shapiro-Will para muestras 30-50)
shapiro.test(ritmo)
shapiro.test(ritmo[grupo=='a'])
shapiro.test(ritmo[grupo=='p'])
shapiro.test(ritmo[grupo=='c'])

#Igualdad de varianzas
leveneTest(ritmo, group=factor(grupo)) #p-value=0.9833>0.05=> Todas las varianzas son iguales

#si las varianzas son desiguales, habria que transformar ritmo 
# para elegir la transformacion hayar el poder de transformacion(p): Ln ritmo para p=0, sino ritmo**p

#muestra el valor del poder de transformacion
library(car)
spreadLevelPlot(ritmo, by=grupo, robust.line=FALSE, xlab="Nivel Ritmo") # p=-1.348592

#transformaci?n: Ln X para p=0, sino X**p
ritmo.trans<-(ritmo)**(-1.5)
hist(ritmo.trans)
boxplot(ritmo.trans~grupo)

#######ANOVA
#visualizamos descriptivos x grupo
tapply(ritmo,grupo,summary) 

#ANOVA
p.aov<-aov(ritmo ~ grupo)  
summary(p.aov)        
# Las medias no son iguales por grupo => Las ritmos no se distribuyen igual por grupo
# Es independiente el grupo para saber el ritmo cardiaco

#Otra forma de ANOVA
g.lm<-lm(ritmo ~ grupo)
anova(g.lm)
summary(g.lm) # Ojo con los factores en la lm, ha cogido 'a' como Intercepto.... 
# el summary no tiene sentido usarlo para un analisis de la varienza por reg.lineal


########ENCONTRAR DIFERENCIAS
#Se rechaza la hipotesis de igualdad de medias, pero donde estan las diferencias
#el test de Tuckey compara las posibles medias dos a dos.
tukey<-TukeyHSD(p.aov) #los intervalos que no contienen el valor 0 son significativos, p-value<0,05
tukey
plot(tukey) #se puede ver en el grafico
# Las tres diferencias estan fuera del 0=> Las 3 difieren. En los 3 grupo hay una diferencia significativa

###### ESTIMACION DE EFECTOS Y MEDIAS
#Se puede hacer una estimacion de los efectos que tiene cada tratamiento
# y de las medias de cada tratamiento
model.tables(p.aov, type = "effects") 
#a es el que tendria mas efecto, p el que menos

model.tables(p.aov, type = "mean") 
#el ritmo medio es mayor en el grupo a y menor en el p

# Lo podemos representar
boxplot(ritmo ~ grupo)

############## CONCLUSION
# Como se ve claramente las cajas y medianas difieren claramente, esta claro que no es
# dependiente el grupo en el ritmo cardiaco.
# La concentracion de valores en p es mayor que en a o c.


