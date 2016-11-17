# Limpiar el workspace
rm(list = ls(all = TRUE))


#####################ANALISIS VARIANZA - 1 FACTOR ####################

##################### FORMULACIÓN DEL PROBLEMA.
# Se inyecta 5 compuestos a 12 ratas de similares características y se anotan los tiempos de reacción. 
# Los animales son clasificados en 5 grupos, administrándole a cada uno de ellos un compuesto distinto. 
# Los resultados son:
# Familia 1: Tiempo de reacción(min): 8.3,7.6,8.4,8.3
# Familia 2: Tiempo de reacción(min): 7.4,7.1
# Familia 3: Tiempo de reacción(min): 8.1,6.4
# Familia 4: Tiempo de reacción(min): 7.9,8.5,10
# Familia 5: Tiempo de reacción(min): 7.1
# ¿Se puede considerar a un nivel conf=0.05 que hay diferencias significativas entre los compuestos?
###############################################.
# Para saber como se distribuye una variable independiente: tipo familia en una variable dependiente: t.reaccion
# hay que estudiar el comportamiento de VI en las diferentes poblaciones de la VD => ANOVA
# Planteando que las diferentes medias en las k poblaciones son iguales como H0 

##################### REVISION DATOS Y FACTORIZAR.
familia<-c(1,1,1,1,2,2,3,3,4,4,4,5)
tiempo<-c(8.3,7.6,8.4,8.3,7.4,7.1,8.1,6.4,7.9,8.5,10,7.1)

fam<-factor(familia) #creamos factores, para que la familia tenga un orden
fam # Es obligatorio para la anova (aov)
split(tiempo, fam) #visualizamos los datos para cada familia

##################### REQUISITOS ANOVA.
# Para plantear ANOVA se deben cumplir:

##################### NORMALIDAD TRATAMIENTOS.
# 1. Normalidad en las diferentes familias
# Aqui se puede utilizar un contraste de hipotesis por shapiro.
# Pero para muestras tan pequeñas no nos vale, o nos da error o un p-value bajo para rechazar la normalidad
shapiro.test(tiempo[familia==1])
shapiro.test(tiempo[familia==2])
shapiro.test(tiempo[familia==3])
shapiro.test(tiempo[familia==4])
shapiro.test(tiempo[familia==5])
# Otra opcion es utilizar una chisq.test con simulacion.

# Pero el metodo mas seguro y que funciona con pocas muestras es hacer un plot de los elementos de cada
# tratamiento con respecto a su residuo (residuo diferencia del elemento con la media de su trat.)
# Es decir, calculamos los residuos de cada tratamiento
res.fam1=tiempo[familia==1] - mean(tiempo[familia==1])
res.fam2=tiempo[familia==2] - mean(tiempo[familia==2])
res.fam3=tiempo[familia==3] - mean(tiempo[familia==3])
res.fam4=tiempo[familia==4] - mean(tiempo[familia==4])
res.fam5=tiempo[familia==5] - mean(tiempo[familia==5])
# Los juntamos todos en un mismo vector
residuos<-c(res.fam1,res.fam2,res.fam3,res.fam4,res.fam5)
# La media siempre tendra que ser practicamente 0
mean(residuos)
# Y los residuos con respecto a sus valores deberan estar alrededor de una linea imaginaria, 
# muy cerca de ella
plot(tiempo,residuos,xlim = c(4,12), ylim = c(-10,10))
# En este caso la media es -1.480267e-16 practicamente 0 y los puntos forman practicamente una linea


##################### TODAS VARIANZAS IGUALES.
# 2. Todas las varianzas son iguales (de las diferentes familias)

# gráfico para ver la varianza para las diferentes familias
boxplot(tiempo~familia)
# Parece que varianza es parecida puesto que las cajas mas o menos tiene el mismo IQR
# Pero no es nada seguro tenemos que confirmar con levene


# Nos aseguramos con una prueba de Levene que me dice si las
# variables por grupo son iguales
require(car)
leveneTest(tiempo~fam)
#o lo que es lo mismo
leveneTest(tiempo, group = fam)
# El estadistico que es una F de fisher te da 1.3233, p-value=0.3493>0.05=> Aceptamos H0
# => Las varianzas iguales

# OJO tambien nos podemos encontras con levene.test de la librery "lawstat", pero esta deprecada. 
# La diferencia es que en group no necesitamos factor, en este caso familia.
#require(lawstat)
#levene.test(tiempo, group=familia)

##################### ANOVA A MANO.
#Calculamos la ANOVA a mano
tm<-sum(tiempo)/12
m1<-sum(8.3,7.6,8.4,8.3)/4
m2<-sum(7.4,7.1)/2
m3<-sum(8.1,6.4)/2
m4<-sum(7.9,8.5,10)/3
m5<-7.1


# diferencia de la media del grupo a la media total

sce<-2*(((m1-tm)**2)+((m2-tm)**2)+((m3-tm)**2)+((m4-tm)**2)+((m5-tm)**2))
sce

(4*(m1-tm)**2)+(2*(m2-tm)**2)+(2*(m3-tm)**2)+(3*(m4-tm)**2)+(1*(m5-tm)**2)

#diferencia de cada puntuación a la media de su grupo
mm<-c(m1,m1,m1,m1,m2,m2,m3,m3,m4,m4,m4,m5)
scr<-sum((tiempo-mm)**2)

#diferencia de cada puntuación a la media total
sct<-sum((tiempo-tm)**2)

sct<-sce+scr
sct


se2<-sce/(5-1)
sr2<-scr/(12-5)
st2<-sct/(12-1)

se2
sr2
st2

##################### ANOVA DIRECTAMENTE.
# Nos podemos ahorra todos estos calculos
# VD: tiempo, VI: fam.
# Contrasta si tiempo medio es igual en todas las categorias de familia.
p.aov<-aov(tiempo ~ fam)  
summary(p.aov)         
# Nos da los grados de libertad, la sum de cuadrados y la media cuadratica 
# tanto de la variable dep como de los residuos
# Luego nos da el Fvalue del experimento y su p-value bastante alto--> Podriamos aceptar H0
# Puesto que entraria en la zona de aceptacion.

# Comprobamos con F teorico para los grados de libertad y para 95%
qf(0.95,df1=4,df2=7,lower.tail = TRUE) 
#O
qf(0.05,df1=4,df2=7,lower.tail = FALSE) 




#####################ANALISIS VARIANZA - 2 FACTORES ####################
##################### FORMULACIÓN DEL PROBLEMA.
## Estudiar el efecto de las variables Edad y Fumar sobre la Ansiedad Social con alfa=0,01.
##################### REVISION DATOS Y FACTORIZAR.
mf<-c(3.91,5.01,4.47,3.33,4.71,5.65,6.49,5.50,5.72,5.44,4.94,7.13,5.54,5.94,6.16,4.83,3.95,4.04,3.66,9.44,9.66,7.68,9.57,7.98,7.39,5.92,5.48,5.19,6.12,4.45)
fumar<-c(rep(1,15),rep(2,15))
# Si no hacemos factor la anova no se entera de los grupos
fac.fumar<-factor(fumar)
fac.fumar
#Revisamos la tabla
split(mf,fac.fumar)

edad<-rep(c(rep(1,5),rep(2,5),rep(3,5)),2)
# Si no hacemos factor la anova no se entera de los grupos
fac.edad<-factor(edad)
fac.edad
#Revisamos la tabla
split(mf,fac.edad)


##################### NORMALIDAD TRATAMIENTOS.
#Revisamos los residuos de cada tratamiento con respecto a la muestra
res1=mf[fac.fumar==1&fac.edad==1] - mean(mf[fac.fumar==1&fac.edad==1])
res2=mf[fac.fumar==1&fac.edad==2] - mean(mf[fac.fumar==1&fac.edad==2])
res3=mf[fac.fumar==1&fac.edad==3] - mean(mf[fac.fumar==1&fac.edad==3])
res4=mf[fac.fumar==2&fac.edad==1] - mean(mf[fac.fumar==2&fac.edad==1])
res5=mf[fac.fumar==2&fac.edad==2] - mean(mf[fac.fumar==2&fac.edad==2])
res6=mf[fac.fumar==2&fac.edad==3] - mean(mf[fac.fumar==2&fac.edad==3])
# Los juntamos todos en un mismo vector
residuos<-c(res1,res2,res3,res4,res5,res6)
# La media siempre tendra que ser practicamente 0
mean(residuos)
# Media practicamente 0
plot(mf,residuos,xlim = c(1,12), ylim = c(-10,10))
# Residuos practicamente forman una linea recta.
# Observacion pero con residuos relativamente altos=> Las varianzas van a ser altas



##################### TODAS VARIANZAS IGUALES
# Revisamos varianzas iguales
boxplot(mf ~ fac.edad)
boxplot(mf ~ fac.fumar)
boxplot(mf ~ fac.fumar*fac.edad)

# Hacemos levene
leveneTest(mf~fac.edad*fac.fumar)
# p-value=0.6611>0.05 => varianzas iguales


##################### ANOVA DIRECTAMENTE.
# IMPORTANTE: Cuando hacemos aov usando + estamos analizando VD con cada factor, pero no con la interaccion
#             Cuando hacemos aov usando * estamos analizando VD con cada factor y la interaccion entre ellos

p.aov<-aov(mf ~ fac.edad + fac.fumar)
summary(p.aov)
# Parece que la edad tiene un p-value pequeño--> en zona de rechazo--> Rechazo H0 de mf con edad --> La edad no se relaciona con la ansiedad
# Sin embargo el p-value de fumar es relativamente alto 0.04>0.01 --> Acepto H0 de fumar (si estan relacionados pero poco)

#compruebo para la edad
qf(0.01,2,26,lower.tail = FALSE) # 5.52<7.75 --> Estadistico en zona de rechazo
qf(0.01,1,26,lower.tail = FALSE) # 7.72>4.308 --> Estadistico en zona de aceptacion

# Conclusion Vemos como el p-value es bajo para el fumar--> H0:beta3=0
# --> Rechazamos H0 --> fumar y mf estan relacionados



# Si queremos ver realamente una ANOVA de la tabla cruzada tenemos que usar *
p.aov<-aov(mf ~ fac.edad * fac.fumar)
summary(p.aov)
# Aqui lo vemos todo: vemos que la edad no se relaciona con la ansiedad, que fumar se relaciona pero poco
# Y que la interaccion entre ellos tambien se relaciona pero poco

# AQUI DEBERIAMOS APLICAR TUKEY PARA LA EDAD (SE EXPLICA EN EL SIGUIENTE EJERCICIO)

##################### OTRAS CONCLUSIONES.
# De hecho si probamos a relacionar ambas variables en una regresion lineal:
reg<-lm(mf ~ edad + fumar)
summary(reg)
# edad tiene un p-value muy alto => eso significa que coeficiente es muy seguro que sea 0 
# => No tiene una relacion lineal con mf.
# Sin embargo fumar si tiene relacion con mf, puesto que su p-value es menor que 0.1 (aunque no mucho puesto que p-valur no es menor que 0.05)

#Si lo vemos en una grafica
qqplot(mf,edad,xlim = c(-2,15), ylim = c(-3,10))
abline(reg,col="blue",lwd=2)
qqplot(mf,fumar,xlim = c(-2,15), ylim = c(-3,10))
abline(reg,col="blue",lwd=2)

# Se podria plantear:
reg<-lm(mf ~ fumar)
summary(reg)
plot(mf,fumar,xlim = c(-2,15), ylim = c(-3,10))
abline(reg,col="blue",lwd=2)
# fumar esta algo relacionado pero no mucho entre el 90% y 95% de confianza.


# OBS1:
# Para incluir la interaccion en un modelo que es lo suyo en las ANOVA de 2 factores:
reg<-lm(mf ~ edad + fumar + edad*fumar)
summary(reg)
# Si nos fijamos con la interaccion no entrarian ninguno en el modelo todos estan en zona de aceptacion 
# Con lo cual sus coeficientes serian 0 y por lo tanto no entrarian.

# OBS2:
# Importante si incluimos los factores en vez de las variables la regresion la calcula para los factores:
# Y da resultados que no nos dan mucha informacion
reg<-lm(mf ~ fac.edad + fac.fumar)
summary(reg)
# fac.edad2, fac.edad3, fac.fumar2, los toma como variables=> Evitar factores en una regresion
# a diferencia de en aov que es obligatorio




#####################ANALISIS VARIANZA - 1 FACTOR (CON MEDIAS DESIGUALES) ####################

control<-c(50,65,72,46,38,29,70,85,72,40,57,59)
mg25<-c(49,47,30,62,62,60,19,28,56,62,55,40)
mg50<-c(20,59,64,61,28,47,29,41,60,57,61,38)
mg100<-c(20,23,38,31,27,16,27,18,22,12,24,11)
mg125<-c(18,30,22,26,31,11,15,12,31,36,16,13)

# creamos un vector con valores de dosis y otro con la identificación de muestra
# "muestra" se crea como un factor, ya que en caso contrario se podría confundir con un vector numérico.
muestra<-rep(1:5, each=12)
muestra<-factor(muestra, labels=c("control","mg25","mg50","mg100","mg125"))
muestra
dosis<-c(control,mg25,mg50,mg100,mg125)
dosis

#resumen y gráfico 
# Aplico un summary a cada dosis agrupado por muestra
tapply(dosis, muestra, summary)
boxplot(dosis~muestra)


#Asumiendo que la variable "dosis" sigue una distribución normal en sus dirferentes poblaciones
# con varianza común para las tres poblaciones.


# la tabla del análisis de la varianza es:
p.aov<-aov(dosis~muestra)
summary(p.aov)
#Como el p-valor es muy pequeño se concluye que hay diferencias muy significativas 
#entre las medias de las 5 muestras=>la dosis se comporta diferente segun las diferentes muestras 

#Otra posibilidad es definir el modelo lineal y obtener la tabla con la instrucción anova.
g.lm<-lm(dosis~muestra)
anova(g.lm)
#Información del modelo lineal
summary(g.lm)
# ANOVA de un modelo lineal te hace un analisis de la varianza entre VI y las VD.

# Sin embargo aqui esta cogiendo para la primera muestra (control en este caso) como si fuera beta0.
# Esto vale cuando estas trabajando con un modelo y quieres hacer un analisis de la varianza pero
# para un analisis de la varianza entre una variable y uno o dos factores (en una tabla de relacion) NO VALE.
# summary no valdria, anova te puede puede servir igual que aov antes => Si te da un F global.
# Los t de summary no te valdrian.

# Con el summary podemos ver los Std. Error que se comenten en cada beta
# Y tambien Residual standard error= 13.27. Que es el error total que se comete con la regresion
# Inversamente al R^2.

# Esta estimación se obtiene directamente del modelo lineal,
# Y se le llama error cuadrático medio o estimación insesgada de la varianza
summary(g.lm)$sigma^2

#Tambien se puede calcular, en realidad es:
#     la sum(de cuadrados de los errores)/ df de los residuos
ECM <- deviance(p.aov)/p.aov$df.residual
ECM


###### CONCLUSION
# El p-valor es inferior al nivel de significación propuesto (0.01) de modo que rechazamos
# la hipótesis nula de igualdad de medias y admitimos que hay diferencias entre las dosis

# Si queremos averiguar que muestra es la que estropea la relacion con dosis. Es decir
# que media no es igual al resto:

# 1. TUKEY si las muestras tienen el mismo tamaño
# 2. SHEFFE, BONFERRONI si las muestra tienen tamaños diferentes

# En nuestro caso aplicamos tukey puesto que las 4 muestras tienen todas tamaño 12
TukeyHSD(p.aov)
# Tukey se basa en el contraste de medias por t de student y calcula un intervalo de confianza 
# alrededor de las diferencia de las medias (para cualquier combinacion dos a dos).
# Si ese intervalo no contiene el 0, es por que las medias son significativamente diferentes.
# O dicho de otra forma aquella diferencia de medias donde padj sea menor que 0.05 (al 95%)
# SOLO VALE PARA N IGUALES DE LOS TRATAMIENTOS
# En nuestro caso se marcan con *:

#diff       lwr        upr     p adj
#mg25-control   -9.4166667 -24.69163   5.858299 0.4194270
#mg50-control   -9.8333333 -25.10830   5.441633 0.3752401
#mg100-control -34.5000000 -49.77497 -19.225034 0.0000004 *
#mg125-control -35.1666667 -50.44163 -19.891701 0.0000003 *
#mg50-mg25      -0.4166667 -15.69163  14.858299 0.9999918
#mg100-mg25    -25.0833333 -40.35830  -9.808367 0.0002134 *
#mg125-mg25    -25.7500000 -41.02497 -10.475034 0.0001397 *
#mg100-mg50    -24.6666667 -39.94163  -9.391701 0.0002774 *
#mg125-mg50    -25.3333333 -40.60830 -10.058367 0.0001821 *
#mg125-mg100    -0.6666667 -15.94163  14.608299 0.9999465

# Serian mg100-control, mg125-control, mg100-mg25, mg125-mg25, mg100-mg50, mg125-mg50. 

# Se ve mas claro en la grafica
plot(TukeyHSD(p.aov))

#Tukey vale cuando los tratamientos tienen un tamaño igual
# En caso contrario ha de probarse con Scheffe o Bonferroni

# Scheffe
install.packages("agricolae")
require(agricolae)
# Scheffe se basa en la F de snedecor y calcula las tasas de error para cualquier combinacion de X medias
# y las agrupa en grupos (dos a dos, una con las demas, una con otra,etc..). Su estadisitico muestra la minima 
# diferencia significativa. Y te muestra los grupos con una tasa de error mayor.
# Si el diferencia minima (Minimum Significant Difference) supera al valor critico de F estamos hablando de que 
# hay grupos con medias significativamente diferentes.

scheffe.test(p.aov,trt="muestra",group=TRUE,console=TRUE)
# Si utilizas group=TRUE => Te indica los grupos con diferencias significativas de las medias.
#     Groups, Treatments and means
#       a 	 control 	 56.92 
#       a 	 mg25    	 47.5 
#       a 	 mg50    	 47.08 
#       b 	 mg100   	 22.42 
#       b 	 mg125   	 21.75

scheffe.test(p.aov,trt="muestra",group=FALSE,console=TRUE)
# Si utilizas group=FALSE => Te indica los pares 2 a 2 con diferencias significativas puntualizadas por *, ** y ***
#   control - mg100  34.5000000 0.0000 ***  15.161587 53.838413
#   control - mg125  35.1666667 0.0000 ***  15.828254 54.505079
#   control - mg25    9.4166667 0.5586      -9.921746 28.755079
#   control - mg50    9.8333333 0.5155      -9.505079 29.171746
#   mg100 - mg125     0.6666667 1.0000     -18.671746 20.005079
#   mg100 - mg25    -25.0833333 0.0010 *** -44.421746 -5.744921
#   mg100 - mg50    -24.6666667 0.0013  ** -44.005079 -5.328254
#   mg125 - mg25    -25.7500000 0.0007 *** -45.088413 -6.411587
#   mg125 - mg50    -25.3333333 0.0009 *** -44.671746 -5.994921
#   mg25 - mg50       0.4166667 1.0000     -18.921746 19.755079

# En cualquiera de los 2 casos se pueden identificar la/s medias o grupos de medias que estan perjudicando el analisis



# Bonferroni (tambine para n distintos de las muestras) es mas arriesgado que Scheffe, trabaja con t de student
# Controla la tasa de error de las medias con un factor sobre el alfa, lo cual hace 
# rechazar mas veces la hipotesis de igualdad de medias
# Funciona exactamente igual que Scheffe
LSD.test(p.aov,trt="muestra",group=TRUE, console = TRUE)

#Groups, Treatments and means
# a 	 control 	 56.91667 
# a 	 mg25 	   47.5 
# a 	 mg50 	   47.08333 
# b 	 mg100 	   22.41667 
# b 	 mg125 	   21.75

LSD.test(p.aov,trt="muestra",group=FALSE, console = TRUE)

#  control - mg100  34.5000000 0.0000  ***  23.646042  45.35396
#  control - mg125  35.1666667 0.0000  ***  24.312709  46.02062
#  control - mg25    9.4166667 0.0877    .  -1.437291  20.27062
#  control - mg50    9.8333333 0.0749    .  -1.020625  20.68729
#  mg100 - mg125     0.6666667 0.9025      -10.187291  11.52062
#  mg100 - mg25    -25.0833333 0.0000  *** -35.937291 -14.22938
#  mg100 - mg50    -24.6666667 0.0000  *** -35.520625 -13.81271
#  mg125 - mg25    -25.7500000 0.0000  *** -36.603958 -14.89604
#  mg125 - mg50    -25.3333333 0.0000  *** -36.187291 -14.47938
#  mg25 - mg50       0.4166667 0.9390      -10.437291  11.27062

# Notar que los valores cambian un poco con respecto a Scheffe, por que son diferencias mas restrictivas
# y son diferentes estadisticos pero los grupos y medias 2 a 2 significativas son muy parecidos
# Aunque mas restrictivas en Bonferroni(notar los puntos '.' en control - mg25 y control - mg50)




###############ANALISIS VARIANZA - 2 FACTORES (CON MEDIAS DESIGUALES EN UN FACTOR) #################


#Se trata de un diseño de bloques aleatorizados (cada finca es un bloque).
produc <- c(2.1, 2.2, 1.8, 2, 1.9, 2.2, 2.6, 2.7, 2.5, 2.8, 1.8, 1.9, 1.6, 2, 1.9, 2.1, 2, 2.2, 2.4, 2.1)
fert <- factor(rep(c("1","2","3","4"), each=5))
finca <- factor(rep(c("1","2","3","4","5"), 4))
#creamos la tabla de filas "finca" y columnas "fertilizantes"
xtabs(produc ~ finca + fert)

#resumen de los datos y los gráficos 
# Resumen de productos agrupados por fertilizante
tapply(produc, fert, summary)
# Resumen de productos agrupados por finca
tapply(produc, finca, summary)

#Vemos su relacion en grafica--> Se podia haber usado un plot
stripchart(produc ~ fert, method = "stack")
stripchart(produc ~ finca, method = "stack")

# Vemos su relacion entre las 3 (mejor que bloxplot)
# En ultimo lugar siempre ponemos lo que queremos tener en eje x (vertical)--> Nuestra VD
# Esta interaccion muestra como va cambiando la media de VD en funcion de los factores
interaction.plot(fert, finca, produc, legend = T)
interaction.plot(finca, fert, produc, legend = T)
# La media no varia mucho segun la grafica, pero hay que asegurarse con una ANOVA.
# De aqui se puede desprender que no tenemo un valor atipico fuerte o una asimetria

#El modelo lineal y la tabla del análisis de la varianza son:
g.lm <- lm(produc ~ finca + fert)
anova(g.lm)

#No hay diferencias entre las fincas, pero sí las hay entre los fertilizantes.
# Su p-value es muy bajo con lo cual se debe quitar de la regresion.
# Acordarse que summary(g.lm) nos lleva a confusion
# Acordarse que si no incluyes finca * fert no estas teniendo en cuanta la interaccion

####################################
### Anova sin regresion lineal
# Al mismo resultado de anova(g.lm) vamos a llegar con aov

# Para utilizar la aov deberiamos asegurarnos de normalidad en los tratamientos y varianzas iguales
# Suponemos que se cumple
aov2factor<-aov(produc ~ finca + fert)
summary(aov2factor)
# Vemos como pasaba en la lm, que fert no tiene medias iguales y por tanto no esta distribuido igaul para
# product en los diferentes tratamientos


# TRABAJAMOS AHORA SOLO CON fert:
# Queremos analizar produc en los diferentes tratamientos de fert

# resumen y gráfico 
tapply(produc, fert, summary)
plot(produc~fert)

# Anova con fert
aov1factor<-aov(produc~fert)
summary(aov1factor)
# p-value muy pequeño con lo cual=> Rechazamos H0 medias iguales
# => producto no se distribuye igual por los diferentes tratamientos de fert

# Como tamaño de las muestra es igual utilizamos Tukey, sino lo fuera Scheffe o Bonferroni
TukeyHSD(aov1factor)
plot(TukeyHSD(aov1factor))
#son significativas las diferencias entre 3-2, 4-2 y 4-3(por muy poco) y 2-1.
# Hay que ver siempre los que tengan menos del p-value



###############ANALISIS VARIANZA - 2 FACTORES (CON MEDIAS DESIGUALES EN INTERACCION) ####################

#Sobre dos tipos de suelo (ácido y alcalino) se aplican tres tipos de abono (A,B,C) 
#y se espera que se manifiesten de distinta forma en cada tipo de suelo.
#la respuesta indica el índice de abundancia de una determinada especie

abono<-as.character(c(rep("A",3),rep("B",3),rep("C",3),rep("A",3),rep("B",3),rep("C",3)))
suelo<-c(rep("ácido",9),rep("alcalino",9))
abunda<-as.numeric(as.character(c(8,4,0,10,8,6,8,6,4,14,10,6,4,2,0,15,12,9)))

# Factorizo
fac.abono<-factor(abono, labels = c("A","B","C"))
fac.suelo<-factor(suelo, labels = c("ácido","alcalino"))

xtabs(abunda~fac.abono+fac.suelo)

# Reviso y grafico
tapply(abunda, fac.abono, summary)
boxplot(abunda~fac.abono)

tapply(abunda, fac.suelo, summary)
boxplot(abunda~fac.suelo)

boxplot(abunda~fac.suelo+fac.abono)

interaction.plot(fac.abono,fac.suelo,abunda,legend = T)
interaction.plot(fac.suelo,fac.abono,abunda,legend = T)

# Normalidad en todos los tratamientos
#Revisamos los residuos de cada tratamiento con respecto a la muestra
shapiro.test(abunda[fac.suelo=="ácido"&fac.abono=="A"])
shapiro.test(abunda[fac.suelo=="ácido"&fac.abono=="B"])
shapiro.test(abunda[fac.suelo=="ácido"&fac.abono=="C"])
shapiro.test(abunda[fac.suelo=="alcalino"&fac.abono=="A"])
shapiro.test(abunda[fac.suelo=="alcalino"&fac.abono=="B"])
shapiro.test(abunda[fac.suelo=="alcalino"&fac.abono=="C"])

# Compruebo igualdad de varianzas entre todos los tratamientos
leveneTest(abunda~fac.suelo*fac.abono)

# ANOVA: Para suelo y para abono por separado
p.aov<-aov(abunda~fac.suelo+fac.abono)
summary(p.aov)
# Parece que ambas estan relacionadas con abunda, ya que sus medias son iguales en sus tratamientos respectivamente

# Cuando utilizamos * en vez de +, estamos probando de verdad la tabla cruzada suelo,abono
# Esto es realmente una ANOVA de 2 factores
p.aov<-aov(abunda~fac.suelo*fac.abono)
summary(p.aov)
# Parece que la interaccion entre ella no se distribuye igual

#hay diferencias para cada tipo de abono+suelo (en la interaccion)
# Creamos una variable comun para estudiarla
ab.su=paste(abono,suelo,sep="")
ab.su
table(ab.su)
tapply(abunda,ab.su,mean)
boxplot(abunda~ab.su)

# Como ya hemos comprobado normalidad y homocedeisticidad antes con shapiro y con levene para todos los tratamientos interactuados
# podemos proceder a esta anova
p.aov<-aov(abunda~ab.su)
summary(p.aov)
# Como vemos no tiene distribucion comun ahora podemos ver en que tratamiento falla con Tukey porque los
# son iguales a 1 todos

TukeyHSD(p.aov)
plot(TukeyHSD(p.aov))
# difieren los abonos B y C en el suelo Alcalino.

# Todas estas ANOVAS se pueden ver tambien por regresiones lineales:
# 2 factores a la vez
g.lm <- lm(abunda~abono + suelo)
anova(g.lm)

# 2 factores interactuados
g.lm <- lm(abunda~abono*suelo)
anova(g.lm)

# Solo con la interaccion de los factores
g.lm<-lm(abunda~ab.su)
anova(g.lm)



######################ANALISIS DE VARIANZA INTRSUJETOS (1 FACTOR)###############
# A veces tenemos agrupados para n factores los valores de una variable dependiente.
# Por ejemplo nuemro de personas con ansiedad agrupado por fumar y por tramos de edad.
# Pero si en vez del numero de personas tenemos Pepito (Si tiene ansiedad), Fulanito (Si tiene ansiedad), etc...
# Ya no es un totalizador sino que tenemos otro factor mas intrasujetos: Llamemosle Sujeto
# El aov cambia:

#Ejemplo 1:
# Pensemos que tenemos un experimento en el que cinco sujetos han de memorizar una lista de palabras.
# Las palabras de la lista son de tres tipos: palabras con valencia positiva, negativa o neutra. 
# La variable dependiente es el número de palabras recordadas en la fase de test, y no la tenemos totalizada sino por sujeto.

valencia<-factor(rep(c("Neg","Neu","Pos"),5))
sujeto<-factor(rep(c("Pedro","Maria","Elena","Vanessa","Luis"),each=3))
num.palabras<-c(32,15,45,30,13,40,26,12,42,22,10,38,29,8,35)

# Observa que tenemos un único factor (la valencia de las palabras) con tres factores (negativa, positiva y neutra). 
# Tenemos además un factor de efectos aleatorios: Sujetos.


# Revisamos medias
xtabs(num.palabras~valencia+sujeto)
tapply(num.palabras, valencia, mean)

# Suponemos normalidad entre las diferentes valencias y varianza igual
# Aplicamos la anova
p.aov<-aov(num.palabras~valencia+Error(sujeto/valencia))
summary(p.aov)
# Vemos 2 cosas:
# 1ª:
# Error: sujeto
#           Df Sum Sq Mean Sq F value Pr(>F)
# Residuals  4  105.1   26.27
# =>No devuelve datos por que en este caso no hay ningun factor entre sujetos como pudiera ser la edad o el genero(p.ej.)

# 2º:
# Error: sujeto:valencia
#           Df Sum Sq Mean Sq F value   Pr(>F)    
# valencia   2 2029.7  1014.9   189.1 1.84e-07 ***
# Residuals  8   42.9     5.4 
# Te indica la relacion entre Sujeto:Valencia y num-palabras. (Indicando que ahora valencia es para varios sujetos)
# Vemos que las medias difieren=> El tipo de valencia no afecta a los sujetos para recordar el numero de palabras



######################ANALISIS DE VARIANZA INTRSUJETOS (2 FACTORES)###############
#Ejemplo 2:
# Al anterior ejemplo le añadimos un nuevo factor, los sujetos tenian que recordar las palabras con 
# diferente valencia y ademas se hacian dos pruebas: (libres, con señal)

tipo.tarea<-factor(rep(c("Libre","Señal"),5,each=3))
valencia<-factor(rep(c("Neg","Neu","Pos"),10))
sujeto<-factor(rep(c("Pedro","Victor","Maria","Amparo","Ana"),each=6))
num.palabras<-c(8,9,5,7,9,10,12,13,14,16,13,14,13,13,12,15,16,14,12,14,15,17,18,20,6,7,9,4,9,10)

# Observa que tenemos 2 factores (la valencia y el tipo de tarea) 
# Tenemos además un factor de efectos aleatorios: Sujetos.


# Revisamos medias
xtabs(num.palabras~valencia+tipo.tarea+sujeto)
tapply(num.palabras, list(valencia,tipo.tarea), mean)
boxplot(num.palabras~valencia*tipo.tarea)
interaction.plot(valencia,tipo.tarea,num.palabras)
interaction.plot(tipo.tarea,valencia,num.palabras)

# Suponemos normalidad entre las diferentes valencias,tipo tarea y varianzas iguales
# Aplicamos la anova
p.aov<-aov(num.palabras~(valencia*tipo.tarea)+Error(sujeto/(valencia*tipo.tarea)))
summary(p.aov)
# Vemos varias cosas:
# No hay factores para sujeto
# sujeto:valencia --> las medias son iguales=> La valencia por sujeto afecta al numero de palabras
# sujeto:tipo.tarea--> las medias no son iguales=> El tipo de tarea por sujeto no afecta al numero de palabras
# sujeto:valencia:tipo.tarea--> las medias si son iguales=>
#   la interacion entre tipo de tarea y valencia (por sujeto) afecta al numero de palabras recordadas


######################ANALISIS DE VARIANZA MIXTOS###############
# Limpiar el workspace
rm(list = ls(all = TRUE))
setwd("D:/Pedro/Master MBIT/Temario/Modulo4/Sesion 38-42 - Analisis Estadistico - Lourdes Servan/Ficheros Datos")

#Ejemplo 3:
# Al anterior ejemplo vamos a incluirte factores a los sujetos: Genero y Dosis de un tratamiento
# De esa forma tenemos un misto para la varianza.
# Queremos estudiar la relacion num.palabras recordadas con el tipo de valencia y tipo de tarea, entre
# varios sujetos que tienen asociados 2 factores: genero y dosis
ej3<-read.table("anova_mixta.txt",header=T)
attach(ej3)

str(ej3)
# Miramos la tabla de frecuencias
xtabs(num.palabras~valencia+tipo.tarea+sujeto)
table(genero,dosis)

#Vemos las medias
tapply(num.palabras, list(genero,dosis,valencia,tipo.tarea), mean)
boxplot(num.palabras~valencia*tipo.tarea*genero*dosis)
interaction.plot(valencia,tipo.tarea,num.palabras)
interaction.plot(dosis,valencia,num.palabras)


# Suponemos normalidad entre las diferentes tratamientos y varianzas iguales
# Aplicamos la anova
p.aov<-aov(num.palabras~(valencia*tipo.tarea*genero*dosis)+Error(sujeto/(valencia*tipo.tarea))+(genero+dosis))
summary(p.aov)
# Aqui podemos ver muchisimas combinaciones:
# 1. De cada sujeto con sus factores dependientes: genero y dosis
#  Error: sujeto
#               Df Sum Sq Mean Sq F value Pr(>F)  
# genero        1  425.0   425.0   4.088 0.0682 .
# dosis         2  688.0   344.0   3.309 0.0750 .
# genero:dosis  2   62.5    31.2   0.300 0.7464  
# Residuals    11 1143.7   104.0 
# Relativamente se puede aceptar H0 (por poco), sus medias son iguales entonces Sujeto depende de genero y de dosis

# 2. Cada factor general dentro de sujeto: sujeto:tipo.tarea, sujeto:valencia y su interaccion
# Error: sujeto:valencia
#                       Df Sum Sq Mean Sq F value Pr(>F)  
# valencia               2  14.69   7.343   4.396 0.0248 *
# valencia:genero        2   9.77   4.885   2.925 0.0748 .
# valencia:dosis         4  24.81   6.202   3.713 0.0186 *
# valencia:genero:dosis  4   3.99   0.996   0.596 0.6690  
# Residuals             22  36.75   1.670                 

# Error: sujeto:tipo.tarea
#                         Df Sum Sq Mean Sq F value   Pr(>F)    
# tipo.tarea               1  96.33   96.33  36.540 8.37e-05 ***
# tipo.tarea:genero        1   2.04    2.04   0.774    0.398    
# tipo.tarea:dosis         2   7.13    3.56   1.352    0.299    
# tipo.tarea:genero:dosis  2   3.16    1.58   0.600    0.566    
# Residuals               11  29.00    2.64                     

# Error: sujeto:valencia:tipo.tarea
#                                  Df Sum Sq Mean Sq F value Pr(>F)
# valencia:tipo.tarea               2   5.39  2.6944   1.674  0.211
# valencia:tipo.tarea:genero        2   5.15  2.5729   1.598  0.225
# valencia:tipo.tarea:dosis         4   4.77  1.1932   0.741  0.574
# valencia:tipo.tarea:genero:dosis  4   2.61  0.6522   0.405  0.803
# Residuals                        22  35.42  1.6098               
# Aqui se pueden sacar bastante conclusiones. Las mas importantes:
# a. El numero de palabras no depende de la valencia sola
# b. El numero de palabras no depende de la valencia y la dosis, y muy poquito de la valencia genero
# c. El numero de palabras no depende del tipo de tarea solo
# d. En el resto de combinaciones si vemos que las medias son iguales y por lo tanto una dependencia.
#    y la que mas nos interesa en este caso son valencia:tipo.tarea:genero:dosis.

# Por ultimo muestra la suma de cuadrados, media cuadratica y demas sin tener en cuenta el sujeto:
# Error: Within
# Df Sum Sq Mean Sq
# genero                      1 133.33  133.33
# valencia:genero             2   8.67    4.33
# tipo.tarea:genero           1   0.33    0.33
# valencia:tipo.tarea:genero  2   8.67    4.33


# Conclusion: valencia:tipo.tarea:genero:dosis intrasujetos tienen una dependencia con el numero de palabras recordadas
# Es decir, los factores tipo de valencia y tipo de tarea, para varios sujetos de diferentes generos y con diferentes dosis
# guardan relacion con el numero de palabras recordadas