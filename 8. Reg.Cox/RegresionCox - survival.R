# Limpiar el workspace
rm(list = ls(all = TRUE))

# Si tenemos una variable dicotomica que queremos predecir en funcion de variables independientes.
# Si en vez de hacer una regresion logistica para predecir que que porcentaje tendra 0 y 1, 
# queremos saber el instante de tiempo (o de longitud, o de espacio, etc..) en el que la variable 
# predicha va a tomar el valor 1. Estamos hablando de un analisis de supervivencia y la mejor forma 
# de modelarlo seria con la regresion de Cox.

# Para ello tenemos que:
#   1. seleccionar los sujetos a estidiar
#   2. Seleccionar la mediad de tiempo (o longitud...) con la que trabajar: dias, horas, meses, etc..
#   3. Seleccionar tramo de tiempo del estudio.
#   4. Seleccionar evento que ha de cumplirse en los sujetos para ser estudiado: Por ejemplo alta de un cliente (para un analisis de fuga)
#       y seleccionar su evento final (por ejemplo baja cliente) es decir evento a estudiar.
#       El tramo entre alta cliente y baja cliente se llama tiempo de trama o muerte.
# CENSURA:
#   1. Es posible que haya sujetos que termine el estudio y no hayan tenido el evento de estudio
#      A esto se le dice que estos sujetos estan censurados por la derecha.
#   2. Si al inicio del estudio hay sujetos que aun no han tenido el evento de inicio (por ejemplo una edad determinada)
#      Y este se produce durante el estudio, se dice que estan censurados por la izquierda.
# TRUNCAMIENTO:
#   1. Si hay sujetos que al inicio del estudio han tenido ya el evento de estudio, no entran al estudio
#      Se dice que estan truncados por la izquierda
#   2. Si solo se quiere contar x sujetos que cumplan el evento final, los x restantes que los cumplan
#      no se tendran en cuenta, es un truncamiento por la derecha.
#############################EJEMPLOS BASICOS#########################
# Cargar librarias
library("survival")

### OBJETOS SURV
# Para crear un objeto de tipo survival utilizamos Surv(time,event)
# time es el vector de tiempos
# event te indica que elementos estan censurados por la derecha

# Un sujeto que ha tenido 5 meses (por ejemplo) de vida, y NO ha sido censurado
Surv(5,1) #{5}

# Un sujeto que ha tenido 5 meses (por ejemplo) de vida, y SI ha sido censurado
Surv(5,0) #{5+}, te lo indica con el +

# Con arrays se ve mejor:
tiempo <- c(1,2,3,4,5,6)
estatus <- c("falla","falla","falla","censura",
               "censura","censura")
Surv(tiempo,estatus=="falla") #{1  2  3  4+ 5+ 6+}

# Si por ejemplo se nos indica la falla por el num 7 y el resto son censurados:
tiempo <- c(1,2,3,4,5,6)
estatus <- c(7,7,7,"Censura","NA",0)
Surv(tiempo,estatus==7) #{1  2  3  4+ 5+ 6+}

## CURVA DE SUPERVIVENCIA
# Se define la curva de supervivencia como la posibilidad de supervivir de los sujetos en 
# cada tiempo ti. El mejor estimador de una curva de supervivencia o funcion de supervivencia,
# es el estimador Kaplan-Meier (KM)

# Para ello es facil modelarla con survfit(formula)
tiempo <- c(1,2,3,4,5,6,7,8,9,10)
estatus <- c(1,1,1,1,1,0,0,0,0,0)
datos<-Surv(tiempo,estatus)

# Creamos una formula basica sobre el objeto surv
survfit(datos~1)
# n--> num sujetos
# events --> nuemro de sujetos que presentan el evento
# Por lo tanto el numero de sujetos censurados es n-events
# median es el tiempo mediano en la curva antes que se presente la falla.
# Es decir el primer S(t)<=0.5
# Y las correspondientes bandas al median.
# Importante cuando nos aparece NA, se refiere a Inf que en este caso hace referencia al 1. 

# Lo suyo es verlo con el summary:
K_M <- survfit(datos~1)
summary(K_M)
# Te va poniendo en cada tiempo: 1,2,3,...
# Los elementos que estan en riego y la probabilidad de supervivencia, junto con su error estandar
# y sus bandas de confianza.
# Es decir en el time=3, han muerto 2 sujetos (el primero y el segundo) y quedan en riesgo otros 8
# en ese tiempo hay un n.event con lo cual morira un nuevo sujeto, en el tiempo siguiente ya solo nos
# quedaran 7 elementos en riesgo.

# Si queremos ver solamente unos tiempos en concreto se lo decimos asi
time <- c(0,.5,1,2,3.7,4,5,6,10,11,20)
summary(K_M, time)

# Se pueden ver las variables de la curva:
names(K_M)
# Los tiempos:
summary(K_M)$time
# Las probabilidades:
summary(K_M)$surv

# Y muy importante: Graficarla
# Sin intervalos
plot(K_M, conf.int=F)
# Con intervalos por defecto
plot(K_M)

# En bonito
plot(K_M,conf.int=F, main = "Funcion de supervivencia",
     xlab= "Tiempo ",ylab=" Probabilidad de supervivencia",lwd=2, col ="blue")
box(lwd=3, col = "black")
axis(1, seq(0,10,1))
axis(2, seq(0,1,.1))
abline(h = seq(0,1,.1), v =seq(0,10,.5),lty=3,col ="gray")
legend("bottomleft",c("Curva de supervivencia"),lty=1,col="blue")


##FUNCION DE RIESGO
# Se define la funcion de riesgo como la probabilidad de que a un sujeto x le suceda el
# evento a estudiar en el instante de tiempo siguiente.
# La funcion de riesgo acumulada va acumulando las probabilidades de riesgo con el tiempo
# Y es inversamente proporcional a la curva de supervivencia

# Se calcula la funcion de riesgo acumulada como:
plot(K_M,conf.int=F,fun="cumhaz")
# Se ve que con el tiempo, eje horizontal, van aumentando los riesgos, llegando al 0.7 cuando t=5


##ANALISIS POR GRUPOS
# Se puede ver la curva de supervivencia agrupada por grupos.
# En realidad se hace una tabla de contingencia y se permite comparar si estas curvas de 
# supervivencia son iguales o no en cada tramo de tiempo.
# Esto ya venermos que es util para variables que de por si no pueden entrar en el modelo de Cox
# O simplemente ver la evolucion por grupos:
rm(list = ls(all = TRUE))
tabla <- read.table(text = "
                id tiempo status grupo
                  1 3 0 A
                  2 6 1 A
                  3 5 1 A
                  4 8 1 A
                  5 9 0 A
                  6 3 0 A
                  7 4 1 A
                  8 8 1 A
                  9 5 0 A
                  10 6 1 B
                  11 4 0 B
                  12 3 1 B
                  13 8 0 B
                  14 6 1 B
                  15 12 0 B
                  16 11 1 B
                  17 15 0 B
                  18 7 1 B
                  19 12 1 C
                  20 6 0 C
                  21 18 1 C
                  22 14 1 C
                  23 16 1 C
                  24 19 0 C
                  25 14 1 C
                  26 17 1 C
                  27 13 0 C",
                  header = TRUE, row.names = c("id"))

attach(tabla)
# Curva de supervivencia completa (sin grupos)
plot(survfit(Surv(tiempo,status)~1), conf.int=F,
     main="Funcion de supervivencia",
     xlab= "Tiempo de falla",
     ylab="Probabilidad de supervivencia")

# Curva por grupo:
plot(survfit(Surv(tiempo,status)~grupo), col=2:4,
     main="Funcion de supervivencia por grupo",
     xlab= "Tiempo de falla" ,
     ylab="Probabilidad de supervivencia")
     legend("bottomleft",c("Grupo A","Grupo B","Grupo C"),
     lty=c(1,1,1),col=2:4)

     
## Diferencia entre curvas
# Matematicamente se puede hacer este test de independencia para ver como de diferentes o iguales
# son los grupos, usando survdiff  
survdiff(Surv(tiempo,status)~grupo)
# Vemos el valor observado y el valor esperado y luego los calculos para hallar la chisq
# que resulta ser 8.8, con un p-value 0.0123<0.05 por lo tanto se rechaza la H0:igualdad de funciones 
# de supervivencia => Curvas diferentes.

detach(tabla)

     
##Intervalos de confianza
# Se pueden sacar varios tipos de intervalos para la curva de supervivencia:
tiempo <- c(1,2,3,4,5,6,7,8,9,10)
estatus <- c(1,1,1,1,1,0,0,0,0,0)
datos<-Surv(tiempo,estatus)

# Con el intervalo plano, el de por defecto
K_M.ordinario <- survfit (datos~1,conf.type = "plain" )
summary(K_M.ordinario)
plot(K_M.ordinario,xlab="Tiempo de falla",ylab="Probabilidad de supervivencia")

# Con el intervalo log
K_M.log <- survfit (datos~1,conf.type = "log" )
summary(K_M.log)
plot(K_M.log,xlab="Tiempo de falla",ylab="Probabilidad de supervivencia")

# Con el intervalo log-log
K_M.log.log <- survfit (datos~1,conf.type = "log-log" )
summary(K_M.log.log)
plot(K_M.log.log,xlab="Tiempo de falla",ylab="Probabilidad de supervivencia")

# Incluso reducir el intervalo con conf.int
K_M.ordinario_70 <- survfit (datos~1,conf.int=.70 )
summary(K_M.ordinario_70)
plot(K_M.ordinario_70,xlab="Tiempo de falla",ylab="Probabilidad de supervivencia")


# MODELO:
# El modelo que mejor se ajusta para un analisis de supervivencia.
# Es decir el que mejor predice el riesgo de una variable en el tiempo con
# respecto a los datos que van teneiendo en el tiempo una serie de covariables es
# la regresion de Cox
# Para modelar usamos coxph(Surv(tiempo,estatus) ~ cov1 + cov2 + ... + covn )
tiempo <- c(1,2,3,4,5,6,7,8,9,10)
estatus <- c(1,1,1,1,1,0,0,0,0,0)
cov1 <- c(0,1,1,1,0,0,1,0,1,0)
cov2 <- c(23,54,76,57,97,34,65,23,45,76)
coxph(Surv(tiempo,estatus) ~ cov1 + cov2)

#Los coeficientes estan dados en la columna coef, en el renglon correspondiente
# a cada covariable, en este caso, el coeficiente para las covariables cov1
# y cov2 son 0.58261 y 0.00905 respectivamente. En la columna denotada por
# exp(coef) se muestra el valor de cada coeficiente bajo la funcion exponencial,
# dado que este valor, es el que mide el impacto de cada variable explicativa
# en la curva de supervivencia en la interpretacion del modelo.

# Tambien te indican el error estandar de cada coeficiente (beta), con su estadistico y 
# p-value que te indica la significancia de cada coeficiente, sobre el contraste: H0:beta=0
# En nuestro caso ninguno de los coeficientes es significativo.

# Por ultimo te dan un test de likelihood, con H0: beta0=beta, es decir si es significativo el modelo
# en nuestro caso no es significativo el modelo.

# Aun se puede encontrar mas informacion:
summary(coxph(Surv(tiempo,estatus) ~ cov1 + cov2))
# Con el summary a parte de lo anterior tienes el exponente del inverso para interpretacion inversa,
# y los CI por defecto.
# Aparte te aparecen los 3 test para significacion del modelo. Ambos 3 muy parecidos: Likelihood, Wald y Score.
# Los 3 dicen que el modelo no es significante.
# Si el p-value fuera bajo seria un modelo aceptable

# Cuando ya tenemos el modelo ajustado podemos marcar la curva de supervivencia
(cox1<-survfit(coxph(Surv(tiempo,estatus) ~ cov1 + cov2)))
plot(cox1)
# OBS: La sobrevida mediana es NA, o Inf porque nunca llega a 0.5 (es logico que el 0.95UCL tambien lo sea)

# Incluso podemos comparar la curva estimada con KM sobre el modelo de cox hallado
km1<-survfit(Surv(tiempo,estatus) ~ 1, conf.int=F)

plot(cox1,conf.int=FALSE,main="Gráfico No. 3. Comparación del ajuste del modelo de Cox \n y el estimador de KM",xlab="Meses",ylab="Supervivencia")
lines(km1,lty=2)
legend("bottomleft",legend=c("Ajuste por Cox","Estimador de KM"),lty=c(1,2))
# Si nos fijamos el estimador de KM siempre da una estimacion algo mas baja que la regresion de cox

## Comprobacion supuesto proporcionalidad
# Todos los cocientes entre los exp(coef) son proporcionales y eso es obligatorio para un modelo de cox
# para comprobar esto se hace lo siguiente:
cox1<-coxph(Surv(tiempo,estatus) ~ cov1 + cov2)
cox.zph(cox1)
# En el caso de la cov1 parece que es superior a 0.05 pero en el caso de la cov2 incluso en global no
# se cumple que el p-value sea inferior a 0.05 o al menos 0.1 por lo tanto tanto para cov2 como en global
# no se respetan la proporcionalidad

# Graficamos los coeficientes:
plot(cox.zph(cox1))# Te muestra grafico para cov2, la unica significativa.
plot(cox.zph(cox1),var=1,main="Betas para cov1")
plot(cox.zph(cox1),var=2,main="Betas para cov2")

# Residuos
# Hay 4 tipos de residuos de la regresion de cox:
# Martingala, para ver la forma funcional de las variables:
plot(cov1,resid(cox1),xlab="Edad",ylab="Residuos de martingala",
     main="Verificacion de la forma funcional para cov1")
lines(lowess(cov1,resid(cox1),iter=0))

plot(cov2,resid(cox1),xlab="Edad",ylab="Residuos de martingala",
     main="Verificacion de la forma funcional para cov2")
lines(lowess(cov2,resid(cox1),iter=0))

# Desviance, para observar valores atipicos:
plot(resid(cox1,type="deviance"),xlab="Indice",ylab="residuos(tipodesvio)", main="Gráfico No. 7. Residuos (tipo deviance)")

# De score (o puntaje), los cuales sirven para ver los puntos influyentes:
rr<-resid(cox1,type="dfbeta")

plot(cov1, rr[,1], xlab="cov1", ylab="Influencia para
cov1",main="Gráfico de influencias para cov1")

plot(cov2, rr[,2], xlab="cov2", ylab="Influencia para
cov1",main="Gráfico de influencias para cov2")

# De schoenfeld, para asegurarnos el supuesto de proporcionalidad de coeficientes:
rr<-resid(cox1,type="schoenfeld")

plot(rr[,1], xlab="cov1", ylab="Prop. para
     cov1",main="Gráfico de proporcionalidad para cov1")
abline(h=0)

plot(rr[,2], xlab="cov2", ylab="Prop. para
     cov2",main="Gráfico de proporcionalidad para cov2")
abline(h=0)



##############################################################################
################################ EJEMPLO 1 ###################################
##############################################################################
# Se desea estudiar la supervivencia de 100 sujetos de diferentes edades (age), 
# de los cuales algunos fueron medicados (drug), y se tienen datos de el tiempo de falla (time)
# y si estan censurados o no por la derecha.
# Se tiene tambien la fevha de entrada y de salida del estudio (entdate y enddate).
# Se desea ajustar un modelo de cox que pueda predecir el indice de supervivencia de estos sujetos.


############################# ESTUDIO VARIABLES #################################
hmohiv<-read.table("http://www.ats.ucla.edu/stat/r/examples/asa/hmohiv.csv", sep=",", header = TRUE)
attach(hmohiv)
hmohiv

# Creamos una variable psymbol identica a censor que nos servira para el pch del plot
psymbol<-censor+1
table(censor)
table(psymbol)
#Existen 80 que fueron censurados por la derecha

# Miramos un grafico distribuido por censor:
plot(age, time, pch=(psymbol))
legend(40, 60, c("Censor=1", "Censor=0"), pch=(psymbol))
# Podemos ver con circulo los que no fueron censurados y tampoco me da mucha inforrmacion.
# Parece que la mayoria de sujetos se encuentran entre los 30 y 45 años, de los cuales hay 
# algunos que mostraron el evento al poco tiempo.
# De los mas jovenes mostraron el evento mucho mas tarde algunos de ellos.
# En cuanto a los mayores, muy pocos mostraron el vento, tan solo uno de mas de 45.

# Trasformamos la edad y volvemos a mirar (hacemos una especie de inverso de la edad):
age1<-1000/age
plot(age1, time, pch=(psymbol))
legend(40, 30, c("Censor=1", "Censor=0"), pch=(psymbol))
# Pudiendose hacer la misma interpretacion que antes, parece que el tiempo se va distribuyendo
# con respecto a la edad como una funcion exponencial.
# Es decir tenemos muchos datos de tiempo en muy corto espacio de tiempo al principio y luego
# parece que el evento se cumple menos en tramos de tiempo altos.
# Lo que si parece que esta claro es que la edad es algo que influye en el tiempo para la muerte del paciente


# Vemos si podria entrar en la regresion con una relacion exponencial. Hacemos un test:
test <- survreg( Surv(time, censor) ~ age, dist="exponential")
summary(test)
# Entraria con un coeficiente beta con valor -0.0939 y con un p-value bajisismo => Edad es sigificativa
# Ademas haciendo el test de likelihood parece claro que el modelo es bueno.

# Si lo intentaramos con otra distribucion???
test <- survreg( Surv(time, censor) ~ age, dist="lognormal")
summary(test)
# No obtenemos un p-value tan bajo aunque queda claro que podria incorporarse como lognormal.


# Podemos obtener una prediccion del modelo de supervivencia exponencial para los datos y graficarlo:
pred <- predict(test, type="response") 
# Ordenamos las edades
ord<-order(age)
age_ord<-age[ord]
# Y las predicciones en el mismo orden
pred_ord<-pred[ord]
# Graficamos
plot(age, time, pch=(psymbol))
lines(age_ord, pred_ord)
legend(40, 60, c("Censor=1", "Censor=0"), pch=(psymbol))
# Claramente es una exponencial


##########################FUNCION SUPERVIVENCIA################################
hmohiv.surv <- survfit( Surv(time, censor)~ 1, conf.type="none")
summary(hmohiv.surv)
plot (hmohiv.surv,  xlab="Time", ylab="Survival Probability" )
# Parece clarisimo que el riesgo al principio es muy alto, y luego a partir de los 15 dias 
# se va estabilizando.

# Y podemos agrupar en 6 meses:
library(KMsurv)
library(nlme)
# A que semestre pertenece cada mes
t6m<-floor(time/6)
# Juntamos t6m con su correspondiente censura
tall<-data.frame(t6m, censor)
# Agrupamos el sumatorio de censor por semestre en die 
die<-gsummary(tall, sum, groups=t6m)
# Agrupamos el nuemro de elementos por semestre en die
total<-gsummary(tall, length, groups=t6m)
rm(t6m)
ltab.data<-cbind(die[,1:2], total[,2])
#     t6m censor total[, 2]
# 0    0     41         51
# 1    1     21         24
# 2    2      6          8
# 3    3      1          2
# 4    4      0          1
# 5    5      5          5
# 6    6      1          1
# 7    7      1          1
# 8    8      1          1
# 9    9      3          4
# 10  10      0          2
detach(hmohiv)
attach(ltab.data)
lt=length(t6m)
# A los que tengan longitud mas 10 les ponemos NA
t6m[lt+1]=NA # 0  1  2  3  4  5  6  7  8  9 10 NA
nevent=censor
# Recogemos numero de elementos que ya han muerto para cada semestre
nlost=total[,2] - censor
# Utilizamos lifetab para hacer un analisis de supervivencia de forma tabulada
mytable<-lifetab(t6m, 100, nlost, nevent)
mytable[,1:5]
#      nsubs nlost nrisk nevent       surv
#0-1     100    10  95.0     41 1.00000000
#1-2      49     3  47.5     21 0.56842105
#2-3      25     2  24.0      6 0.31711911
#3-4      17     1  16.5      1 0.23783934
#4-5      15     1  14.5      0 0.22342483
#5-6      14     0  14.0      5 0.22342483
#6-7       9     0   9.0      1 0.14363025
#7-8       8     0   8.0      1 0.12767133
#8-9       7     0   7.0      1 0.11171242
#9-10      6     1   5.5      3 0.09575350
#10-NA     2     2   1.0      0 0.04352432
# Tenemos los sujetos de estudio(nsubs), los perdidos en ese semestre (nlost), los que estan en riesgo (nrisk)
# y los eventos que se producen (nevent), con su correspondiente probabilidad (surv)

# Graficamos
plot(t6m[1:11], mytable[,5], type="s", xlab="Tiempo supervivencia por cada 6 meses", 
     ylab="Porcion de supervivientes")
plot(t6m[1:11], mytable[,5], type="b", xlab="Tiempo supervivencia por cada 6 meses", 
     ylab="Porcion de supervivientes")
# Hay un riesgo de supervivencia muy alto en los 2 primeros semestres: del 100% al 56% y del 56% al 31%.
detach(ltab.data)

##########################INTERVALOS DE CONFIANZA################################
# TENEMOS 2 POSIBLES INTERVALOS DE CONFIANZA: Pointwise y Hall-Wellner
# para la estimacion de un KM.
# El pointwise es el tipico que utilizamos, y el de Hall-Wellner que es algo mas exacto.
# Para ello utilizamos la libreria km.ci
library(km.ci)

attach(hmohiv)
# Estimamos la curva
hmohiv.surv <- survfit( Surv(time, censor) ~ 1)
# Hallamos las bandas de confianza para Hall-Wellner
a<-km.ci(hmohiv.surv, conf.level=0.95, tl=NA, tu=NA, method="loghall")
par(cex=.8)
# Pintamos la curva con sus bandas de Hall-Wellner
plot(a, lty=2, lwd=2)

# Calculamos las bandas por defecto: Pointwise
time.conf <- survfit( Surv(time, censor)~ 1)
# Las pintamos
lines(time.conf, lwd=2, lty=1)
lines(time.conf, lwd=1, lty=4, conf.int=T)
linetype<-c(1, 2, 4)
legend(40, .9, c("Kaplan-Meier", "Hall-Wellner", "Pointwise"), lty=(linetype))

# Las bandas de confianza de Hall-Wellner son mas amplias pero mas exactas para un survival

#############################COMPARACION POR GRUPOS############################
# 1. CON COVARIABLE drug:
# Vamos a hacer dos grupos: Los que tomaron el medicamente(drug=1) y los que no lo tomaron
# Revisamos si sus curvas son significativamente diferentes:

# Test de log-rank de Mantel-Haenszel:
survdiff(Surv(time, censor) ~ drug, rho=0)
# Test de Peto y Peto 
survdiff(Surv(time, censor) ~ drug, rho=1)

# En ambos vemos que nuestro p-value es menor que 0.05 por lo tanto tenemos diferencias en las curvas.
# Por lo tanto se podria pensar en que es una variable interesanta para el modelo en caso
# de que cumpla proporcionalidad.
# Vemos las 2 curvas de supervivencia:
timestrata.surv <- survfit(Surv(time, censor) ~ drug, hmohiv, conf.type="log-log")
plot(timestrata.surv, lty=c(1,3), xlab="Tiempo", ylab="Probabilidad de supervivencia")
legend(40, 1.0, c("Drug - No", "Drug - Si") , lty=c(1,3) ) 
# Vemos como es mucho menor la probabilidad de supervivencia de las personas que tomaron los medicamentos,
# comparado con las que no lo tomaron.
# De hecho se alcanza la probabilidad minima a los 15 times en drug=1 y casi a los 60 times
# en drug=0
# Por lo tanto un factor de riesgo claro identificado es drug=1

# 2. CON COVARIABLE age:
# El caso de age es un tanto diferente puesto que ya hemos visto que esta sumamente correlacionada en el tiempo
# pero es discreta, por ello haremos estratos agecut(grupos):
agecat <- cut(age, c(19.9, 29, 34, 39, 54.1))
age.surv <- survfit( Surv(time, censor) ~ strata(agecat), conf.type="log-log")

print(age.surv)

# Graficamos:
plot(age.surv, lty=c(6, 1, 4, 3), xlab="Tiempo", ylab="Probabilidad de supervivencia")
legend(40, 1.0, c("Group 1", "Group 2", "Group 3", "Group 4"), lty=c(6, 1, 4, 3)) 
# Grupo 1-->(19.9,29] --> Mayor probabilidad de supervivencia de todos
# Grupo 2-->(29,34] --> El 2º con mayor prob. de supervivencia
# Grupo 3-->(34,39] --> Menor supervivencia que el 2 pero cuando pasan los 10 times empieza a parecerse al 2
# Grupo 4-->(39,54.1] --> Mayor probabilidad de muerte

# De hecho se puede ver que las diferencias de curvas son significativas:
survdiff(Surv(time, censor) ~ agecat, rho=0)
survdiff(Surv(time, censor) ~ agecat, rho=1)

##########################COMPARACION ESTIMADORES##############################
# Existen dos estimadores para la curva de supervivencia, el mas ultilizado es el Kaplan-Meier
# pero exite otro practicamente identico que se llama Nelson-Aalen.

# Los comparamos:

# Nelson-Allen
a<- survfit(coxph(Surv(time,censor)~1), type="aalen")
summary(a)
# IMP: Hay que calcular el inverso del logaritmo de la probabilidad
h.aalen<-(-log(a$surv))
aalen.est<-cbind(time=a$time, d=a$n.event, n=a$n.risk, h.aalen, s1=a$surv)

# Kaplan-Meier
b<-survfit(Surv(time, censor)~1)
km.est<-cbind(time=b$time, s2=b$surv)

# Mezclamos ambos por timepo
all<-merge(data.frame(aalen.est), data.frame(km.est), by="time")
all
#    time  d   n   h.aalen         s1         s2
# 1     1 15 100 0.1500000 0.86070798 0.85000000
# 2     2  5  83 0.2102410 0.81038895 0.79879518
# 3     3 10  73 0.3472273 0.70664471 0.68937118
# d: nuemro de eventos, n: numero de sujetos en riesgo

plot(all$time, all$s1, type="s", xlab="Survival Time (Months)", 
     ylab="Survival Probability")
points(all$time, all$s1, pch=1)
lines(all$time, all$s2, type="s")
points(all$time, all$s2, pch=3)
legend(40, .8, c("Nelson-Aalen", "Kaplan-Meier"), pch=c(1, 3))
# Son practicamente identicas, algo mas alta la de Nelson-Aalen

#########################HAZARD RATIO############################
# nuevo de eventos entre numero de sujetos en riesgo
# Es decir probabilidad de morir en cada tiempo
h2<-all$d/all$n
plot.new()
# Visualizamos la probabilidad de morir en cada tramo de tiempo
# Hazard Ratio
plot(all$time, h2, type="p", pch=20, xlab="Survival Time (Months)", 
     ylab="Hazard Ratio")
lines(lowess(all$time, h2,f=.75, iter=5))
# Vemos que el Hazar ratio o tasa de riesgos, va cambiando con el tiempo.
# Al principio es muy alta, va a bajando hasta situarse en el 0.06 aprox en el time 20.
# Y posteriormente epieza a ir incrementandose.


########################MODELADO################################
# Para crear un modelo de Cox entre otras necesitamos saber los datos de tiempo si son: incompletos, completos
# y si hay empates (momentos de fallo observados empatados).
# Hay 3 tipos de algoritmos:
#   1. Metodo Efron: "efron", opcion por defecto, es mas eficiente computacionalmente para 
#      tiempos continuos. Cuando el numero de empates es elevado es mejor utilizar este, se acerca 
#      al metodo exact.
#   2. Metodo "Exact partial likelihood": "exact", es equivalente a un metodo logistico condicional
#      y es adecuado para tiempos discretos y de pequeño tamaño. Si los datos son excesivamente grandes
#      puede ser excesivo computacionalmente.
#   3. Metodo Breslow, "breslow", computacionalmente mas libiano

# Vamos a probar con age, drug y su combinacion:
# 1. SOLO AGE
age.coxph <- coxph( Surv(time,censor)~age, method="breslow")
summary(age.coxph)
# Muy ajustado

# Probamos a crear una categoriaca de age:
# 20:29='A' 
# 30:34='B' 
# 35:39='C'
# 40:54='D'
library(car)
agecat<-recode(age, "20:29='A'; 30:34='B'; 35:39='C';40:54='D'", as.factor=T)
agecat.ph <- coxph( Surv(time, censor)~agecat, method="breslow")
summary(agecat.ph)
# Categoria referencia A, son bastante significativas B, C y D

# Si cambio los contrastes y dejo la referencia sobre la 4 (la D):
agecat<-recode(age, "20:29='D'; 30:34='B'; 35:39='C';40:54='A'", as.factor=T)
contrasts(agecat) <- contr.sum(levels(agecat)) 
agecat.ph <- coxph( Surv(time, censor)~agecat, method="breslow")
summary(agecat.ph)
# La 1 (A) si me sale significativa pero la 2 (B) y la 3 (C) NO.
# IMP: Decidir la categoria de referencia

# 2. SOLO DRUG
drug.coxph <- coxph(Surv(time,censor)~drug, method="breslow")
summary(drug.coxph)
# Bastante ajustado

# 3. AGE, DRUG E INTERACCION
inter.coxph <- coxph( Surv(time,censor)~age+drug+age*drug, method="breslow")
summary(inter.coxph)
# drug y la combinacion no son significativos

# 4. CON AGE Y DRUG SOLO
main.coxph <- coxph( Surv(time,censor)~age+drug, method="breslow")
summary(main.coxph)
# Muy ajustado con age y con drug

# Con efron tenemos lo mismo:
efron.coxph <- coxph( Surv(time,censor)~age+drug, method="efron")
summary(efron.coxph) 

#######################RESPRESENTACION MODELO############################
# Vamos a representar varios modelos con plot
# Modelo 1
fig4_2.ph <- coxph( Surv(time, censor)~drug, method="breslow")

drug.new<-data.frame(drug=c(0,1))
plot(survfit(fig4_2.ph, newdata=drug.new), xlab="Survival Time (Months)",
     ylab="Survival Probability")
points(survfit(fig4_2.ph, newdata=drug.new),pch=c(1,2))
legend(40, .8, c("Drug Absent", "Drug Present"), pch=c(1,2))
# Como ya vimos en el estimador, la presencia del medicamento disminuye la probabilidad de supervivencia

# Modelo 2
# tratamos como variable la edad menos 30 años --> I(age-30)
I(age-30)

age30.ph <- coxph( Surv(time, censor)~drug+I(age-30), method="breslow")

drug.new<-data.frame(drug=c(0,1), age=c(30,30))
plot(survfit(age30.ph, newdata=drug.new),xlab="Survival Time (Months)",
     ylab="Survival Probability")
points(survfit(age30.ph, newdata=drug.new),pch=c(1,2))
legend(40, .8, c("Drug Absent", "Drug Present"), pch=c(1,2))
# Incluyendo la edad - 30 y drug, tenemos una regresion con probabilidades mas altas que la anterior


# Modelo 3
# tratamos como variable la edad menos 45 años --> I(age-45)
I(age-45)

age45.ph <- coxph( Surv(time, censor)~drug+I(age-45), method="breslow")

drug.new<-data.frame(drug=c(0,1), age=c(45,45))
plot(survfit(age45.ph, newdata=drug.new),xlab="Survival Time (Months)",
     ylab="Survival Probability")
points(survfit(age45.ph, newdata=drug.new),pch=c(1,2))
legend(40, .8, c("Drug Absent", "Drug Present"), pch=c(1,2))
# Y sin embargo con la edad -45 vemos una probabilidad de supervivencia mucho menor

#######################PLOT LOG HAZARD############################
agecat<-recode(age, "20:29='A'; 30:34='B'; 35:39='C';40:54='D'", as.factor=T)
agecat.ph <- coxph( Surv(time, censor)~agecat, method="breslow")
summary(agecat.ph)

names(agecat.ph)
agecat.ph$coefficients
age.coeff<-data.frame(agecat.ph$coefficients)
age.plot<-cbind(midpt=c(24, 30.5, 35.5, 47.5), rbind(0, data.frame(age.coeff)))
age.plot
plot(age.plot[,1], age.plot[,2], ylab="Log Hazard", xlab="Age", type="b")
# Aqui se ve como va variando el coefficiente de edad en funcion al tramo de edad

# OBS: COn drug no se puede hacer puesto que drug=0 --> Seria 0 su coeficiente 
# y drug=1 seria 0.77 su coeficiente
drug.coxph$coefficients



########################ANALISIS RESIDUOS################################
main.coxph <- coxph( Surv(time,censor)~age+drug, method="breslow")
summary(main.coxph)

# Residuos martingala (mirar forma funcional covariables)
resid_martingala<- residuals(main.coxph, type="martingale", data=hmohiv)
plot(hmohiv$age, resid_martingala, xlab="Age",ylab="Martingale Residuals")
lines(lowess(hmohiv$age, resid_martingala))

plot(hmohiv$drug, resid_martingala, xlab="Drug",ylab="Martingale Residuals")
lines(lowess(hmohiv$drug, resid_martingala))
# Ambos tienen una forma funcional lineal

# Scores (mirar puntos influyentes y outliers)
resid_score<- residuals(main.coxph, type="score", data=hmohiv)
plot(hmohiv$age, resid_score[,1], xlab="Age",ylab="Score Residuals")
lines(lowess(hmohiv$age, resid_score[,1]))
# No hay puntos influyentes, tal vez entre el 40 y 45, pero la recta no se curva mucho

plot(hmohiv$drug, resid_score[,2], xlab="Drug",ylab="Score Residuals")
lines(lowess(hmohiv$drug, resid_score[,2]))
# No hay puntos influyentes, se compensan

#######################PROPORCIONALIDAD################################
# Comprobamos si cumplen proporcionalidad:
zph.main.coxph <- cox.zph(main.coxph, transform = 'log')
print(zph.main.coxph)
# Este objeto contiene realemente los residuos de Schoenfeld y hace un contraste de hipotesis
# de forma que como los p-value son mayores que 0.05 tenemos que aceptar H0: si hay proporcionalidad

# Para verlo graficamente, deben estar parecida a una recta paralela al y=0.
# Es decir no debe cruzarla.
plot(zph.main.coxph[1])
abline(h=0, lty=3)

plot(zph.main.coxph[2])
abline(h=0, lty=3)

# Ejemplo de no proporcionalidad
inter.coxph <- coxph( Surv(time,censor)~age+drug+age*drug, method="breslow")
summary(inter.coxph)
zph.inter.coxph <- cox.zph(inter.coxph, transform = 'log')
print(zph.inter.coxph)
plot(zph.inter.coxph[3])
abline(h=0, lty=3)



##############################################################################
##############################EJEMPLO 2#######################################
##############################################################################

# Mayo Clinic Lung Cancer Data
help(lung)
# time --> Dias para morir
# death --> Indicador de muerte (1=muerte)
# time2 --> Dia al 3/31/80 o muerte
# death2 --> Indicador de muerte del 3/31/80 (1=muerto, 0=vivo)

# Crear objeto Surv
survobj <- with(lung, Surv(time,status))

# Curva de supervivencia para toda la muestra, estimador KM
fit0 <- survfit(survobj~1, data=lung)
summary(fit0)
plot(fit0, xlab="Survival Time in Days", 
     ylab="% Surviving", yscale=100,
     main="Survival Distribution (Overall)") 

# Comparar las curvas de superv. entre generos 
fit1 <- survfit(survobj~sex,data=lung)
plot(fit1, xlab="Survival Time in Days", 
     ylab="% Surviving", yscale=100, col=c("red","blue"),
     main="Survival Distributions by Gender") 
legend("topright", title="Gender", c("Male", "Female"),
       fill=c("red", "blue"))

# Testear diferencia de curvas por generos
survdiff(survobj~sex, data=lung) 
# 0.00131<0.05--> CUrvas significativamente diferentes

# Predecir modelo de regresion para sexo masculino, a traves de la edad y puntuaciones clinicas 
MaleMod <- coxph(survobj~age+ph.ecog+ph.karno+pat.karno,
                 data=lung, subset=sex==1)
MaleMod
# age, ph.ecog y ph.karno, son suficientemente significativas como para quedarse en el modelo

# Evaluar proporcionalidad 
cox.zph(MaleMod) 
# La cumplen todos, aunque ph.karno muy justo