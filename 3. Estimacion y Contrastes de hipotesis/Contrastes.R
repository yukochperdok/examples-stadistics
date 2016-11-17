# Limpiar el workspace
rm(list = ls(all = TRUE))
###########################
# Contrastes de hipótesis #
###########################

###########################
# 1.  Se tomaron las tensiones sanguıneas de una muestra aleatoria de 10 pacientes hipotensos, 
# obteniendose las mediciones: 10 10.5 11 10.7 10.8 12 11.5 9.1 11.3 9.9 . 
# Suponiendo una distribucion normal de las tensiones en la poblacion de hipotensos observada, 
# hallar un intervalo de conﬁanza al nivel del 90% para la varianza σ2 de esta poblacion.
# Plantear si la varianza de la poblacion seria mayor a 0.9

x <- c(10 , 10.5 , 11 , 10.7 , 10.8 , 12 , 11.5 , 9.1 , 11.3 , 9.9)
var.x<-var(x) # 0.724
sd.x <- sd(x) # 0.8508819
# df=n-1
df<-length(x)-1
# alfa=0.1 repartido entre 2
chisq.der<-qchisq(0.05,df, FALSE) #3.325113
chisq.der
chisq.izq<-qchisq(0.95,df, FALSE) #16.91898
chisq.izq

# intervalo pertenece a [3.7087, 18.74755]


#H0:var(x)<=0.9, H1:var(x)>0.9
# Calculamos el estadistico:
estadistico<-(df*sd.x^2)/0.9
estadistico

# Solo una cola al 10%
chisq.izq<-qchisq(0.90,df, FALSE) #14.68366
chisq.izq

# Conclusion: COmo estadistico<14.68366=> Aceptamos H0 pues estamos en zona de aceptacion.
# Por lo tanto se puede afirmar que var(x)<=0.9


# 2. Supongamos la primera muestra de diez alumnos matriculados en la Universidad de Córdoba 
# durante el año 2000, sus tallas son: 190; 199; 170; 189; 179; 221; 175; 193; 150; 209 
# Suponiendo que los datos procedan de una distribución Normal, contrástese la hipótesis nula 
# de que la varianza sea 500 cm2 contra la alternativa de que tenga otro valor, 
# con un nivel de significación del 10%. 

x <- c(190,199,170,189,179,221,175,193,150,209)
var.x<-var(x) # 410.7222
sd.x <- sd(x) # 20.26628
# df=n-1
df<-length(x)-1
# alfa=0.1 repartido entre 2
chisq.der<-qchisq(0.05,df, FALSE) #3.325113
chisq.der
chisq.izq<-qchisq(0.95,df, FALSE) #16.91898
chisq.izq

# intervalo pertenece a [3.325113,16.91898]


#H0:var(x)=500, H1:var(x)<>500
# Calculamos el estadistico:
estadistico<-(df*sd.x^2)/500
estadistico # 7.393

# COmo 3.325113 < estadistico < 16.91898 => Aceptamos H0 =>  Con nivel de confianza al 90% podemos 
# afirmar que la var(x)=500 para la poblacion



# 3. En un preparado alimenticio infantil se especifica que el contenido medio de proteÌnas es al menos del 42%.
# Tratamos de comprobar esta especificaciÛn y para ello tomamos 10 preparados que analizamos para determinar su contenido en proteínas, 
# obteniendo una media del 40% y una cuasidesviaciÛn tÌpica del 3.5%. 
# Es correcta la especificaciónn citada para un nivel de significaciÛn del 0.05, suponiendo normal la distribución de la variable contenido proteico?

# X: contenido proteico
n=10
media=40
desvia=3.5

# Contraste
#Ho: media >= 42
#H1: media < 42

# Estadístico
t= (40-42)/(desvia/sqrt(n))
t

# valor teórico 
qt(0.05,df=9)
-qt(.95, df=9)

# valor teórico < valor del experimento
# -1.833 < -1.8070
# No se rechaza Ho, admitimos como correcta la especificación del preparado acerca del contenido proteico
  
# OBS:
# Si no tenemos la muestra entera no podemos usar t.test, ya que necesita la muestra.
# A lo sumo te podrias generar 10 valores con mu=40 y sd=3.5 para una normal
x<-rnorm(10,mean = 40, sd = 3.5)
t.test(x, alternative="less", mu=42, conf.level=0.05)
# Si repetimos varias veces veremos como la mayoria el p-value es mayor que 0.05 con lo cual aceptariamos H0.
# Pero hay algunas veces que el p-value es mas pequeño, con lo cual rechazariamos H0, 
# ¿porque?
# Por que rnorm no siempre genera una media exacta de 40, y la prueba estaba super ajustada -1.833 < -1.8070.
# Con lo cual como genere una media algo mas baja de 40 nos va a salir un test erroneo.
# Por eso con pruebas tan ajustadas es mejor directamente coger mean y sd


###########################
# 4. Se realiza un estudio de prácticas de prescripción. 
# El propósito es analizar la prescripción de digoxina, un fármaco importante, potencialmente tóxico y
# comúnmente utilizado. El nivel de dosificación para los mayores de 64 aÒos debe ser menor que el de
# personas más jóvenes. Se extraen muestras independientes de cada grupo y se obtiene el nivel de
# dosificación para cada paciente seleccionado. Los resultados son:
# X : mayores de 64 años: n=41, media=0.265 mg/dia, desviación=0.102mg/dia
# Y : hasta 64 años: n=29, media=0.268 mg/dia, desviación=0.068mg/dia  
# Se puede considerar que la dispersión en ambas poblaciones es la misma?

# Ho: varianza(x)=varianza(y)
# O lo que es lo mismo varianza(x)/varianza(y)=1 


# Estadístico
# F=(var_muest(x)/varianza(x))/(var_muest(y)/varianza(y))
# Como nuestra suposicion es varianza(x)/varianza(y)=1
# => F=var_muest(x)/var_muest(y)

F = (0.102)^2 / (0.068)^2         # F: 2.25
F

# Valor teórico
#df1=41-1, df2=29-1
# Importante la F de snedecor no es simetrica--> Hay que calcular para 1-alfa/2 y alfa/2
# Con alfa=0.05 => 0.025 y 0.975
qf(.025,40,28,lower.tail = TRUE) # valor: 0.5098037
qf(.975,40,28,lower.tail = TRUE) # valor: 2.047664
# o bien al reves
qf(.025,40,28,lower.tail = FALSE) # valor: 2.047664
qf(.975,40,28,lower.tail = FALSE) # valor: 0.5098037
# Lo importante es ver que esta no es simetrica


# el valor del estadístico está en la región de rechazo (0.5098037 < 2.047664 < 2.25), 
# las varianzas poblacionales son diferentes => La dispersion es diferente en ambas poblaciones

# Esto se podria haber hecho con var.test pero tenemos que tener las muestras. Por ejemplo
# X : mayores de 64 años: n=41, media=0.265 mg/dia, desviación=0.102mg/dia
x<-rnorm(41,mean = 0.265, sd = 0.102)
mean(x)
sd(x)
# Y : hasta 64 años: n=29, media=0.268 mg/dia, desviación=0.068mg/dia  
y<-rnorm(29,mean = 0.268, sd = 0.068)
mean(y)
sd(y)

#H0:var(x)/var(y)=ratio=1--> 2 colas
var.test(x,y,ratio=1,alternative = "two", conf.level = 0.05)

# Nos salen muchos casos en los que la media y la sd no se ajustan bien a los datos, por rnorm.
# Y por ello hay veces que el estadistico que nos sale esta entre los qt.
# Lo suyo cuando se tienen las 2 muestras es hacer esto, pero si no se tiene mejor utilizar directamente
# los datos tal y como hemos hecho antes.


###########################
# 5. Se mide el número de partÌculas que llegan a una determinada zona procedentes
# de una sustancia radioactiva en un corto espacio de tiempo siempre igual, 
# anotándose los resultados en la siguiente tabla:
# nº partículas: 0, 1, 2, 3, 4, 5, 6
# nº periodos de tiempo: 269, 325, 207, 82, 28, 7, 2
#
# Verificar que se ajusta a una distribución de Poisson

x<-c(0, 1, 2, 3, 4, 5, 6)
#Valores observados
y<-c(269, 325, 207, 82, 28, 7, 2)

# Calculamos su media
lbd = sum(x*y)/sum(y)
lbd

#probabilidades esperadas para que sea Poisson:
dpois(0,lambda = lbd)
dpois(1,lambda = lbd)
dpois(2,lambda = lbd)
dpois(3,lambda = lbd)
dpois(4,lambda = lbd)
dpois(5,lambda = lbd)
dpois(6,lambda = lbd)

# Para 6 tenemos el valor 2 --> Agrupamos en 5 categorias
x<-c(0, 1, 2, 3, 4, 5)
y<-c(269, 325, 207, 82, 28, 9)


#probabilidades esperadas para que sea Poisson:
probs=c( dpois(0,lambda = lbd) ,
         dpois(1,lambda = lbd) ,
         dpois(2,lambda = lbd) ,
         dpois(3,lambda = lbd) ,
         dpois(4,lambda = lbd) ,
         dpois(5,lambda = lbd) + dpois(6,lambda = lbd) )

# np
np=sum(y)*probs

#sumatorio(ni - npi)^2 / npi    #valor del experimento
sum((y - np)^2 / np )  # 0.4849

# valor teórico
# df= num_categorias-1=4
qchisq(.95,4)  #9.4877

# el valor del experimento no es mayor que el valor teórico, por ello no se rechaza Ho
# se acepta que se distribuye como una Poisson

# Esto se puede hacer como chisq.test
chisq.test(x=y,p=probs,correct = FALSE, rescale.p = TRUE)


###########################
# 6. Lanzamiento de 120 veces de un dado con las siguientes observaciones
#  Numero del dado:           1  2  3  4  5  6
#  Numero de veces que sale: 20 22 17 18 19 24
# Verificar que se ajusta a una distribución binomial

x<-c(0, 1, 2, 3, 4, 5, 6)
#Valores observados
y<-c(20, 22, 17, 18, 19, 24)
# Si sigue una binomial los 120 lanzamientos se deberian repartir en probabilidades iguales:1/6
# quedando B(120,1/6)
# con lo cual H0~B(120,1/6)
chisq.test(x=y,p=rep(1/6,6),correct = FALSE)
# p-value>0.05=> Acepto H0 => El lanzamiento de este dado en condiciones normales a un
# nivel de confianza del 95% se distribuye como una Binomial


###########################
# 6. Una variable X se que tiene una mean=3.5 y una sd=0.7 tenemos las siguientes observaciones
#  Limites clase bateria:             Observaciones:
#     1.45-1.95                           2
#     1.95-2.45                           1
#     2.45-2.95                           4
#     2.95-3.45                           15
#     3.45-3.95                           10
#     3.95-4.45                           5
#     4.45-4.95                           3
#
# Verificar que se ajusta a una distribución normal
# Lo primero seria hacer una correccion de Yates agrupando las categorias 1.45 a 2.95 y 3.95 a 4.95:
#  Limites clase bateria:             Observaciones:
#     1.45-2.95                           7
#     2.95-3.45                           15
#     3.45-3.95                           10
#     3.95-4.95                           8
x<-factor(c("1.45-2.95","2.95-3.45","3.45-3.95","3.95-4.95"))
y<-c(7,15,10,8) 

# Segundo calcular las probabilidades esperadas para una normal
null.props<- c(pnorm(2.95,mean = 3.5,sd = 0.7, lower.tail = TRUE) - pnorm(1.45,mean = 3.5,sd = 0.7, lower.tail = TRUE),
              pnorm(3.45,mean = 3.5,sd = 0.7, lower.tail = TRUE) - pnorm(2.95,mean = 3.5,sd = 0.7, lower.tail = TRUE),
              pnorm(3.95,mean = 3.5,sd = 0.7, lower.tail = TRUE) - pnorm(3.45,mean = 3.5,sd = 0.7, lower.tail = TRUE),
              pnorm(4.95,mean = 3.5,sd = 0.7, lower.tail = TRUE) - pnorm(3.95,mean = 3.5,sd = 0.7, lower.tail = TRUE))

null.props

# OBS: Tambien se podian haber tipificado los valores: 2.95, 3.45, 3.95, 4.95 
# y luego haber calculado cada uno de los pnorm con mean=0 y sd=1

# Las frecuencias esperadas serian:
npi<-sum(y)*null.props
npi
#8.572593 10.220436 10.732531  9.639961

# nuestro estadistico seria:
estadistico<-sum((y-npi)^2/npi)
estadistico # 2.852626

# Nuestro chisq para 0.05 y df=k-1=4 seria:
qchisq(0.05,df = 3, lower.tail = FALSE) # 7.814728

# Como estadistico es menor que la chisq nula podemos afirmar que H0 es cierta=> Se distribuye como una normal

#Mas facil con un test, habiendo calulado props.null
chisq.test(x=y, p=null.props, rescale.p = TRUE, correct = FALSE, simulate.p.value = 0.05)
# Estadistico=2.7757 con p-value=0.4275 mayor que 0.05 => Puedo aceptar H0 => Se distribuye como una normal





###########OTROS CONTRASTES#################
#########Kolmogorov-Smirnov#######
## Para saber si una muestra proviene de una distribucion F()
## Para n<20-30 y no esta pensado para Normales aunque se puede usar, pero mejor usar Shapiro

# Ejemplo La serie de datos (172; 178; 148; 145; 188; 193; 186; 199; 187; 177)
# ¿Se podrá aceptar, con un nivel de significación del 5% que este conjunto de datos 
# procede de una distribución Normal, N(175;500)? 
x<- c(172,178,148,145,188,193,186,199,187,177)
x.norm<-rnorm(10,mean = 175, sd=500)

# Se plantea el siguiente contraste:
# Ho: Los datos proceden de una Normal  ( de media 175 y de varianza 500) x=x.norm 
# H1: Los datos proceden de otra distribución (o de una Normal con otros parámetros) x<>x.norm

ks.test(x,x.norm,alternative = "two",exact = TRUE)
# Estadistico D me dice que la mayor diferencia de probabilidades entre x y una normal con frecuencias
# acumuladas es de 0.6 y el p-value<0.05 con lo cual rechazo H0 
# X no proviene de una N(177.3,325.789)

#OBS 
#Si quisieramos H0:x<=x.norm/H1:x>x.x.norm => "greater"
#Si quisieramos H0:x>=x.norm/H1:x<x.x.norm => "less"
# Pero esto no tiene mucho sentido

#OBS2:
# Esto se puede hacer para comparar con cualquier distribucion. Incluso para saber si dos muestras 
# vienen del mismo origen o sigue una misma distribucion.

# Para comprobar H0:
x.norm<-rnorm(10,mean = 175, sd=500)
y.norm<-rnorm(10,mean = 175, sd=500)
ks.test(x.norm,y.norm)
# p-value>>0.05 => Aceptamos que ambas poblaciones vienen del mismo distribucion


#########Shapiro-Wilk#######
# Te dice si una muestra procede de una N(mu,sd)
# Muy potenete para n<30-50

x<- c(172,178,148,145,188,193,186,199,187,177)

# Ho: x~N(mu,sd)
shapiro.test(x)
# Te indica como estadistico W=0.87883 muy cercano a 1 => H0 aceptada
# Pero mejor verlo con p-value = 0.1265 > 0.05 => Se acepta H0 => X no procede de una normal, con media y varianza desconocidas

#########Lilliefors - Test#######
# Sirve exactamente lo mismo que el de shapiro. Pero normalmente se usa para todo tipo de n
x<- c(172,178,148,145,188,193,186,199,187,177)

require(nortest)
# Ho: x~N(mu,sd)
lillie.test(x)
#  p-value = 0.3597> 0.05 => Se acepta H0 => X no procede de una normal, con media y varianza desconocidas
