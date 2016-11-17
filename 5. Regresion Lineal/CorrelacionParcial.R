# Limpiar el workspace
rm(list = ls(all = TRUE))
require(ggm)
##############FORMULACION PROBLEMA
# Para estimar la producción en madera de un bosque se
# suele realizar un muestreo previo en el que se toman una serie de mediciones
# no destructivas. Disponemos de mediciones para 20 árboles, así como el volumen
# de madera que producen una vez cortados
# Las variables observadas son:
# HT = altura en pies
# DBH = diametro del tronco a 4 pies de altura (en pulgadas)
# D16 = diametro del tronco a 16 pies de altura (en pulgadas)
# VOL = volumen de madera obtenida (en pies cubicos).
# El objetivo del análisis es determinar cual es la relacion entre dichas medidas y
# el volumen de madera, con el fin de poder predecir este ultimo en funcion de las primeras. 

##############MUESTRA
DBH <- c(10.2,13.72,15.43,14.37,15,15.02,15.12,15.24,15.24,15.28,
         13.78,15.67,15.67,15.98,16.5,16.87,17.26,17.28,17.87,19.13)
D16 <-c(9.3,12.1,13.3,13.4,14.2,12.8,14,13.5,14,13.8,13.6,14,
        13.7,13.9,14.9,14.9,14.3,14.3,16.9,17.3)
HT <-c(89,90.07,95.08,98.03,99,91.05,105.6,100.8,94,93.09,89,
       102,99,89.02,95.09,95.02,91.02,98.06,96.01,101)
VOL <-c(25.93,45.87,56.2,58.6,63.36,46.35,68.99,62.91,58.13,
        59.79,56.2,66.16,62.18,57.01,65.62,65.03,66.74,73.38,82.87,95.71)
bosque<-data.frame(VOL=VOL,DBH=DBH,D16=D16,HT=HT)
attach(bosque)

#############EXPLORACION DATOS
str(bosque)
summary(bosque)
# Se deberian explorar valores nulos, distribuciones, representaciones, valores atipicos.... etc...

#############RELACIONES
plot(bosque)
pairs(bosque)

############CORRELACIONES
##########Matriz de correlaciones

# var(bosque) y cov(bosque) y cor(bosque) es lo mismo
# Normalmente se usa var(x) y te da la varianza de cada valor de x sobre la media
# Pero var(x,y) te da la varianza de x con respecto a y = cov(x,y)
# La covarianza de x e y te da la correlacion entre esas dos variables = cor(x,y)
# La unica diferencia es que cor es un coeficiente y la covarianza es un sumatorio 
# de diferencias de unidades
# Cuando tu haces cor(matriz) equivalente a cov(matriz)=var(matriz)
cov(bosque)
var(bosque)
cor(bosque)

# D16 parece ser la variable más relacionada (linealmente) con VOL en presencia 
# de las restantes, y DBH la que menos.
# IMP: cor() te da la correlacion, no la correlacion parcial

####Correlacion con Contraste de hipotesis
# correlación
# H0: p=0 entre D16 y VOL
cor.test(VOL,D16)
# Aceptamos H1=> VOL y D16 estan correladas

####Correlaciones parciales
# Una vez que tu ya tienes las correlaciones con cor(bosque)
# Tu puedes sacar la correlacion parcial de cualquiera 2 quitando el resto con pcor
# indicandole la matriz de corelacion

# Correlacion parcial de VOL con D16, eliminando el sobre-ajuste de DBH y HT
pcor(c("VOL","D16","DBH","HT"),cor(bosque)) # 0.7627127
# Correlacion parcial de VOL con DBH, eliminando el sobre-ajuste de D16 y HT
pcor(c("VOL","DBH","D16","HT"),cor(bosque)) # 0.3683119
# Correlacion parcial de VOL con HT, eliminando el sobre-ajuste de D16 y DBH
pcor(c("VOL","HT","D16","DBH"),cor(bosque)) #0.7285511

###Matriz de correlacion parcial
# O toda la matriz de correlacion
parcor(cor(bosque))

#VOL        DBH        D16         HT
#VOL 1.0000000  0.3683119  0.7627127  0.7285511

# Con esto ya podemos ver realamente que VOL se relaciona sobre todo con D16 y con HT.
# Aunque DBH tiene una correlacion alta con VOL, luego tiene una correlacion parcial muy baja.



# ¿Cual es la variable explicativa que guarda más relación con la respuesta
# coeficiente de correlación parcial?
# Se define el coeficiente de correlaci´on parcial muestral entre una variable respuesta
# y una explicativa, dado un bloque adicional de variables explicativas,
# como aquel que cuantifica el grado de relaci´on lineal existente entre las dos primeras
# considerando (descontando) la relaci´on (lineal) entre ambas y el bloque
# de variables adicionales. Su utilidad es decisiva para el ajuste de modelos de regresi´on
# m´ultiple, en los que contamos con un grupo de variables explicativas a
# incorporar en el modelo de predicci´on. La correlaci´on simple entre una variable
# explicativa y la respuesta s´olo da informaci´on sobre la relaci´on entre ellas, obviando
# toda relaci´on con otras variables. La correlaci´on parcial a´isla la parte de
# las variables respuesta y explicativa que no est´a relacionada linealmente con las
# otras variables explicativas y da informaci´on sobre si dichos restos est´an relacionados
# linealmente, esto es, si la covariable en cuesti´on aporta algo de informaci´on
# (lineal) adicional sobre la respuesta, que no hayan aportado las otras variables.

parcor(cor(bosque))
#VOL        DBH        D16         HT
#VOL 1.0000000  0.3683119  0.7627127  0.7285511


cor(bosque)
#VOL       DBH       D16        HT
#VOL 1.0000000 0.9078088 0.9530963 0.6010862
# sólo con la correlación VOL parecía muy relacionada linealmente con DBH


# VISUALMENTE:

# Correlacion:
pairs(bosque)
#o
library(corrplot)
corrplot(cor(bosque))

#Correlacion parcial:
corrplot(parcor(cor(bosque)))

#########MODELO DE REGRESION...Por pasos

# primera variable mas relacionada parcialmente=D16
reg<-lm(VOL~D16)
summary(reg)
# Rsq ajusted=90% y los p-values muy bajos => Buen modelo

# segunda variable HT mas relacionada parcialmente=HT
# Se incorpora y se revisa el Rsq-ajusted y los p-values
reg1<-lm(VOL~D16+HT)
summary(reg1)
# Rsq-ajusted=94% y los p-values siguen siendo bajos. SE DEJA HT tambien


###########ERROR: Introducir variable no correcta
# Introducimos la primera variable
rg<-lm(VOL~D16)
summary(rg)
# Rsq ajusted=90% y los p-values muy bajos => Buen modelo

# vemos como se comportaria el modelo con DBH
rg<-lm(VOL~D16+DBH)
summary(rg)
# Rsq ajusted=90% pero el p-value de DBH alto => DBH Sobra
resd16dbh<-rg$residuals

# vemos como se comportaria el modelo con HT
rg<-lm(VOL~D16+HT)
summary(rg)
resd16ht<-rg$residuals
# Rsq-ajusted=94% y los p-values siguen siendo bajos. SE DEJA HT tambien => MEJOR MODELO

boxplot(resd16dbh,resd16ht)
summary(resd16dbh)
summary(resd16ht)
sd(resd16dbh)
sd(resd16ht)
# con HT (el 2) el modelo presenta menos varianza en los residuos que con DBH => Menor incertidumbre con HT

# Vemos como se comportaria el modelo con todas las variables
rg<-lm(VOL~DBH+D16+HT)
summary(rg)  
# Rsq-adj=95% pero DBH sigue teniendo p-value alto. No tiene una dependencia con VOL
# Hay que quitarlo

# Nos tenemos que quedar con la primera VOL=-105.9027+7.4128*D16+0.6765HT
# Rsq-ajusted>90% => Fuerte asociación entre las variables D16 y HT y el VOL de madera 
# producido
