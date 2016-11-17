# Limpiar el workspace
rm(list = ls(all = TRUE))

# Cargar librarias
require(aod)
require(ggplot2)

########################FORMULACION PROBLEMA##################
# Un investigador está interesado en conocer la relación variables como: 
# GRE (Graduate Record Exam puntajes),
# GPA (promedio de calificaciones) 
# rank, el prestigio de la institución de grado
# Y el efecto de la admisión en la escuela de graduados: admit 

# La variable respuesta, admitir / no admitir, es una variable binaria.


# EXACTAMENTE LO MISMO QUE HEMOS HECHO CON UN MODELO LOGIT SE PUEDE HACER CON
# UN MODELO PROBIT. Modelo tambien logistico pero se basa en una funcion de area en 
# vez de con logaritmos.
# Te sirve de forma identica que logit para predecir una variable categorica, con
# dos categorias. La unica diferencia es que cuando plantees el gml debe ser de la 
# familia binomial, link=probit, en vez de logit.
# Hacemos exactamente los mismos pasos pero sin explicacion profunda.


mydata <- read.csv("http://www.ats.ucla.edu/stat/data/binary.csv")
## Primeras filas
head(mydata)


########################EXPLORACION VARIABLES#########################
# distribución de las variables
summary(mydata)
# Con esto ya podemos observar que admit, toma valores 0 a 1, y sobre todo mas 0's
# pues su media esta en 0.31
# Las puntuaciones van de 220 a 800 y la puntuacion media es 587 (parece una normal)
# El promedio de calificaciones va desde 2.2 a 4 y su media esta relativamente alta
# 3.3 (tambien parece una normal)
# El prestigio sin embargo parece un factor de 1 a 4

sapply(mydata, sd)
# Con esta instruccion contabilizamos los admit y no admit por rank
xtabs(~admit + rank, data = mydata)

# Aqui podemos ver como se reparten
boxplot(gre~admit,data = mydata)
boxplot(gpa~admit,data = mydata)
# Tanto en uno como en otro el RIC es mas o menos el mismo, pero los admitidos estan
# un poco mas arriba puesto que sus calificaciones son mejores.
# Otra forma de verlo
xtabs(~admit + cut(gre,breaks = 4), data = mydata)
xtabs(~admit + cut(gpa,breaks = 4), data = mydata)

# Podemos representarlos
hist(mydata$gre[mydata$admit==0])
hist(mydata$gre[mydata$admit==1])
qplot(gre,gpa,data = mydata, color=admit, facets = rank~.)
# Vemos claramente que a medida que va aumentando gre y gpa van aumentando los admitidos
# Y de la misma forma con rank a menor rank hay mas admitidos. 
# Es cierto que ha admitidos con un un rank=4 o con bajos gre y gpa pero los menos
# Como ultima observacion vemos que hay muy pocos individuos con un rank=1 que tengan
# calificaciones bajas
M <- cor(mydata)
corrplot(M)


# generamos factor(rank) para tratar como categórica
mydata$rank <- factor(mydata$rank)

########################MODELO LOGARITMICO - PROBIT #########################
# Antes de hacer una glm podemos ver con un cdplot la funcion de densidad (0,1) 
# de la VD con respecto a cada VI:
cdplot(factor(admit)~gre, data=mydata) # A medida que aumenta gre, aumenta el 1 de admit
cdplot(factor(admit)~gpa, data=mydata) # A medida que aumenta gpa, aumenta el 1 de admit
cdplot(factor(admit)~rank, data=mydata) # A medida que aumenta el rank, aumenta el 0 de admit
# Se ven claramente los 4 rank


myprobit <- glm (admit ~ gre + gpa + rank, mydata, family=binomial(link = "probit"))
summary(myprobit)

# Pequeñas diferencias en los coeficientes con respecto a logit, pero analogos.

########### INTERVALOS DE CONFIANZA DE LOS COEFICIENTES.##############

# Nota: para los modelos logísticos, los intervalos de confianza están basados en la función
# de log-likelihood.--> LOG DE MAXIMA VEROSIMILITUD
confint(myprobit)

################### WALD TEST DE LOS COEFICIENTES.###################

# Contrasta si los coeficientes son igual a 0 (Ho: coef.iguales a 0)
# Es decir si rank es significativo para un p-value bajo
wald.test(b=coef(myprobit), Sigma=vcov(myprobit), Terms=4:6)
# Chi-cuadrado de 21.4, con df=3 se asocia con un p-value 8.9e-05<<0.05
# lo que significa que el efecto de rank es estadísticamente significativa.


################# COEFICIENTES - ODDS RATIO.###################
## NO TIENE SENTIDO CON PROBIT ##
#exp(coef(mylogit))

# odds ratio para CI:
#exp(confint(mylogit))

##################### INFERENCIA (predict) ###################
# Al igual que con logit, con probit se pueden hacer estimaciones de las probabilidades.
# Y tal como se hacia con logit, se fijan valores y se usa predict

# Conjunto de combinaciones de valores
newdata <- data.frame(
  gre = rep(seq(from = 200, to = 800, length.out = 100), 4 * 4),
  gpa = rep(c(2.5, 3, 3.5, 4), each = 100 * 4),
  rank = factor(rep(rep(1:4, each = 100), 4)))

head(newdata)

# Predicciones se añaden a p y se, le quitamos la columna 3 (residual.scale)
newdata[, c("p", "se")] <- predict(myprobit, newdata, type = "response", se.fit=TRUE)[-3]
head(newdata)

# Dibujamos en un plot
ggplot(newdata, aes(x = gre, y = p, colour = rank)) +
  geom_line() +
  facet_wrap(~gpa)

####################SELECCION DE VARIABLES (STEPWISE)#################
# Utilizamos Stepwise del mismo modo


# modelo base (sin variables)
mod.ini<-glm(admit~1, data=mydata, family=binomial(link = "probit"))
summary(mod.ini)

# modelo con todas las variables
mod.full<-glm(admit~gre + gpa + rank, data=mydata, family=binomial(link = "probit"))
summary(mod.full)

# STEPWISE
mod.stp <- stepAIC(mod.ini, scope = list(lower = mod.ini,upper = mod.full), direction = "both")
# resumen del modelo con menor AIC
mod.stp$anova
# Modelo con las variables minimas
summary(mod.stp)



# BONDAD DE AJUSTE DE MI MODELO: 
# Exactamente igual que para logit.
with(mod.stp, pchisq(null.deviance-deviance,df.null-df.residual, lower.tail = FALSE))
# 7.218932e-08 --> MUY BUEN AJUSTE. Nos dice que nuestro modelo es significativamente mejor que el modelo nulo

# A veces se nos habla de la verosimilitud (likelihood) de un modelo frente a otro:
# Verosimititud de modelo nulo:
logLik(mod.ini)
# Verosimilitud de modelo ajustado:
logLik(mod.stp) # Cuanto mayor likelihood tenga mas ajustado es

######################### COMPARACION MODELOS ####################
# Puedo comparar 2 modelos: Con Chisq (Wald test) o LRT (likelihood)
myprobit <- glm (admit ~ gre + gpa+ rank, mydata, family=binomial(link = "probit"))
myprobit2 <- glm (admit ~ gre + rank, mydata, family=binomial(link = "probit"))
anova(myprobit,myprobit2, test="Chisq")
# Me indica que el segundo es mas exacto. De hecho me devuelve el p-value que me devuelve
# el summary de glm para la variable que estamos quitando gpa

# Tambien lo puedo hacer comparando el log verisimilitud: diferencia entre el modelo vacio y con la variable:
anova(myprobit,myprobit2, test="LRT")
# El mismo valor. 0.01408
# Y es que Wald test es una aproximacion a likelihood

######################### LINEALIDAD ####################
# Se puede ver con esta funcion:
crPlots(myprobit) # Te indica si la relacion es muy o poco lineal para cada VD
# En este caso el grafico para rank no interesa

