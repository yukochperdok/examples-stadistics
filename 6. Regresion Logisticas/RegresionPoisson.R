# Limpiar el workspace
rm(list = ls(all = TRUE))

# Cargar librarias
require(aod)
require(ggplot2)
require(msm)

########################FORMULACION PROBLEMA##################
# Intentar predecir el valor del numero de premios (num_awards) en funcion
# de las variables prog y math

# La variable respuesta, numero de premios es una variable discreta, que se refiere
# como a un contador de eventos durante un tiempo.

# Una regresion de Poisson te puede servir para predecir una cuenta de eventos que suceden
# en un tramo de tiempo con respecto a unas variables(independientes).
# Son


mydata <- read.csv("http://www.ats.ucla.edu/stat/data/poisson_sim.csv")
## Primeras filas
head(mydata)


########################EXPLORACION VARIABLES#########################
# distribución de las variables
summary(mydata)
# Vemos claramente que id es un identificador de fila.

# math es una variable discreta cuyos valores estan muy agrupados en el medio (45-59)
str(mydata$math)

# prog parece un factor de 1 a 3.
str(mydata$prog)

# num_awards es un contador claramente de 0 a 6
str(mydata$num_awards)

# Deberiamos explorar algo mas las variables
# Factorizamos prog etiquetandolo con los valores ("General"=1, "Academic"=2, "Vocational"=3) 
# e id que realmente le colocamos levels 1 al 200.
mydata <- within(mydata, {
  prog <- factor(prog, levels=1:3, labels=c("General", "Academic", "Vocational"))
  id <- factor(id)
})
head(mydata)
summary(mydata)
print(mydata$prog)
print(mydata$id)

# ¿Sigue num awards una distribucion de poisson?
mean(mydata$num_awards) # 0.63
var(mydata$num_awards) # 1.108643
# Similares

# Y agrupados por programa???
with(mydata, tapply(num_awards, prog, function(x) {
  sprintf("M (SD) = %1.2f (%1.2f)", mean(x), var(x))
}))
# Estadisticamente iguales

boxplot(mydata$num_awards~mydata$prog)
# Vemos que sobre todo tenemos muchos premios para el tipo Academico, que de hecho tiene la
# mayor media del numero de premios.

ggplot(mydata, aes(num_awards, fill = prog)) +
  geom_histogram(binwidth=.5, position="dodge")
# Una vez mas nos damos cuenta que la mayoria de los datos son de tipo academico.
# El cual tiene la mayoria de 0,1,2 premios. Entre general y vocacional tienen mas omenos 
# los mismos.
# Para numero de premios 4,5 y 6 lo tiene todos el tipo Academico.


########################MODELO POISSON#########################
# Si nosotros tuvieramos una sobre dispersion en la varible predicha
# es decir si Residual deviance > degrees of freedom
#, es mas logico usar:
#  1. Quasipoisson
#        summary(m1 <- glm(num_awards ~ prog + math, family="quasipoisson", data=mydata))
#  2. Una Regression negativa binomial
#        summary(m1 <- glm.nb(num_awards ~ math + prog, data = mydata))

# En nuestro caso no tenemos una sobre dispersion de las variables, por lo tanto usamos la 
# familia poisson
summary(m1 <- glm(num_awards ~ prog + math, family="poisson", data=mydata))

#                 Estimate Std. Error z value Pr(>|z|)    
# (Intercept)    -5.24712   0.65845  -7.969   1.60e-15 ***
# progAcademic    1.08386   0.35825   3.025   0.00248 ** 
# progVocational  0.36981   0.44107   0.838   0.40179    
# math            0.07015   0.01060   6.619   3.63e-11 ***

# Tenemos como significativos: 
#         progAcademic con referencia al General
#         math
#         el intercepto: b0
# OBS: Si no se adaptara bien el modelo nos lo daria como salida el glm, en este caso
# la desviacion residual es bastante menor que la del modelo nulo y el AIC es bastante bajo.
# Parece un modelo bien ajustado, aseguramos con una chisq sobre la desviacion del modelo:

with(m1, cbind(res.deviance = deviance, df = df.residual,
               p = pchisq(deviance, df.residual, lower.tail=FALSE)))

#     res.deviance  df         p
#[1,]     189.4496 196 0.6182274
# Tenemos un p-value lo suficientemente alto para rechazar H0 y decir que el modelo esta bien ajustado


########################COMPARAR MODELOS#########################
# Comparamos el modelo actual con otro m2 quitandole prog
m2 <- update(m1, . ~ . - prog)
## Test por Chisq, aunque se podria hacer por LTR
anova(m1, m2, test="Chisq")

# La comparacion te dice que quitar prog no es beneficioso.
# Te dice que prog es significativo 0.0006852 ***

# Y ademas si nos fijamos para el modelo 2:num_awards ~ math
# Tenemos sobre dispersion puesto que la desviacion de los errores es mayor que los df: 204>198.
# En caso de que fuera mejor este modelo habria que trabajar con otro tipo de modelo:
# Quasipoisson o negative binomial regression.

#######################CALCULO RATIOS#################
# Si queremos calcular los ratios completos en vez de sus coeficientes para
# ver mejor como varia la VD:
r.estim <- cbind(Estimate = exp(coef(m1)), exp(confint(m1)))
r.estim

# Conclusiones: 
# Por cada unidad de incremento de math, num_awards incrementa un 7%.
# Para un prog Academico tenemos un aumento de un 195% de incremento sobre num_awards
# con respecto a un prog General.
# Vocacional no fue significativa

######################PREDICCION######################
# Fijamos la media de math y probamos variando prog
(s1 <- data.frame(math = mean(mydata$math),
                  prog = factor(1:3, levels = 1:3, labels = levels(mydata$prog))))

predict(m1, s1, type="response", se.fit=TRUE)

#$fit
# 1         2         3 
# 0.2114109 0.6249446 0.3060086 

# Queda claro que fijando la media de math tenemos que Academico llega a un p=0.62
# frente a general que seria de un p=0.21
# Su IRR=0.62/0.21=2.96


# Si predecimos todos los valores los podemos mostrar en una grafica:
mydata$phat <- predict(m1, type="response")

# Ordenamos por programa y luego por math
mydata <- mydata[with(mydata, order(prog, math)), ]

# Creamos un plor
ggplot(mydata, aes(x = math, y = phat, colour = prog)) +
  geom_point(aes(y = num_awards), alpha=.5, position=position_jitter(h=.2)) +
  geom_line(size = 1) +
  labs(x = "Math Score", y = "Expected number of awards")

# CONCLUSION: En el grafico se ve claramente que los mas premiados son aquellos que tienen
# sus programas academicos, y cuanto mas math mas premios


#######################CALCULAR RR (relativo riesgo)################
# Una de las medidas mas empleadas para caracterizaruna distribucion poisson (y en general glm)
# es el relativo riesgo (RR).
# En resumen el RR es la relacion entre la probabilidad de que ocurra un evento a 
# un individuo que este en un grupo espuesto y la de que le ocurra a un individuo que no
# este en un grupo expuesto.

#       RISK  Presente  Ausente           RR=p-evento(cuando expuesto)/ p-evento(cuando NO expuesto)
#    Fumador    a         b
# No fumador    c         d               RR= a/(a+b)/c/(c+d)

## Estimacion coeficiente de math
(coefMath <- coef(m1)["math"]) #0.0701524

## Funcion para estimar el punto de estimado de logRR
estFunc <- function(math) {
  coefMath * math
}
## Varianza del estimado de math
(varCoefMath <- vcov(m1)["math","math"]) # 0.0001123431

## Funcion para estimar la varianza del logRR estimado para cada nivel de math
varFunc <- function(math) {
  varCoefMath * (math^2)
}

## Secuencia de datos para math desde -10 a +10
newdat <- data.frame(delta_math = seq(from = -10, to = +10, by = 0.1))
## Calculo del logRR para dicha secuencia
newdat$logRR <- estFunc(newdat$delta_math)
## Varianza del logRR
newdat$varLogRR <- varFunc(newdat$delta_math)
## Calculo de los CI (95%) para logRR
newdat$lower <- newdat$logRR - qnorm(0.975) * sqrt(newdat$varLogRR)
newdat$upper <- newdat$logRR + qnorm(0.975) * sqrt(newdat$varLogRR)

head(newdat)

# Dibujamos la muestra de math con respecto al riesgo
ggplot(data = newdat, aes(x = delta_math, y = logRR)) +
  geom_line() +
  geom_ribbon(aes(ymin = lower, ymax = upper), color="gray", alpha = .3) +
  theme_bw() + theme(legend.key = element_blank())
# Dibujamos la muestra de math con respecto al exp(riesgo)
ggplot(data = newdat, aes(x = delta_math, y = exp(logRR))) +
  geom_line() +
  geom_ribbon(aes(ymin = exp(lower), ymax = exp(upper)), color="gray", alpha = .3) +
  theme_bw() + theme(legend.key = element_blank())

#El RR aumenta a medida que aumenta math.
# En 0 la confianza es maxima,los intervalos se ajustan a la recta

##########################################################################
#########################NEGATIVE REGRESSION LOGISTIC#####################
##########################################################################
# Cuando tenemos sobre dispersion en la variable de salida, como hemos comentado antes.
# Es decir la desviacion residual del modelo supera los grados de libertad, hemos de escoger 
# este tipo de regresion para explicar una variable de cuenta. NO POISSON (Tal vez quiasipoisson)
require(foreign)
require(ggplot2)
require(MASS)


# Predecir el numero de dias de ausencia (daysabs) en funcion de puntuacion estandariazada de
# matematicas (math), y el programa de la universidad (prog)

dat <- read.dta("http://www.ats.ucla.edu/stat/stata/dae/nb_data.dta")
dat <- within(dat, {
  prog <- factor(prog, levels = 1:3, labels = c("General", "Academic", "Vocational"))
  id <- factor(id)
})

summary(dat)

ggplot(dat, aes(daysabs, fill = prog)) +
  geom_histogram(binwidth=1) +
  facet_grid(prog ~ ., margins=TRUE, scales="free")
# Tenemos los dias de ausencia muy dispersados


# Si revisamos la media y la varianza de nuestra variable de salida:
with(dat, tapply(daysabs, prog, function(x) {
  sprintf("M (VAR) = %1.2f (%1.2f)", mean(x), var(x))
}))
# Con lo cual podriamos pensar que no sigue una distribucion de Poisson puesto que no cumple
# que su media y varainza son iguales, y es que el problema es que tiene una sobre dispersion de los datos
# pero sigue siendo una cuenta => Optamos por una regresion binomial negativa.

#Modelamos glm.nb (package MASS):
summary(m1 <- glm.nb(daysabs ~ math + prog, data = dat))

# Categoria de referencia:
contrasts(dat$prog) #-->General es nuestra categoria de referencia


# 1. Desviacion de los residuos:
#Deviance Residuals: 
#  Min       1Q   Median       3Q      Max  
#-2.1547  -1.0192  -0.3694   0.2285   2.5273
# Los errores se encuentran entre -2 y 2

# Los coeficientes
# Coefficients:
#                  Estimate Std. Error z value Pr(>|z|)    
# (Intercept)      2.615265   0.197460  13.245  < 2e-16 ***
#  math           -0.005993   0.002505  -2.392   0.0167 *  
#  progAcademic   -0.440760   0.182610  -2.414   0.0158 *  
#  progVocational -1.278651   0.200720  -6.370 1.89e-10 ***
# Nos dan los beta, con sus errores estandard, mas si son significativos
# Son todos significativos:
# En el caso de math por cada unidad que incrementa, se decrementa el cociente de razones en 0.006.
# El coeficiente de progAcademic muestra la diferencia en el cociente de razones entre el nivel academico
# y el nivel general --> El nivel academico es 0.44 menor que el nivel general.
# El coeficiente de progVocacional muestra la diferencia en el cociente de razones entre el nivel vocacional
# y el nivel general --> El nivel academico es 1.27 menor que el nivel general.

# Tambien puedes ver estadisticos muy interesantes del modelo:
# Null deviance: 427.54  on 313  degrees of freedom
# Residual deviance: 358.52  on 310  degrees of freedom
# AIC: 1741.3
# 2 x log-likelihood:  -1731.258

# Esto nos sirve para comparar modelos.
# Para ver el impacto de prog en el modelo lo comparamos con el modelo sin prog:
m2 <- update(m1, . ~ . - prog)
anova(m1, m2)
# Esto te compara a traves del test de likelihood para Modelos binomiales negativos
# Si nos fijamos el modelo con prog tiene mas lio-lik y tiene un p-value muy bajo
# rechazando H0 => el coeficiente de prog no es 0
# con lo cual el modelo con prog es mejor modelo.

# Un modelo binomial negativo asume que las medias (condiacionada por las correspondientes variables) 
# no son iguales que sus correspondientes varianzas. De hecho en un modelo de Poisson puro el parametro de 
# dispersion se mantiene constante, en un modelo binomial negativo no. De hecho un modelo binomial negativo
# es un modelo de poisson con variaciones. 
# Der hecho tu puedes comparar un modelo Poisson con un binomial negativo y ver cual se ajusta mejor.
m3 <- glm(daysabs ~ math + prog, family = "poisson", data = dat)
X2 <- 2 * (logLik(m1) - logLik(m3))
X2
# Encontramos una diferencia de log lik de 926 - Para df=5. Esto es una diferencia muy grande a favor
# del m1, con lo cual se puede decir que el modelo glm.nb es el que mejor se ajusta, por la dispersion.

# Si hacemos un test chisq con la diferncia de modelos con respecto a la diferencia de df=5-4=1
pchisq(X2, df = 1, lower.tail=FALSE)
# Nos da una chisq muy baja, que nos hace pensar que el modelo m1 es mucho mas ajustado a los datos.

# Se puede hacer tambien calcular los CI:
(est <- cbind(Estimate = coef(m1), confint(m1)))
# Y calcular los odds ratio:
exp(est)
# Aqui ya podemos interpretar los porcentajes:
# Los estimados nos dicen que la probabilidad ausencia es 0.64 veces en academico que en general. 
# Osea para general la ausencia aumenta con respecto a academico.

# La probabilidad de ausencia es 0.28 veces mas en vocacional que en general.
# Osea para general la ausencia aumenta con respecto a vocacional.

# Por ultimo por cada unidad de incremento en math hay un 1% de probabilidad menos en la ausencia (100-99).

# MODELO:
# ln(daysabs) = Intercept + b1(prog=2) + b2(prog=3) + math, o dicho de otra manera:
# daysabs = exp(Intercept + b1(prog=2) + b2(prog=3) + math) 
#         = exp(Intercept) * exp(b1(prog=2)) * exp(b2(prog=3)) * exp(math)
# Exactamente igual que poisson normal. Es decir para predecir es exactamente igual.
# Pero en funcion de la dispersion de los datos es conveniente ajustar con negative binomial, para calcular los coeficientes.

# Predicciones:
# 1. Fijamos la media de math y probamos con los 3 niveles de prog
newdata1 <- data.frame(math = mean(dat$math),
                       prog = factor(1:3, levels = 1:3, labels = levels(dat$prog)))
newdata1$phat <- predict(m1, newdata1, type = "response")
newdata1
# Mayor probabilidad en general que en academic y a su vez que vocacional.

# 2. Variamos ambas variables: math y prog
newdata2 <- data.frame(
  math = rep(seq(from = min(dat$math), to = max(dat$math), length.out = 100), 3),
  prog = factor(rep(1:3, each = 100), levels = 1:3, labels =
                  levels(dat$prog)))

newdata2 <- cbind(newdata2, predict(m1, newdata2, type = "link", se.fit=TRUE))
z<-qnorm(0.975,0,1,lower.tail = TRUE)

newdata2 <- within(newdata2, {
  DaysAbsent <- exp(fit)
  LL <- exp(fit - z * se.fit)
  UL <- exp(fit + z * se.fit)
})

# Y mostramos un plot explicativo, con intervalos de confianza
ggplot(newdata2, aes(math, DaysAbsent)) +
  geom_ribbon(aes(ymin = LL, ymax = UL, fill = prog), alpha = .25) +
  geom_line(aes(colour = prog), size = 2) +
  labs(x = "Math Score", y = "Predicted Days Absent")

# CONCLUSION:
# Aqui se puede ver claramente como a medida que aumenta la nota en matematicas la ausencia deiminuye (muy lentamente)
# Y ademas en los 3 niveles.
# Por otro lado se puede ver que la probabilidad no es la misma en los 3 niveles.


# COSAS A TENER EN CUENTA:
# 1. No se recomienda usar este tipo de regresion con pocos datos.
# 2. Una de las razones mas habituales de sobre dispersion es el exceso de ceros, si es asi es mas 
# apropiado usar una zero-truncated model.
# 3. Cuando se tiene una variable que te dice el numero de veces que el evento de ausencia puede haber ocurrido
# es necesario incorporarla almodelo utilizando el parametro offset.
# 4. Como Poisson model la variable de salida no puede ser negativa.
