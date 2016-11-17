# Limpiar el workspace
rm(list = ls(all = TRUE))
############DISTRIBUCIÓN BINOMIAL
#dbinom - calcula la probabilidad igual a un valor
#pbinom - calcula la probabilidad menor o igual a un valor (lower.tail=TRUE)
#pbinom - calcula la probabilidad mayor a un valor (lower.tail=FALSE)
#qbinom - calcula hasta el numero de exitos necesarios para llegar a una probabilidad (lower.tail=TRUE)

#La probabilidad de que un alumno apruebe matemáticas es de 0,3. 
#Calcular la probabilidad de que un grupo de siete alumnos aprueben:

#Ninguno  N=7 K=0 P=0.3
# P(X=0)
dbinom(0,7,0.3)

#Todos
# P(X=7)
dbinom(7,7,0.3)

#Al menos dos
# P(X>2)= 1-P(X=0)-P(X=1)
1-dbinom(1,7,0.3)-dbinom(0,7,0.3)
#o tambien 1-(P(X=0)+P(X=1))
1-(dbinom(1,7,0.3)+dbinom(0,7,0.3))

#Con pbinom: P(X>=2)=P(X>2)+P(X=2)
pbinom(2,7,0.3,lower.tail = FALSE)+dbinom(2,7,0.3)

#Con pbinom: P(X>=2)=(1-P(X<=2))+P(X=2)
(1-pbinom(2,7,0.3,lower.tail = TRUE))+dbinom(2,7,0.3)

#OBS: Aqui no valen negativos: P(X<2) distinto P(X>-2)=0


############DISTRIBUCIÓN POISSON
#dpois - calcula la probabilidad igual a un valor
#ppois - calcula la probabilidad menor o igual a un valor (lower.tail=TRUE)
#ppois - calcula la probabilidad mayor a un valor (lower.tail=FALSE)

#En una consulta médica se atienden 16 pacientes en 4 horas. Calcular la probabilidad de:

#En 30 minutos se atiendan menos de 3 personas  X=3 lambda=2
dpois(0,2)
dpois(1,2)
dpois(2,2)

# P(X<3)=P(X=0)+P(X=1)+P(X=2)
dpois(0,2)+dpois(1,2)+dpois(2,2)

# Con ppois P(X<3)=P(X<=3)-P(X=3)
ppois(3,2)-dpois(3,2)



#Atienda a 12 pacientes en 180 minutos. lambda=16/4*3=12
#P(X=12), lambda=12
dpois(12,12)



############DISTRIBUCIÓN NORMAL
#pnorm - calcula la probabilidad menor o igual a un valor (lower.tail=TRUE)
#pnorm - calcula la probabilidad mayor a un valor (lower.tail=FALSE)

#Se sabe que la longitud de las alas extendidas de un tipo de ave rapaz es
#una variable aleatoria que sigue una distribución Normal, de media 120 cm.
#y desviación típica 8 cm. Calcúlese la probabilidad de que la longitud de
#las alas de un ave elegida al azar sea:

#Mayor de 130 cm
# P(X>130) sin tipificar
pnorm(130,mean=120,sd=8,lower.tail=FALSE)
# P(Z>130-120/8) tipificada
pnorm(1.25,mean=0,sd=1,lower.tail=FALSE)

#Menor de 110 cm
# P(X<110)=1-P(X>=110) sin tipificar
1-pnorm(110,mean=120,sd=8,lower.tail=FALSE)
# Tipificada 
# P(Z<110-120/8)=1-P(Z>=110-120/8) tipificada
1-pnorm(-1.25,mean=0,sd=1,lower.tail=FALSE)


#Esté comprendido entre 110 y 130cm
#P(110<X<130)=P(X>130)-P(X>110) con colas hacia arriba
pnorm(110,mean=120,sd=8,lower.tail=FALSE)-pnorm(130,mean=120,sd=8,lower.tail=FALSE)
#P(110<X<130)=P(X<=130)-P(X<=110) con colas hacia abajo
pnorm(130,mean=120,sd=8,lower.tail=TRUE)-pnorm(110,mean=120,sd=8,lower.tail=TRUE)

# Tipificada 
# P(-1.25<Z<1.25)=1-2P(Z>1.25) tipificada
1-(2*pnorm(1.25,mean=0,sd=1,lower.tail=FALSE))

