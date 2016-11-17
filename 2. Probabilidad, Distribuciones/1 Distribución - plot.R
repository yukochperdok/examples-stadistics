# Limpiar el workspace
rm(list = ls(all = TRUE))

########################################
############## CON CONJUNTO DATA DE R ##
########################################

data("trees")
head(trees)

summary(trees)

install.packages("modeest")
library("modeest")
summary(trees$Girth)
mfv(trees$Girth)

#generar muestra aleatoria normal con media y desviación de Girth
rGirth<-rnorm(31,mean=mean(trees$Girth),sd=sd(trees$Girth))
summary(rGirth)
mfv(round(rGirth))

plot(density(trees$Girth))
plot(density(rGirth))


library(ggplot2)
plotter = ggplot() + geom_density(aes(x=trees$Girth), colour="red", fill="green", alpha = .3, data=data.frame(trees$Girth))
plotter = plotter + geom_density(aes(x=rGirth), colour="purple", fill="purple", alpha = .3, data=data.frame(rGirth))
plotter = plotter + xlim(c(0,30))
print(plotter)



### cars

data("cars")
summary(cars)
mfv(cars$speed)
mfv(cars$dist)
plot(density(cars$speed))
plot(density(cars$dist))

rspeed<-rnorm(50,mean=mean(cars$speed),sd=sd(cars$speed))
summary(rspeed)
mfv(round(rspeed))

library(ggplot2)
plotter = ggplot() + geom_density(aes(x=cars$speed), colour="red", fill="green", alpha = .3, data=data.frame(cars$speed))
plotter = plotter + geom_density(aes(x=rspeed), colour="purple", fill="purple", alpha = .3, data=data.frame(rspeed))
plotter = plotter + xlim(c(-10,30))
print(plotter)

boxplot(cars$dist)
plot(density(cars$dist))

#buscar valor extremo, eliminar y comparar distribución
summary(cars$dist)
dist=cars$dist[cars$dist<120]
length(dist)
boxplot(dist)

plotter = ggplot() + geom_density(aes(x=cars$dist), colour="red", fill="green", alpha = .3, data=data.frame(cars$dist))
plotter = plotter + geom_density(aes(x=dist), colour="purple", fill="purple", alpha = .3, data=data.frame(dist))
plotter = plotter + xlim(c(-20,150))
print(plotter)
