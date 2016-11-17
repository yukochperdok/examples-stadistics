# Limpiar el workspace
rm(list = ls(all = TRUE))
### TABLAS DE CONTINGENCIA ####

## Independencia
exito<-c(rep(1,162),rep(1,263),rep(2,38),rep(2,37))
speso<-c(rep(1,162),rep(2,263),rep(1,38),rep(2,37))

table(exito,speso)

#H0:exito y peso son independientes
chisq.test(exito,speso, simulate.p.value=TRUE)
qchisq(0.05,1, lower.tail = FALSE) ##cálculo de chi con 95% confianza, alfa 5% y df=(2-1)(2-1)=1
# Tenemos un p-value muy ajustado pero superior a 0.05 = Se rechaza H0
# Confirmamos con qchisq y vemos que nuestro estadistico (4.183) esta por encima del v.teorico(3.84)
# Entonces entramos en zona de rechazo => Las variables dependen entre si con un CI al 95%


## Independencia
cancer<-c(rep(1,40),rep(1,10),rep(2,20),rep(2,30))
fuma<-c(rep(1,40),rep(2,10),rep(1,20),rep(2,30))

table(cancer,fuma)

#H0:cancer y fuma son independientes
chisq.test(cancer,fuma, simulate.p.value=TRUE)
qchisq(0.95,1) ##cálculo de chi con 95% confianza, alfa 5% y y df=(2-1)(2-1)=1
# Valor bajo de p-value=>Estadistico en region de rechazo=> Rechazamos H0=>Fumar depende en el cancer



## Homogeneidad
radio<-c(rep(1,52),rep(1,248),rep(2,48),rep(2,272))
enferma<-c(rep(1,52),rep(2,248),rep(1,48),rep(2,272))

#H0:Estar enfermo NO se distribuye de forma homogenea con la radiactividad
chisq.test(radio, enferma, simulate.p.value=TRUE)
qchisq(0.95,1) ##cálculo de chi con 95% confianza, alfa 5% y df=(2-1)(2-1)=1
# Valor bajo de p-value=>Estadistico en region de rechazo=> Rechazamos H0=>
# => Estar enfermo es homogeneo a la radiactividad


###########################
# La tabla siguiente clasifica a un grupo de personas según su opinión sobre un
# documental televisivo y el nivel de estudios
# Existe dependencia entre la opinión del programa y el nivel de estudios?

nivel<-c(rep("bajo",41),rep("medio",180),rep("alto",37))
opina<-c(rep("malo",1),rep("regular",10),rep("bueno",30),
         rep("malo",40),rep("regular",80),rep("bueno",60),
         rep("malo",25),rep("regular",12))


tabla<-table(nivel,opina)

#H0:La opinion del programa depende del nivel de estudios
chisq.test(tabla)
# p-value menor de 0.05, se rechaza la hipótesis de independencia(H0)
# la opinión del programa depende del nivel de estudios del entrevistado

# OBS: Se podia haber hecho:
chisq.test(nivel,opina)
# Y sale lo mismo


