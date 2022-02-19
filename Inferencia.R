"POR FAVOR ANTES DE EJECUTAR EL SCRIPT SELECCIONE EL WORKING DIRECTORY CON LA FUNCIÓN
setwd('directorio de la carpeta PEC')"

# gasto medio diario

install.packages("GGally")
library("GGally")
library("ggplot2")


cln.data <- read.csv(file= "Datos/clean_data.txt", sep=";", header = TRUE,)
head(cln.data)

# Eliminamos Outliers 

f.data <- filter(cln.data, cln.data$A13 < 50)

  # Outliers representan menos del 1%
  nrow(filter(data, cln.data$gastomedio < 900)) / nrow(cln.data)


f.data <- filter(f.data, f.data$gastomedio < 900)

attach(f.data)



"5.1 Relación positiva entre A13 y Gasto total"

ggplot(f.data, aes(x=gastototal, y=A13)) + geom_point() + geom_smooth() +
  geom_smooth(method=lm, colour="red")

  "Comparación de datos sin filtrar y filtrados"
  summary(f.data$gastomedio)
  summary(cln.data$gastomedio)
  
  sd(f.data$gastomedio)
  sd(cln.data$gastomedio)
  
  "Menos del 3% de los datos eran outliers"
  nrow(f.data) / nrow(cln.data)


"5.2 Gasto medio mayor que 200 sin outliers y con outliers"
attach(f.data)

"Contraste de medias a ver si es menor que 200, se rechaza H0"
t.test(gastomedio, mu= 200, stdev= sd(gastomedio), conf.level= 0.95, alternative= "less")

attach(cln.data)

"Contraste de medias a ver si es menor que 200, se rechaza H0, sin outliers"
t.test(gastomedio, mu= 200, stdev= sd(gastomedio), conf.level= 0.95, alternative= "less")

"5.3 Turistas britanicos mejores que alemanes"

attach(f.data)
f.data.i.a <- subset(f.data, pais == 1 | pais == 8)

attach(f.data.i.a)
box.GmedioPais <- ggplot(f.data.i.a, aes(x=gastomedio))
box.GmedioPais + geom_boxplot() + coord_flip() + facet_wrap(~pais, scales="free")

"Calculamos las medias y desviaciones típicas"

aggregate(gastomedio, by=list(f.data.i.a$pais), mean)

aggregate(gastomedio, by=list(f.data.i.a$pais), sd)

"Contraste de desviaciones para ver si son iguales, NO se rechaza H0"
var.test(f.data.i.a$gastomedio~f.data.i.a$pais, alternative="two.sided", conf.level=0.95)

var.test(f.data.i.a$gastomedio~f.data.i.a$pais, alternative="two.sided", conf.level=0.99)

"Contraste de medias para ver si son iguales, se rechaza H0, Alemanes es mayor que britanicos"
t.test(f.data.i.a$gastomedio~f.data.i.a$pais, alternative="two.sided", conf.level=0.95, var.equal = TRUE)

t.test(f.data.i.a$gastomedio~f.data.i.a$pais, alternative="t", conf.level=0.95, var.equal = TRUE, mu=3)


"5.4 Gasto medio en agosto es mayor a la media de diciembre"
f.data.agosto <- subset(f.data, mes == 8)
f.data.diciembre <- subset(f.data, mes == 12)

mean(f.data.agosto$gastomedio)
mean(f.data.diciembre$gastomedio)

aggregate(cln.data$gastototal, by=list(cln.data$mes), mean) 
aggregate(cln.data$gastomedio, by=list(cln.data$mes), mean)

aggregate(f.data$A13, by=list(f.data$mes), mean)

"Contraste de desviaciones para ver si son iguales, se rechaza H0"
var.test(f.data.agosto$gastomedio, f.data.diciembre$gastomedio, alternative="two.sided", conf.level=0.95)

"Contraste de medias para ver si son iguales, se rechaza H0, agosto es menor que la media de diciembre"
t.test(f.data.agosto$gastomedio, f.data.diciembre$gastomedio, alternative="two.sided", conf.level=0.95)



"Que pasa con el gasto total?"
t.test(f.data.agosto$gastototal, f.data.diciembre$gastototal, alternative="two.sided", conf.level=0.95)
"Se rechaza H0"


"5.5 Gasto medio es mayor en Comunidad de Madrid que Cataluña"
f.data.m.c <- subset(f.data, ccaa == 9 | ccaa == 13)

"Contraste de desviaciones para ver si son iguales, se rechaza H0"
var.test(f.data.m.c$gastomedio~f.data.m.c$ccaa, alternative="two.sided", conf.level=0.95)

"Contraste de medias para ver si son iguales, se rechaza H0, Gasto medio en Comunidad de Madrid es mayor"
t.test(f.data.m.c$gastomedio~f.data.m.c$ccaa, alternative="less", conf.level=0.95, var.equal = FALSE)


"5. Gasto Total es mayor en Baleares que en Comunitat Valenciana"
f.data.b.v <- subset(f.data, ccaa == 4 | ccaa == 10)

aggregate(f.data.b.v$gastototal, by=list(f.data.b.v$ccaa), mean)
aggregate(f.data.b.v$gastototal, by=list(f.data.b.v$ccaa), sd)

"Contraste de desviaciones para ver si son iguales, se rechaza H0"
var.test(f.data.b.v$gastototal~f.data.b.v$ccaa, alternative="two.sided", conf.level=0.95)

"Contraste de medias para ver si son iguales, se rechaza H0, Gasto total mayor en Baleares que es Cumunitat Valenciana"
t.test(f.data.b.v$gastototal~f.data.b.v$ccaa, alternative="greater", conf.level=0.95, var.equal = FALSE)

# Representacion grafica

f.data.str <- f.data
f.data.str$mes <- as.character(f.data.str$mes)
f.data.str$A1 <- as.character(f.data.str$A1)
f.data.str$pais <- as.character(f.data.str$pais)
f.data.str$ccaa <- as.character(f.data.str$ccaa)
f.data.str$aloja <- as.character(f.data.str$aloja)
f.data.str$motivo <- as.character(f.data.str$motivo)
f.data.str$A16 <- as.character(f.data.str$A16)

ggpairs(subset(f.data.str, pais == 1 | pais == 8), columns=c(5,9,10), mapping=aes(colour=pais))

ggpairs(subset(f.data.str, (pais == 1 | pais == 8 ) & gastomedio < 600 & (A13<11 & A13>2)), columns=c(3,5,9,10), mapping=aes(colour=pais))


ggpairs(subset(f.data.str, (mes == 8 | mes == 12) & gastomedio < 600 & (A13<11 & A13>2)), columns=c(1,5,9,10), mapping=aes(colour=mes))



ggpairs(subset(f.data.str, (ccaa == 9 | ccaa == 13) & gastomedio < 600 & (A13<11 & A13>2)), columns=c(5,9,10), mapping=aes(colour=ccaa))

ggpairs(subset(f.data.str, (ccaa == 9 | ccaa == 13) & gastomedio < 600 & (A13<11 & A13>2)), columns=c(4,5,9,10), mapping=aes(colour=ccaa))


ggpairs(subset(f.data.str, (ccaa == 4 | ccaa == 10) & gastomedio < 600 & (A13<11 & A13>2)), columns=c(4,5,9,10), mapping=aes(colour=ccaa))
