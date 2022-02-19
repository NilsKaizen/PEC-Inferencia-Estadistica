"POR FAVOR ANTES DE EJECUTAR EL SCRIPT SELECCIONE EL WORKING DIRECTORY CON LA FUNCI�N
setwd('directorio de la carpeta PEC')"

dir <- "Datos/clean_eg_2019.txt"
library(tidyverse)
library(corrplot)
library(gridExtra)
library(grid)

data <- read.csv(dir, sep=";")

head(data)

# constantes
paises <- c("Alemania", "Bélgica","Francia", "Irlanda", "Italia", "Paises Bajos",
            "Portugal", "Reino Unido", "Suiza", "Rusia", "Paises Nordicos",
            "Resto de Europa", "EEUU", "Resto de América", "Resto del mundo")

ccaa <- c("Andalucia", "Aragon", "Principado de Asturias", "Illes Balears", "Canarias",
          "Cantabria", "Castilla y Leon", "Castilla-La Mancha", "Cataluña", 
          "Comunitat Valenciana", "Extremadura", "Galicia", "Comunidad de Madrid", 
          "Region de Murcia", "Comunidad Foral de Navarra", "Pais Vasco", "La Rioja", 
          "Ceuta", "Melilla")

meses <- c("Enero", "Febrero", "Marzo", "Abril", "Mayo", "Junio", "Julio", "Agosto", 
           "Septiembre", "Octubre", "Noviembre", "Diciembre")

# mes
bar.mes <- ggplot(data=data, aes(x=mes, ))
bar.mes + geom_bar(fill = rgb(0.2,0.4,0.6,0.6)) +
  scale_x_continuous(breaks= seq(1:12)) + 
  labs(title= "Afluencia de turistas dividido por meses", y = "Total Turistas", x = "Mes")

    # 1.1 matrix pais vs mes
    pais.mes <- matrix(nrow = 15, ncol=12)
    
    i <- 0
    j <- 0
    
    for(i in seq(1:12)){
      for(j in seq(1:15)){
        temp <- nrow(filter(data, data$pais == j, mes == i))
        
        pais.mes[j, i] <- temp
        
      }
    }
    # 1.2 Dividimos cada pais por su valor maximo
    
    for (i in seq(1:15)){
     
     pais.mes[i,] <- pais.mes[i,] / max(pais.mes[i,])
      
    }
   
    pais.mes 
    rownames(pais.mes) <- paises
    colnames(pais.mes) <- meses
    
    
    
    corrplot(pais.mes,
             method = "pie", is.corr=FALSE, tl.cex=0.65)
    
    grid.table(table(data$pais, data$mes), rows=paises, cols=meses)
    
    # 1.3 matrix ccaa vs mes
    ccaa.mes <- matrix(nrow = 19, ncol=12)
    
    i <- 0
    j <- 0
    
    for(i in seq(1:12)){
      for(j in seq(1:19)){
        temp <- nrow(filter(data, data$ccaa == j, mes == i))
        
        ccaa.mes[j, i] <- temp
        
      }
    }
    
    # 1.4 Dividimos cada ccaa por su valor maximo
    for (i in seq(1:19)){
      
      ccaa.mes[i,] <- ccaa.mes[i,] / max(ccaa.mes[i,])
      
    }
    
    ccaa.mes 
    rownames(ccaa.mes) <- ccaa
    colnames(ccaa.mes) <- meses
    
    
    
    corrplot(ccaa.mes,
             method = "pie", is.corr=FALSE, tl.cex=0.65)
    
grid.table(table(data$ccaa, data$mes), rows=ccaa, cols=meses)


# A0_7 

bar.A0_7 <- ggplot(data = data, aes(x= A0_7)) 
bar.A0_7 + geom_bar(aes(fill= as.factor(A1))) + theme_classic() + 
  scale_x_continuous(labels = c("No Transito", "Transito"), breaks = c(2, 8)) +
  scale_fill_discrete(name= "Via Salida", labels = c("Carretera", "Aeropuerto", "Puerto", "Tren"),
                      limits= c(1,2,3, 4)) +
  labs(x = "En Transito", y = "Cantidad", title= "Turistas en transito segun via de Salida")

# A1 

pie.A1 <- ggplot(data = data, aes(x="", fill=as.factor(A1)))
pie.A1 + geom_bar(aes(y = (..count..)/sum(..count..))) + theme_light() + 
  scale_y_continuous(labels=scales::percent)+
  scale_fill_discrete(name= "Via Salida", labels = c("Carretera", "Aeropuerto", "Puerto", "Tren"),
                      limits= c(1,2,3, 4)) +
  coord_polar("y", start=0) + labs(x = "", y = "", title= "Porcentaje Turistas Segun Via de Salida")
 

# pais



hist.pais <- ggplot(data= data, aes(x=pais, fill= as.factor(aloja)))
hist.pais + geom_histogram(binwidth = 1) + 
  scale_x_continuous(name= "Pais", labels = paises, breaks= seq(1:15)) + 
  scale_fill_discrete(name= "Alojamiento", labels = c("Hotel y similar", "Resto de Mercado", "Alojamiento No mercado"),
                      limits= c(1,2,3)) +
  theme(axis.text.x = element_text(angle= 75, hjust=1)) +
  labs(y = "", title= "Turistas segun pais de residencia habitual / Alojamiento")

  # suma de turistas provenientes de Alemania, Francia y Reino Unido
  sum(data$pais == 1 | data$pais == 3 | data$pais == 8)
  

hist.pais <- ggplot(data= data, aes(x=pais, fill= as.factor(A1)))
hist.pais + geom_histogram(binwidth = 1) + 
  scale_x_continuous(name= "Pais", labels = paises, breaks= seq(1:15)) + 
  scale_fill_discrete(name= "Via de Salida", labels = c("Carretera", "Aeropuerto", "Puerto", "Tren"),
                      limits= c(1,2,3, 4)) +
  theme(axis.text.x = element_text(angle= 75, hjust=1)) +
  labs(y = "", title= "Turistas segun pais de residencia habitual / Via de Salida")

# ccaa

bar.ccaa <- ggplot(data=data, aes(x= ccaa, fill=as.factor(A1)))
bar.ccaa + geom_bar() +
  scale_x_continuous(name= "CCAA", labels = ccaa, breaks= seq(1:19)) +
  theme(axis.text.x = element_text(angle= 75, hjust=1)) +
  scale_fill_discrete(name= "Via de Salida", labels = c("Carretera", "Aeropuerto", "Puerto", "Tren"),
                      limits= c(1,2,3, 4)) +
  labs(y = "", title= "Turistas segun CCAA de destino / Via de Salida") 


pais.ccaa <- matrix(nrow = 15, ncol=19)

i <- 0
j <- 0

for(i in seq(1:19)){
  for(j in seq(1:15)){
     temp <- nrow(filter(data, data$ccaa == i, pais == j))
     
     pais.ccaa[j, i] <- temp
     
  }
}
  # Valor maximo de la matrix pais vs ccaa
  for (i in seq(1:15)){
    
    pais.ccaa[i,] <- pais.ccaa[i,] / max(pais.ccaa[i,])
    
  }
  
  pais.ccaa 
  rownames(pais.ccaa) <- paises
  colnames(pais.ccaa) <- ccaa



corrplot(pais.ccaa,
         method = "pie", is.corr=FALSE, tl.cex=0.65)

grid.table(table(data$ccaa, data$pais), rows= ccaa, cols=paises)

# A13

box.A13 <- ggplot(data = filter(data, data$A13 < 50), aes(x=A13))
box.A13 + geom_boxplot(shape="circle") + 
  scale_x_continuous(breaks=seq(0,30)) +
  labs(x="Numero de pernoctaciones", title="Distribucion de la Variable numero de pernoctaciones") 
  #facet_grid(rows=vars(pais))


nrow(filter(data, data$A13 < 50)) / 87055

hist.A13Mes <- ggplot(data=data, aes(x=mes, y= sum(A13)))
hist.A13Mes + geom_bar()

aggregate(data$A13, by=list(data$mes), mean)


# aloja

pie.aloja <- ggplot(data = data, aes(x="", fill=as.factor(aloja)))
pie.aloja + geom_bar(aes(y = (..count..)/sum(..count..))) + theme_light() + 
  scale_y_continuous(labels=scales::percent)+
  scale_fill_discrete(name= "Alojamiento", labels = c("Hotel y Similar", "Resto de Mercado", "Alojamiento No Mercado"),
                      limits= c(1,2,3)) +
  coord_polar("y", start=0) + labs(x = "", y = "", title= "Porcentaje Turistas Segun Alojamiento")

# motivo

pie.motivo <- ggplot(data = data, aes(x="", fill=as.factor(motivo)))
pie.motivo + geom_bar(aes(y = (..count..)/sum(..count..))) + theme_light() + 
  scale_y_continuous(labels=scales::percent)+
  scale_fill_discrete(name= "Motivo", labels = c("Ocio/Vacaciones", "Negocios/Trabajo", "Resto"),
                      limits= c(1,2,3)) +
  coord_polar("y", start=0) + labs(x = "", y = "", title= "Porcentaje Turistas Segun Motivo")

# A16

bar.A16 <- ggplot(data=data, aes(x=(A16)))
bar.A16 + geom_bar() + labs(y="", x="Paquete Turistico: Si / No", title="Seleccion de paquete Turistico por Paises") +
  theme_bw()+
  facet_grid(cols=vars(pais))

# gastototal
col.GtotalPais <- ggplot(data=data, aes(x=pais, y=gastototal)) 
col.GtotalPais + geom_col(fill = rgb(0.2,0.4,0.6,0.6)) +
  scale_x_continuous(name= "Pais", labels = paises, breaks= seq(1:15)) +
  theme(axis.text.x = element_text(angle= 75, hjust=1),) +
  labs(y="Gasto total", title="Gasto total dividido por paises")

col.GtotalCCAA <- ggplot(data=data, aes(x=ccaa, y=gastototal)) 
col.GtotalCCAA + geom_col(fill = rgb(0.2,0.4,0.6,0.6)) +
  scale_x_continuous(name= "CCAA", labels = ccaa, breaks= seq(1:19)) +
  theme(axis.text.x = element_text(angle= 75, hjust=1)) +
  labs(y="Gasto total", title="Gasto total dividido por CCAA")

  # Porcentaje de turistas provenientes Resto del mundo y Resto de América
  sum(nrow(filter(data, data$pais == 14 | data$pais == 15))) * 100 / sum(nrow(data))
  
  # Porcentaje de gasto turistas Resto del mundo y resto de América
  sum(filter(data, data$pais == 14 | data$pais == 15)$gastototal) * 100 / sum(data$gastototal)
  

summary(data$gastototal)

# gasto medio diario
data['gastomedio'] <- data['gastototal'] / data['A13']

summary(data$gastomedio)

box.Gmedio <- ggplot(data=filter(data, data$gastomedio < 900), aes(x=gastomedio))
box.Gmedio + geom_boxplot()

nrow(filter(data, data$gastomedio > 900)) * 100 /nrow(data)

nrow(filter(data, data$gastomedio > 900))

sd(filter(data, data$gastomedio < 900)$gastomedio)


col.GmedioPais  <- ggplot(data=data, aes(x=pais, y=mean(gastomedio))) 
col.GmedioPais  + geom_col(fill = rgb(0.2,0.4,0.6,0.6)) +
  scale_x_continuous(name= "Pais", labels = paises, breaks= seq(1:15)) +
  theme(axis.text.x = element_text(angle= 75, hjust=1),) +
  labs(y="Gasto total", title="Gasto medio dividido por paises")


# Correlacion Variables Cuantitativas
cuant.data <- select(data, A13, gastototal, gastomedio)
corrplot(cor(cuant.data), method = 'number', type= "upper")

# Contingencia Variables Cualitativas
cual.data <- select(data, mes, A0_7, A1, pais, ccaa, aloja, motivo, A16)
table(cual.data$mes)

# eliminar variables 
cln.data <- data

cln.data$A0_1 <- NULL
cln.data$A0_7 <- NULL
cln.data$factoregatur <- NULL

view(cln.data)

write.table(cln.data, file = "Datos/clean_data.txt", sep = ";",
            row.names = FALSE)


