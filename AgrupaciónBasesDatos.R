library(tidyverse)

# Función de Nombres
setwd("~/Datos")

i <- 0
data <- data.frame()

for (i in seq(1:12)) {
  if (i <= 9) {
    b <- paste('elevado_eg_mod_web_tur_','0', toString(i), '19.txt', sep='')
  } else {
    b <- paste('elevado_eg_mod_web_tur_', toString(i), '19.txt', sep='')
  }
  
  temp <- read.csv(b, sep=";", header = TRUE, stringsAsFactors = FALSE)
  
  data <- rbind(data, temp)
}

# Exportar la base de datos 

write.table(data, file = "eg_2019.txt", sep = ";",
            row.names = FALSE)

# Limpiar Variables

rm(temp)
rm(data)
rm(b)
rm(i)
