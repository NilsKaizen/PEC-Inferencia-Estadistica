# Fijar la carpeta de trabajo
setwd("~/Datos")

# Importar la libreria tidyverse
library(tidyverse)

# Importar las Bases de datos
raw.data.19 <- read.csv(file= "eg_2019.txt", sep=";", header = TRUE, stringsAsFactors = FALSE)
head(raw.data.19)

# Paso 1 Busqueda de Valores nulos
any(is.na(raw.data.19))

  # Paso 1.1
  for (i in seq(1:9)) {
    if (any(is.na(raw.data.19[i])) == TRUE) {
      print(colnames(raw.data.19[i]))
    }
    
  }

  # Paso 1.2 eliminación de valores nulos

  sum(is.na(raw.data.19$factoregatur))
  
  raw.data.19 <- as_tibble(raw.data.19)
  
  raw.data.19 <- drop_na(raw.data.19)
  any(is.na(raw.data.19))
  
  
# Paso 2 Eliminación de columnas innecesarias

head(raw.data.19)

  # Paso 2.1 eliminación del año de columna mm_aaaa
  raw.data.19 <- separate(raw.data.19, mm_aaaa, into = c("mes", "año"), sep="20")
  raw.data.19$año <- NULL
  tail(raw.data.19)  
  
  # Paso 2.2 eliminación de columna A0
  unique(raw.data.19$A0)
  raw.data.19$A0 <-  NULL
  head(raw.data.19)
  
  # Paso 2.3 verificar si el id de cada encuesta es único 
  length(unique(raw.data.19$A0_1))
  arrange(raw.data.19, raw.data.19$A0_1)
  
clean.data.19 <- raw.data.19
rm(raw.data.19)

write.table(clean.data.19, file = "clean_eg_2019.txt", sep = ";",
            row.names = FALSE)







    