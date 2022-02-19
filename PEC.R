# Importar las Bases de datos
raw.data.19 <- read.csv(file= "eg_2019.txt", sep=";", header = TRUE, stringsAsFactors = FALSE)
head(raw.data.19)

# Buscamos algún valor nulo
any(is.na(raw.data.19))