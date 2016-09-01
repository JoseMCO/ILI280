# Aumenta el tamaño de impresión
options(width=as.integer(1000))


# Leer datos desde el archivo
data = read.csv(file = "./DatosControl.csv", sep = ";")


# 1 Medidas de Tendencia y dispersión

# Temporalmente cambia el tipo de datos para VTR e Inasistencias
dataT = data
dataT[, 3] <- as.factor(data[, 3])
dataT[, 6] <- as.factor(data[, 6])

writeLines("\n\n")
writeLines('a) Realice una tabla resumiendo a grandes razgos el dataset.\n')
summary(dataT)

writeLines("\n\n")
writeLines('b) Encuentre el máximo y mı́nimo para la variable VTR.\n')
summary(data[, 3])

writeLines("\n\n")
writeLines('c) Encuentre la cantidad de personas que reprobaron los ramos con nota <55 y la frecuencia relativa asociada.\n')
summary(data[, 8])