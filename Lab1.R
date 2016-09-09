# Para imprimir tablas en formato latex
library(xtable)

# Aumenta el tamaño de impresión
options(width=as.integer(1000))


# Leer datos desde el archivo
data = read.csv(file = "./DatosControl.csv", sep = ";")


writeLines('########################################################################')
writeLines('1 Medidas de Tendencia y dispersión.')
writeLines('########################################################################')

# Temporalmente cambia el tipo de datos para VTR e Inasistencias
dataT = data
dataT[, 3] <- as.factor(data[, 3])
dataT[, 7] <- as.factor(data[, 7])

writeLines("\n")
writeLines('a) Realice una tabla resumiendo a grandes rasgos el dataset.')
summary(dataT)
xtable(summary(dataT[,c(1,2,3,4)]))
xtable(summary(dataT[,c(5,6,7,8)]))

writeLines("\n")
writeLines('b) Encuentre el máximo y mı́nimo para la variable VTR.')
summary(data[, 3])

writeLines("\n")
writeLines('c) Encuentre la cantidad de personas que reprobaron los ramos con nota <55 y la frecuencia relativa asociada.')
table(data[,8])[1]
sum(data[,8]=="<55")/length(data[,8])

writeLines("\n")
writeLines("d) Construya un nuevo dataset que incluya solo a aquellos alumnos que reprobaron los ramos con nota <55")
dataT <- subset(data, Nota.Final == "<55")
writeLines("  i) ¿Cuántas personas dicen haber mantenido un estado de salud ’Bueno’ o ’Muy Bueno’ y cuantas un estado de salud ’Malo’ o ’Muy Malo’?")
sum(dataT[,6] == "Buena" | dataT[,6] == "Muy Buena")
sum(dataT[,6] == "Suficiente" | dataT[,6] == "Muy Mala")

writeLines("  ii) ¿Cuántas personas dicen haber estudiado 2-5 Hr semanalmente?")
sum(dataT[,2] == "2-5 hr")

writeLines("  iii) ¿Cuántas personas tuvieron una cantidad de inasistencias menor a 5, un estado de salud ’Bueno’ o ’Muy Bueno’ y dedicaron al estudio de su asignatura a lo menos 5-10 Hr?")
sum(dataT[,7] < 5 & ( dataT[,6] == "Muy Buena" | dataT[,6] == "Muy Buena" ) & dataT[,2] == "2-5 hr" )

writeLines("\n")
writeLines("e) Construya una tabla de resumen de la variable VTR")
c(summary(data[,3]), "Std.Dev."=sd(data[,3]), "Var"=var(data[,3]))
xtable(data.frame(x=c(summary(data[,3]), "Std.Dev."=sd(data[,3]), "Var"=var(data[,3]))))

writeLines("\n")
writeLines("f) Obtenga sub-muestras de tamaño n = 200, 500 y 700 para la variable Inasistencias, encuentre la media de cada sub-muestra.")
subData1 = sample(data[,7], size = 200)
subData2 = sample(data[,7], size = 500)
subData3 = sample(data[,7], size = 700)
mean(subData1)
mean(subData2)
mean(subData3)


writeLines('\n\n\n########################################################################')
writeLines('2 Representación Gráfica de Datos.')
writeLines('########################################################################')

writeLines("\n")
writeLines("a) Constuya un grafico de barras para la distribución de frecuencias de cada uno de los estados de salud de la variable Salud.")
counts <- factor(data$Salud, levels=c("Muy Mala", "Suficiente", "Normal", "Buena", "Muy Buena"))
plot(counts, main="Distribución de Salud", xlab="Nivel de Salud", ylab="Alumnos")
