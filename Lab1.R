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
length(data[,1])
ans = summary(dataT)
ans
xtable(ans[,c(1,2,3,4)])
xtable(ans[,c(5,6,7,8)])

writeLines("\n")
writeLines('b) Encuentre el máximo y mı́nimo para la variable VTR.')
ans = summary(data[, 3])[c(1,6)]
ans
xtable(data.frame(VTR=c(ans)))

writeLines("\n")
writeLines('c) Encuentre la cantidad de personas que reprobaron los ramos con nota <55 y la frecuencia relativa asociada.')
ans = c(
  Cantidad=sum(data[,8]=="<55"),
  Rel.Freq=sum(data[,8]=="<55")/length(data[,8])
)
ans
xtable(data.frame(Reprobados=ans))

writeLines("\n")
writeLines("d) Construya un nuevo dataset que incluya solo a aquellos alumnos que reprobaron los ramos con nota <55")
dataT <- subset(data, Nota.Final == "<55")

writeLines("  i) ¿Cuántas personas dicen haber mantenido un estado de salud ’Bueno’ o ’Muy Bueno’ y cuantas un estado de salud ’Malo’ o ’Muy Malo’?")
ans = c(
  Buena=sum(dataT[,6] == "Buena" | dataT[,6] == "Muy Buena"),
  Mala=sum(dataT[,6] == "Suficiente" | dataT[,6] == "Muy Mala")
)
ans
xtable(data.frame(Salud=ans))

writeLines("  ii) ¿Cuántas personas dicen haber estudiado 2-5 Hr semanalmente?")
ans = sum(dataT[,2] == "2-5 hr")
ans
xtable(data.frame(Horas=c("2-5"=ans)))

writeLines("  iii) ¿Cuántas personas tuvieron una cantidad de inasistencias menor a 5, un estado de salud ’Bueno’ o ’Muy Bueno’ y dedicaron al estudio de su asignatura a lo menos 5-10 Hr?")
ans = c(
  Cantidad=sum(dataT[,7] < 5 & ( dataT[,6] == "Muy Buena" | dataT[,6] == "Buena" ) & dataT[,2] == "5-10 hr" )
)
ans
xtable(data.frame(Buenos=ans))

writeLines("  iv) Analice y concluya lo que usted cree que pueden indicar sus cálculos.")
# Procentaje "responsables" reprobados
ans*100/length(data[,1])

# inasistencias por VTR
ans = data.frame(
  "0" = mean(subset(data, VTR == 0)[,7]),
  "1" = mean(subset(data, VTR == 1)[,7]),
  "2" = mean(subset(data, VTR == 2)[,7])
)
ans
xtable(data.frame(Inasistencias.VTR=ans))


writeLines("\n")
writeLines("e) Construya una tabla de resumen de la variable VTR")
ans = c(summary(data[,3])[c(3,4,5,6)], "Std.Dev."=sd(data[,3]), "Var"=var(data[,3]))
ans
xtable(data.frame(VTR=ans))

writeLines("\n")
writeLines("f) Obtenga sub-muestras de tamaño n = 200, 500 y 700 para la variable Inasistencias, encuentre la media de cada sub-muestra.")
ans = c(
  "200"=mean(sample(data[,7], size = 200)),
  "500"=mean(sample(data[,7], size = 500)),
  "700"=mean(sample(data[,7], size = 700))
)
ans
xtable(data.frame(Mean.SubMuestras=ans))


writeLines('\n\n\n########################################################################')
writeLines('2 Representación Gráfica de Datos.')
writeLines('########################################################################')

writeLines("\n")
writeLines("a) Constuya un grafico de barras para la distribución de frecuencias de cada uno de los estados de salud de la variable Salud.")
ans = factor(data$Salud, levels=c("Muy Mala", "Suficiente", "Normal", "Buena", "Muy Buena"))
png(filename="Gsalud.png")
plot(ans, main="Distribución de Salud", xlab="Nivel de Salud", ylab="Alumnos")
dev.off()

writeLines("\n")
writeLines("b) Obtenga dos sub-muestras de datos, una que contenga sólo mujeres y la otra sólo hombres, para cada una de estas sub-muestras construya un boxplot para la variable Inasistencias.")
ansF = Mujeres=subset(data, Sexo == "F")[,7]
ansM = Hombres=subset(data, Sexo == "M")[,7]
ansMax = max(length(ansF), length(ansM))
ans = data.frame(Mujeres=c(ansF, rep(ansF, ansMax - length(ansF))), Hombres=c(ansM, rep(NA, ansMax - length(ansM))) )
png(filename="GinasistenciasMF.png")
boxplot(ans, main="Inasistencias por Género", ylab="Inasistencias", xlab="Sexo")
dev.off()

writeLines("\n")
writeLines("c) Realice un histograma de la variable Inasistencias utilizando la regla de Sturges para calcular el numero de clases del histograma.")
ans = data[,7]
png(filename="Ginasistencias2.png")
ans = hist(ans, main="Inasistencias", xlab="Número Inasistencias", ylab="Frecuencia", breaks="Sturges")
dev.off()
ans
ans$counts/length(data[,7])
cumsum(ans$counts)

writeLines("\n")
writeLines("d) Para finalizar, investigue y realice al menos dos conclusiones generales sobre los datos que involucren cualquiera de las medidas y gráficos que obtuvo.")

# inasistencias por Carrete
ans = c(
  "Demasiado" = mean(subset(data, Carrete == "Demasiado")[,7]),
  "Mucho" = mean(subset(data, Carrete == "Mucho")[,7]),
  "Normal" = mean(subset(data, Carrete == "Normal")[,7]),
  "Poco" = mean(subset(data, Carrete == "Poco")[,7]),
  "Nada" = mean(subset(data, Carrete == "Nada")[,7])
)
ans
xtable(data.frame(Inasistencias.Carrete=ans))
