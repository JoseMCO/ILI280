# source('http://www.openintro.org/stat/data/cdc.R')
source('cdc.R')

## I) Estimación Puntual ##############################################
# 1 ###################################################################
females <- subset(cdc, gender == "f")
females.mean <- mean(females$height)
females.sd <- sd(females$height)

s5 <- mean(head(females$height, 5))
s10 <- mean(head(females$height, 10))
s20 <- mean(head(females$height, 20))
s70 <- mean(head(females$height, 70))
s100 <- mean(head(females$height, 100))
s1000 <- mean(head(females$height, 1000))

females.cv <- females.sd/females.mean
png(filename="barFemales.png")
barplot(c(s5,s10,s20,s70,s100,s1000), names.arg=c(5,10,20,70,100,1000))
dev.off()

# 2 ###################################################################
range <- function(x, low, high){ (x - min(x))/(max(x)-min(x)) * (high - low) + low }
workers <- round(range(rexp(n = 1000000, rate = 0.2), 18, 70), digits = 0)
workers.mean <- mean(workers)
png(filename="histWorkers.png")
hist(workers, freq=FALSE, right=FALSE, ylab="Frecuencia relativa", xlab="Edad", main="Distribución Edades")
dev.off()

# 3 ###################################################################
workers.h100 <- head(workers, 100)
workers.h1000 <- head(workers, 1000)
workers.h10000 <- head(workers, 10000)
workers.h100000 <- head(workers, 100000)
workers.h500000 <- head(workers, 500000)

workers.h100.mean <- mean(workers.h100)
workers.h1000.mean <- mean(workers.h1000)
workers.h10000.mean <- mean(workers.h10000)
workers.h100000.mean <- mean(workers.h100000)
workers.h500000.mean <- mean(workers.h500000)

png(filename="histWorkers100.png")
hist(workers.h100, freq=FALSE, right=FALSE, ylab="Frecuencia relativa", xlab="Edad", main="Distribución edades (100)")
dev.off()
png(filename="histWorkers1000.png")
hist(workers.h1000, freq=FALSE, right=FALSE, ylab="Frecuencia relativa", xlab="Edad", main="Distribución edades (1000)")
dev.off()
png(filename="histWorkers10000.png")
hist(workers.h10000, freq=FALSE, right=FALSE, ylab="Frecuencia relativa", xlab="Edad", main="Distribución edades (10000)")
dev.off()
png(filename="histWorkers100000.png")
hist(workers.h100000, freq=FALSE, right=FALSE, ylab="Frecuencia relativa", xlab="Edad", main="Distribución edades (100000)")
dev.off()
png(filename="histWorkers500000.png")
hist(workers.h500000, freq=FALSE, right=FALSE, ylab="Frecuencia relativa", xlab="Edad", main="Distribución edades (500000)")
dev.off()

# 4 ###################################################################
nations <- c(rep("CL",3000000),rep("DE",1700000),rep("HT",650000),rep("AR",1000000))
nations.CL <- sum(nations == "CL")/length(nations)
nations.DE <- sum(nations == "DE")/length(nations)
nations.HT <- sum(nations == "HT")/length(nations)
nations.AR <- sum(nations == "AR")/length(nations)

nations.s1000 <- sample(nations, 1000)
nations.s1000.CL <- sum(nations.s1000 == "CL")/1000
nations.s1000.DE <- sum(nations.s1000 == "DE")/1000
nations.s1000.HT <- sum(nations.s1000 == "HT")/1000
nations.s1000.AR <- sum(nations.s1000 == "AR")/1000

# 5 ###################################################################
workers.300s <- replicate(300,sample(workers, 1000))
workers.300s.means <- colMeans(workers.300s)
png(filename="histWorkers300.png")
hist(workers.300s.means, freq=FALSE, right=FALSE, ylab="Frecuencia relativa", xlab="Media de Edad", main="Distribución de Media de Edades")
lines(density(workers.300s.means))
dev.off()

bias <- function(x) { ( mean(x) - as.numeric(names(sort(-table(x)))[1]) ) / sd(x) }
workers.bias <- bias(workers)
workers.300s.biases <- apply(workers.300s, 2, bias)

## II) Intervalos de Confianza ########################################
# 1 ###################################################################
workers.s100000 <- sample(workers, 100000)
workers.s100000.mean <- mean(workers.s100000)

