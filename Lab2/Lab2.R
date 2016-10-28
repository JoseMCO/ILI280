# 1 ###################################################################
# a)
coinflip <- function(N=1) {
	return(rbinom(n = N, size = 1, prob = 0.5))
}

streaks <- function(throws) {
	st <- rle(throws)
	return(st$lengths[which(st$values==1)])
}

# b)
throw100 <- coinflip(100)
throw200 <- coinflip(200)
throw500 <- coinflip(500)
throw1000 <- coinflip(1000)

# c)
st100 <- streaks(throw100)
st200 <- streaks(throw200)
st500 <- streaks(throw500)
st1000 <- streaks(throw1000)

table(st100)
table(st200)
table(st500)
table(st1000)

# d)
table(st1000)/1000

# e)
png("Hist1000-st.png")
histST <- hist(st1000, main="Rachas en 1000 lanzamientos (Sturges)", xlab="Largo racha", ylab="Repeticiones", breaks=nclass.Sturges(st1000))
dev.off()

png("Hist1000-fd.png")
histFD <- hist(st1000, main="Rachas en 1000 lanzamientos (Freedman-Diaconis)", xlab="Largo racha", ylab="Repeticiones", breaks=nclass.FD(st1000))
dev.off()

png("Hist1000-sc.png")
histSC <- hist(st1000, main="Rachas en 1000 lanzamientos (Scott)", xlab="Largo racha", ylab="Repeticiones", breaks=nclass.scott(st1000))
dev.off()

png("Hist1000-5.png")
hist5 <- hist(st1000, main="Rachas en 1000 lanzamientos (4)", xlab="Largo racha", ylab="Repeticiones", breaks=5)
dev.off()

png("Hist1000-2.png")
hist2 <- hist(st1000, main="Rachas en 1000 lanzamientos (2)", xlab="Largo racha", ylab="Repeticiones", breaks=2)
dev.off()

# f)
png("Hist1000-ds.png")
hist(st1000, prob=TRUE, main="Rachas en 1000 lanzamientos", xlab="Largo racha", ylab="Probabilidad")# prob=TRUE for probabilities not counts
lines(density(st1000), col="blue", lwd=2) # add a density estimate with defaults
dev.off()

# 2 ###################################################################
# a)
sample <- rnorm(250,177.74,7.18)

# b)
waveH <- pnorm(170, 177.74, 7.18, lower.tail=FALSE)

# c)
waveL <- pnorm(150, 177.74, 7.18, lower.tail=TRUE)

# e)
sample100 <- sort(rnorm(100,0,0.5))
sample500 <- sort(rnorm(500,0,0.5))
sample1000 <- sort(rnorm(1000,0,0.5))
sample15000 <- sort(rnorm(15000,0,0.5))

# f)
dens100 <- dnorm(sample100,0,0.5)
dens500 <- dnorm(sample500,0,0.5)
dens1000 <- dnorm(sample1000,0,0.5)
dens15000 <- dnorm(sample15000,0,0.5)

# g)
png("sample100.png")
hist(sample100, prob=TRUE, main="Población aleatroria (100)", xlab="Valor", ylab="Frecuencia")
curve(dnorm(x,mean=mean(sample100), sd=sd(sample100)), add = TRUE)
dev.off()

png("sample500.png")
hist(sample500, prob=TRUE, main="Población aleatroria (500)", xlab="Valor", ylab="Frecuencia")
curve(dnorm(x,mean=mean(sample500), sd=sd(sample500)), add = TRUE)
dev.off()

png("sample1000.png")
hist(sample1000, prob=TRUE, main="Población aleatroria (1000)", xlab="Valor", ylab="Frecuencia")
curve(dnorm(x,mean=mean(sample1000), sd=sd(sample1000)), add = TRUE)
dev.off()

png("sample15000.png")
hist(sample15000, prob=TRUE, main="Población aleatroria (15000)", xlab="Valor", ylab="Frecuencia")
curve(dnorm(x,mean=mean(sample15000), sd=sd(sample15000)), add = TRUE)
dev.off()


