coinflip <- function(N=1) {
	return(rbinom(n = N, size = 1, prob = 0.5))
}

streaks <- function(throws) {
	st <- rle(throws)
	return(st$lengths[which(st$values==1)])
}

throw100 <- coinflip(100)
throw200 <- coinflip(200)
throw500 <- coinflip(500)
throw1000 <- coinflip(1000)

st100 <- streaks(throw100)
st200 <- streaks(throw200)
st500 <- streaks(throw500)
st1000 <- streaks(throw1000)

table(st100)
table(st200)
table(st500)
table(st1000)

table(st1000)/1000

# png("Hist1000-st.png")¬
histST = hist(st1000, main="Rachas en 1000 lanzamientos", xlab="largo de racha", ylab="repeticiones", breaks=nclass.Sturges(st1000))
# dev.off()

# png("Hist1000-fd.png")¬
histFD = hist(st1000, main="Rachas en 1000 lanzamientos", xlab="largo de racha", ylab="repeticiones", breaks=nclass.FD(st1000))
# dev.off()

# png("Hist1000-sc.png")¬
histSC = hist(st1000, main="Rachas en 1000 lanzamientos", xlab="largo de racha", ylab="repeticiones", breaks=nclass.scott(st1000))
# dev.off()

# png("Hist1000-20.png")¬
hist5 = hist(st1000, main="Rachas en 1000 lanzamientos", xlab="largo de racha", ylab="repeticiones", breaks=5)
# dev.off()

# png("Hist1000-15.png")¬
hist4 = hist(st1000, main="Rachas en 1000 lanzamientos", xlab="largo de racha", ylab="repeticiones", breaks=4)
# dev.off()

# png("Hist1000-10.png")¬
hist2 = hist(st1000, main="Rachas en 1000 lanzamientos", xlab="largo de racha", ylab="repeticiones", breaks=2)
# dev.off()


