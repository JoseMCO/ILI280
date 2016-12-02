## 1) Contraste de Hipótesis ##########################################
# b ###################################################################
writeLines('Población')
population <- rexp(100000, rate=0.015)
population <- (c(99*population/max(population) +1))
summary(population)

# c ###################################################################
writeLines('Muestra 1')
sample <- rexp(1000, rate=0.015)
sample <- (c(99*sample/max(sample) +1))
summary(sample)

# f ###################################################################
writeLines('T-test')
t90 <- t.test(x=population, y=sample, alternative="two.sided", conf.level=0.90)
t95 <- t.test(x=population, y=sample, alternative="two.sided", conf.level=0.95)
t99 <- t.test(x=population, y=sample, alternative="two.sided", conf.level=0.99)
t90; t95; t99

# g ###################################################################
writeLines('Cuantiles')
t90.qt <- qt(c(0.90), df=length(t90)-1)
t95.qt <- qt(c(0.95), df=length(t95)-1)
t99.qt <- qt(c(0.99), df=length(t99)-1)
t90.qt; t95.qt; t99.qt

# h ###################################################################
writeLines('P-values')
t90.pv <- t90$p.value
t95.pv <- t95$p.value
t99.pv <- t99$p.value
t90.pv; t95.pv; t99.pv

## 2) Contraste de Hipótesis 2 ########################################
# a ###################################################################
writeLines('Muestra 2')
sample2 <- rexp(1000, rate=0.020)
sample2 <- (c(99*sample2/max(sample2) +1))
summary(sample2)

# b ###################################################################
writeLines('T-test 2')
t290 <- t.test(x=sample, y=sample2, alternative="two.sided", conf.level=0.90)
t295 <- t.test(x=sample, y=sample2, alternative="two.sided", conf.level=0.95)
t299 <- t.test(x=sample, y=sample2, alternative="two.sided", conf.level=0.99)
t290; t295; t299

# e ###################################################################
writeLines('Error II')
s2delta = mean(population)-mean(sample2)
tp290 <- power.t.test(n=length(t290), sd=sd(sample2), sig.level=0.10, delta=s2delta, alternative="two.sided", strict = TRUE)
tp295 <- power.t.test(n=length(t290), sd=sd(sample2), sig.level=0.10, delta=s2delta, alternative="two.sided", strict = TRUE)
tp299 <- power.t.test(n=length(t290), sd=sd(sample2), sig.level=0.10, delta=s2delta, alternative="two.sided", strict = TRUE)
tp290$power-1; tp295$power-1; tp299$power-1
