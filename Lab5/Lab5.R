resample <- function(data, n) {
  sim = numeric(n)
  for(i in 1:n) {
    sim[i] = mean(sample(data, n, replace = TRUE))
  }
  return(sim)
}

interval <- function(data, conf) {
  n=length(data)
  stdError = sd(data) / sqrt(n)
  error = qt(conf, df=n-1 ) * stdError
  return(mean(data) + c(error, -error))
}

## 1) Sobre Bootstrap #################################################
# c,d,e ###############################################################
writeLines('\nMuestra y Re-muestra')
data <- rnorm(1000, 30, 5)
summary(data)
samples <- resample(data, 1000)
summary(samples)

writeLines('\nMedia y Mediana')
mean(data)
mean(samples)
median(data)
median(samples)

## 2) Invervalos de confianza Bootstrap ###############################
# a,b #################################################################
load("CommuteAtlanta.rda")
tSamples <- resample(CommuteAtlanta$Time, 1000)
writeLines('\nIntervalos de Confianza')
interval(tSamples, 0.90)
interval(tSamples, 0.95)
interval(tSamples, 0.99)

# d ###################################################################
library(boot)
b = boot(CommuteAtlanta$Time, function(x,d){return(mean(x[d]))}, R=1000)
writeLines('\nIntervalos de Confianza con boot')
boot.ci(b, conf=0.90)$normal
boot.ci(b, conf=0.95)$normal
boot.ci(b, conf=0.99)$normal
