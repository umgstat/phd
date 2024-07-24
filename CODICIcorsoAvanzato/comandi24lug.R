
www = "https://raw.githubusercontent.com/umgstat/phd/main/DATASET/studenti.csv"
studenti = read.csv(www , header = TRUE , sep =";")
attach(studenti)
relaz = peso ~ scarpe
modello = lm(relaz)
summary(modello)
plot(scarpe, peso, main = "dati reali")
abline(modello)

y = -56.6 + 2.94 * scarpe
plot(scarpe, y)
abline(modello)

length(scarpe)

errori = rnorm(n = 65, mean = 0, sd = 5.22)
errori

z = y + errori
plot(scarpe, z, main = "dati simulati")
abline(modello)

par(mfrow = c(1,2))
plot(scarpe, peso, main = "dati reali")
abline(modello)
plot(scarpe, z, main = "dati simulati")
abline(modello)
par(mfrow = c(1,1))


attach(anscombe)
relsbagliata = y2 ~ x2
modelsb = lm(relsbagliata)
coef(modelsb)
yyy = 0.5 * x2 + 3
sigma(modelsb)
summary(modelsb)
errr = rnorm(n = 11, mean = 0, sd = 1.24 )
zzzz = yyy + errr


par(mfrow = c(1,2))
plot(x2, y2, main = "dati reali")
abline(modelsb)
plot(x2, zzzz, main = "dati simulati")
abline(modelsb)
par(mfrow = c(1,1))



relperfetta = y2 ~ x2 + I(x2^2)
modelperfetto = lm(relperfetta)
summary(modelperfetto)

yyy = -6 + 2.78 * x2 - 0.13 * x2 * x2
errr = rnorm(n = 11, mean = 0, sd = 0.002 )
zzzz = yyy + errr


par(mfrow = c(1,2))
plot(x2, y2, main = "dati reali")
plot(x2, zzzz, main = "dati simulati")
par(mfrow = c(1,1))

###############


gemelle = read.csv( file.choose() , header = TRUE  )
attach(gemelle)

? rm

gemelle = read.csv( file.choose() , header = TRUE  )
attach(gemelle)


detach(studenti)

peso

tail(gemelle)
head(gemelle)


relsemplice = peso ~ gemella
modsemplice = lm(relsemplice)
summary(modsemplice)

alice = peso[1:21]
ellen = peso[22:42]

plot(jitter(alice), jitter(ellen))
abline(73.66, 0.28)
abline(modsemplice)

plot(jitter(alice), jitter(ellen), xlim = c(0,80), ylim = c(0,80))
abline(73.66, 0.28)

par(mfrow = c(1,3))
plot(jitter(alice), jitter(ellen), main = "21")
plot(jitter(alice), jitter(ellen), xlim = c(0,80), ylim = c(0,80))
abline(73.66, 0.28)
plot(gemella, peso)
abline(73.66, 0.28)




xcaso = runif(42, min = min(peso) , max = max(peso))
rumore = rnorm(42 , mean = 0, sd= 0.37)
ycaso = 73.66 + 0.28 * (as.numeric(as.factor(gemella))-1) + rumore
plot(xcaso, ycaso)
plot(jitter(alice), jitter(ellen))

library(lme4)

relazione1 = peso ~ gemella + ( 1 | gemella ) 
modello1 = lmer(relazione1)
modello1



fixef(modello1)
ranef(modello1)

summary(modello1)

AIC(modello1)




relazione0 = peso ~ 1 + ( 1 | gemella ) 

modello0 = lmer(relazione0)
modello0

mean(peso)
sd(peso)

summary(modello0)
AIC(modello0)








