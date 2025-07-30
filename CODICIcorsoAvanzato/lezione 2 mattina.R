


studenti = read.csv(  file.choose()  ,  header = TRUE , sep = ";"  )
attach(studenti)

## step 1 - formulare un modello

## ipotesi1 = peso ~ statura    ## univariata
## ipotesi2 = peso ~ genere
## ipotesi3 = peso ~ sport
## ipotesi4 = peso ~ statura + genere    ##  additivo   
## ipotesi5 = peso ~ statura * genere    ## con interazione

## step 2 - selezionare il modello / criterio AIC 

## step 3 - diagnostica del modello ("quartetto di Anscombe")
## esempio airquality


attach(airquality)
str(airquality)
names(airquality)


relazione = Ozone ~ Solar.R + Wind + Temp
proposta = lm(relazione)
summary(proposta)


attach(anscombe)
str(anscombe)

m1 = lm(y1 ~ x1)
plot(m1)

m2 = lm(y2 ~ x2)
plot(m2)

par(mfrow = c(2,2))
plot(m2)
par(mfrow = c(1,1))

detach(anscombe)


relazione = Ozone ~ Solar.R + Wind + Temp
proposta = lm(relazione)
summary(proposta)

par(mfrow = c(2,2))
plot( proposta  )
par(mfrow = c(1,1))


relazione2 = Ozone ~ Solar.R + Wind + I(Wind^2)   + Temp
proposta2 = lm(relazione2)
summary(proposta2)


par(mfrow = c(2,2))
plot( proposta2 )
par(mfrow = c(1,1))

detach(airquality)

###  ? rm

#### modello nullo

mean(peso)
sd(peso)
t.test(peso)
sd(peso)/ sqrt(length(peso))

ipotesinulla = peso ~ 1
modellonullo = lm(ipotesinulla)
summary(modellonullo)


## effetti fissi ed effetti casuali

relaz = peso ~ scarpe
modello = lm(relaz)
abline(modello)
plot(relaz, main = "dati reali")

## effetti fissi: intercetta, pendenza

modello  ## a = -56, b = 2.9
coef(modello)


## effetti casuali:

summary(modello)
summary(modello)$sigma

errori = rnorm(n = 65, mean = 0, sd = 5.22)

y = scarpe * 2.94 -56.55 

plot(scarpe, y)


errori = rnorm(n = 65, mean = 0, sd = 5.22)
plot(scarpe, y + errori)
abline(modello)
