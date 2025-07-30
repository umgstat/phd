


gemelle = read.csv(  file.choose()  ,  header = TRUE  )
attach(gemelle)

head(gemelle)
gemelle

a5 = c(73.6, 73.4, 74.1, 73.5, 73.2)
e5 = c(73.8, 73.5, 74.6, 73.8, 73.6)

t.test(a5, e5)

colpodiscena = lm(peso ~ gemella)
summary(colpodiscena)



coef(colpodiscena)
summary(colpodiscena)$sigma


set.seed(123)
pesosimulato = c(rep(73.7, 21), rep(73.9, 21)) + rnorm(42, 0, 0.37)

par(mfrow = c(1,2))
plot(peso[gemella == "alice"], peso[gemella == "ellen"])
plot(pesosimulato[gemella == "alice"], pesosimulato[gemella == "ellen"])
par(mfrow = c(1,1))

## install.packages("lme4")

library(lme4)

relazionebase = peso ~ 1 
modellobase = lm(relazionebase)
summary(modellobase)

relazionenuova = peso ~ 1 + ( 1 | gemella  ) 
mixedmodel = lmer(relazionenuova)
summary(mixedmodel)


1:21

tempo = c(1:21, 1:21)
tempo

data.frame(tempo, peso, gemella)


relazioneperfetta = peso ~ tempo * gemella + ( 1 | gemella  )
perfetto = lmer(relazioneperfetta)
summary(perfetto)



