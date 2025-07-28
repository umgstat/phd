

studenti = read.csv(  file.choose()  ,  header = TRUE , sep = ";"  )

studenti

genere  # non funziona

studenti$genere

attach(studenti)
genere  #  funziona

genere = factor(genere)

table(genere)

summary(peso)
sd(peso)


head(studenti)
tail(studenti)

peso[1]


studenti[2,3]

studenti[,3]

studenti[2,]



studenti[2,3] == statura[2]

hist(peso)
boxplot(statura)

#### il modello lineare (retta di regressione)

ipotesi1 = peso ~ statura

modello1 = lm(ipotesi1)

plot(ipotesi1)
abline(modello1)
abline(-83.8, 0.85, col = "purple", lwd = 3)
summary(modello1)


## che sciocchezza

ipotesi1bis = peso ~ anno

modello1bis = lm(ipotesi1bis)

plot(ipotesi1bis)
abline(modello1bis)
summary(modello1bis)









ipotesi2 = peso ~ genere
t.test(ipotesi2)




modello2 = lm(ipotesi2)
summary(modello2)



sport = factor(sport)

levels(sport)


ipotesi3 = peso ~ sport
t.test(ipotesi3) # ovviamente non funziona
aov(ipotesi3) # giusto ma obsoleto
modello3 = lm(ipotesi3)
plot(ipotesi3)

tapply(peso, sport, mean)


