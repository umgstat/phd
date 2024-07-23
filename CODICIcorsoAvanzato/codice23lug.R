
## importiamo dalla rete il dataset,
## sono i cosiddetti "raw data"

www = "https://raw.githubusercontent.com/umgstat/phd/main/DATASET/studenti.csv"

## il dataset è in formato csv, il separatore è ;

studenti = read.csv(www , header = TRUE , sep =";")
studenti


anno  ## non funziona
studenti$anno   ## funziona

attach(studenti)
anno   ## funziona


## la statistica DESCRITTIVA

summary(studenti)

summary(genere)

table(genere)

sd(statura)

## esploriamo il dataset 

head(studenti)
tail(studenti)

studenti[2,4]
peso[2]

studenti[2,4] == peso[2]

hist(statura)
boxplot(peso)

## passiamo alla statistica INFERENZIALE
## vogliamo investigare sul peso come
## outcome primario

names(studenti)

## un esempio di relazione, ci occorre la  ~ 

peso ~ genere


relazione1 = peso ~ genere
lm(relazione1)  ## ecco il nostro primo modello lineare

mean(peso[genere == "f"])
mean(peso[genere == "m"]) - mean(peso[genere == "f"])

modello1 = lm(relazione1)
modello1

## il modello1 non è altro che il t test

summary(modello1)
t.test(relazione1)


relazione2 = peso ~ fumo
modello2 = lm(relazione2)
summary(modello2)
t.test(relazione2)

## il modello2 è un t test non significativo

sport
fumo
genere

sport = factor(sport)
sport
fumo = factor(fumo)
fumo
genere = factor(genere)
genere

# le variabili qualitative in R si chiamano 'fattori'
# e si suddividono, in odine alfabetico, in 'livelli'


relazione3 = peso ~ statura
modello3 = lm(relazione3)
summary(modello3)

## il modello3 è la famosa retta di regressione

## facciamo un grafico della situazione
plot(relazione3, main = "peso vs. statura")
abline(-83.9 , 0.85 , col = "red")


# possiamo estrarre informazioni dal modello:
coef(modello3)
coef(modello3)[1]
coef(modello3)[2]
coef(modello3)[[1]]
coef(modello3)[[2]]


## attenzione non confondere plot(relazione3) con:

plot(modello3) # si tratta della diagnostica del modello;

# ricordate dal corso base il quartetto di Anscombe?


## ora effetuiamo una ANCOVA senza interazione
relazione4 = peso ~ statura + genere
modello4 = lm(relazione4)
summary(modello4)

plot(statura, peso)
points(statura[genere == "f"], peso[genere == "f"], col = "magenta")
points(statura[genere == "m"], peso[genere == "m"], col = "blue")
abline(-35.7, 0.55, col="magenta")
abline(-35.7+7.2, 0.55, col="blue", lty = 3)


## ora effetuiamo una ANCOVA con interazione
relazione5 = peso ~ statura * genere
modello5 = lm(relazione5)
summary(modello5)


plot(statura, peso)
points(statura[genere == "f"], peso[genere == "f"], col = "magenta")
points(statura[genere == "m"], peso[genere == "m"], col = "blue")
abline(-18.5, 0.45, col="magenta")
abline(-18.5-25.9, 0.45+0.19, col="blue", lty = 3)

# avremmo potuto anche scrivere così:
relazione5bis = peso ~ statura + genere + statura:genere

#  per il momento sospendiamo le analisi
#  del dataset studenti, ed esercitiamoci
#  in maniera autonoma con il dataset:
#
#  diabete.csv
#


### esercitazione diabete.csv


www = "https://raw.githubusercontent.com/umgstat/phd/main/DATASET/diabete.csv"

## osservazione: il dataset usa "," e non ";" per separare i campi

diabete = read.csv(www , header = TRUE)
attach(diabete)
tail(diabete)

reldiab1 = response ~ group
modeldiab1 = lm(reldiab1)
summary(modeldiab1)


reldiab2 = response ~ serumprotein
modeldiab2 = lm(reldiab2)
summary(modeldiab2)
plot(serumprotein, response)
abline(modeldiab2)



reldiab3 = response ~ serumprotein + group
modeldiab3 = lm(reldiab3)
summary(modeldiab3)


reldiab4 = response ~ serumprotein * group
modeldiab4 = lm(reldiab4)
summary(modeldiab4)

## chi scegliamo? 

AIC(modeldiab4, modeldiab3, modeldiab2, modeldiab1)
## scegliamo modeldiab1

## come visualizziamo ?
boxplot(response ~ group)


## inventiamo "un titolo" per la nostra ricerca
## serumprotein predicts response
## group but not setumprotein predicts response


#### ritorniamo agli studenti

AIC(modello5, modello4, modello3, modello2, modello1)


relazmassimale = peso ~ anno + genere + statura + scarpe + fumo + sport + frequenza + massima + minima

modmassimale = lm(relazmassimale)

step(modmassimale)



relminimale = peso ~ statura + scarpe + sport + frequenza
minimale = lm(relminimale)
controllo1  = peso ~ statura * scarpe + sport + frequenza
modelloctrl1 = lm(controllo1)
AIC(minimale, modelloctrl1)
controllo2  = peso ~ statura * frequenza + scarpe + sport 
modelloctrl2 = lm(controllo2)
AIC(minimale, modelloctrl2)





