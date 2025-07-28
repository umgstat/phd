

diabete = read.csv(  file.choose()  ,  header = TRUE , sep = ","  )
attach(diabete)



## ipotesi1 = peso ~ statura
## ipotesi2 = peso ~ genere
## ipotesi3 = peso ~ sport


ipotesi4 = peso ~ statura + genere

ipotesi5 = peso ~ statura * genere



modello4 = lm(ipotesi4)
summary(modello4) 

plot(statura, peso)

points(statura[genere == "f"], peso[genere == "f"], col = "magenta")
points(statura[genere == "m"], peso[genere == "m"], col = "blue")

abline(-35.4, 0.55, col = "magenta")
abline(-35.4 + 7.2, 0.55, col = "blue")








modello5 = lm(ipotesi5)
summary(modello5)

plot(statura, peso)

points(statura[genere == "f"], peso[genere == "f"], col = "magenta")
points(statura[genere == "m"], peso[genere == "m"], col = "blue")

abline(-18.5, 0.45, col = "magenta")
abline(-18.5 - 26.0  , 0.45 + 0.19, col = "blue")







