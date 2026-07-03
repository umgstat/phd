

##### UNO ###

set.seed(123)  # per riproducibilit??

# Numero di simulazioni
n <- 100000

# Generazione dati
X <- rnorm(n, mean = 10, sd = sqrt(4))  # varianza 4 -> sd = 2
Y <- rnorm(n, mean = 5, sd = sqrt(9))   # varianza 9 -> sd = 3

# Somma
Z <- X + Y

# ???? Verifica media e varianza
mean_X <- mean(X)
var_X  <- var(X)

mean_Y <- mean(Y)
var_Y  <- var(Y)

mean_Z <- mean(Z)
var_Z  <- var(Z)

cat("MEDIA:\n")
cat("X =", mean_X, " (atteso 10)\n")
cat("Y =", mean_Y, " (atteso 5)\n")
cat("Z =", mean_Z, " (atteso 15)\n\n")

cat("VARIANZA:\n")
cat("X =", var_X, " (atteso 4)\n")
cat("Y =", var_Y, " (atteso 9)\n")
cat("Z =", var_Z, " (atteso 13)\n\n")

# ???? Grafico
hist(Z, probability = TRUE, breaks = 50,
     main = "Distribuzione di Z = X + Y",
     xlab = "Valori di Z")

# Sovrappongo la densit?? teorica
curve(dnorm(x, mean = 15, sd = sqrt(13)),
      col = "red", lwd = 2, add = TRUE)









##### DUE ###








set.seed(123)

n <- 100000

# X normale
X <- rnorm(n, mean = 0, sd = 1)

# Variabile +/-1
U <- sample(c(-1, 1), n, replace = TRUE)




##### TRE ###


# Definisco Y
Y <- U * X
hist(Y)



##### QUATTRO ###


boxplot(Y ~ factor(U))



##### CINQUE ###




# ??? Verifica: X e Y sono normali
par(mfrow = c(1,2))

hist(X, probability = TRUE, breaks = 50,
     main = "X ~ N(0,1)")
curve(dnorm(x, 0, 1), col = "red", lwd = 2, add = TRUE)

hist(Y, probability = TRUE, breaks = 50,
     main = "Y (ancora normale!)")
curve(dnorm(x, 0, 1), col = "red", lwd = 2, add = TRUE)

par(mfrow = c(1,1))


##### SEI ###




# ??? Ora combinazione lineare
Z <- X + Y

# ???? Grafico di Z


hist(Z, probability = TRUE, breaks = 50,
     main = "Z = X + Y (NON normale!)",
     xlab = "Z")

# confronto con normale teorica (stesso mean e var empirici)
curve(dnorm(x, mean(Z), sd(Z)),
      col = "red", lwd = 2, add = TRUE)

# ???? Statistiche
cat("Media Z:", mean(Z), "\n")
cat("Varianza Z:", var(Z), "\n")

# ???? Scatter plot per capire cosa succede
plot(X, Y, pch = 16, col = rgb(0,0,1,0.2),
     main = "Relazione tra X e Y")
abline(a = 0, b = 1, col = "red")
abline(a = 0, b = -1, col = "red")




##### SETTE ###




set.seed(123)

n <- 100000

# -------------------------
# CASO 1: MISCELA DI NORMALI
# -------------------------

# scelgo una popolazione con probabilità 1/2
gruppo <- sample(c(1, 2), n, replace = TRUE)

# X proviene da N(-3,1) oppure N(3,1)
mix <- ifelse(gruppo == 1,
              rnorm(n, mean = -3, sd = 1),
              rnorm(n, mean = 3, sd = 1))

# -------------------------
# CASO 2: SOMMA DI NORMALI
# -------------------------

X <- rnorm(n, mean = -3, sd = 1)
Y <- rnorm(n, mean = 3, sd = 1)

Z <- X + Y

# -------------------------
# GRAFICI
# -------------------------

par(mfrow = c(1,2))

# Miscela
hist(mix,
     prob = TRUE,
     breaks = 60,
     main = "Miscela di due Normali",
     xlab = "x")

curve(0.5*dnorm(x,-3,1)+0.5*dnorm(x,3,1),
      add = TRUE,
      lwd = 3,
      col = "red")

# Somma
hist(Z,
     prob = TRUE,
     breaks = 60,
     main = "Somma: X + Y",
     xlab = "z")

curve(dnorm(x, mean = 0, sd = sqrt(2)),
      add = TRUE,
      lwd = 3,
      col = "blue")

par(mfrow = c(1,1))



cat("MISCELA\n")
cat("media =", mean(mix), "\n")
cat("varianza =", var(mix), "\n\n")

cat("SOMMA\n")
cat("media =", mean(Z), "\n")
cat("varianza =", var(Z), "\n")



