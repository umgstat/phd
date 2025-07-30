
# % # % # % # % # % # % # % # % # % # % # % # % # % # % # %
# %
# %  per iniziare facilmente ..
# % 
# % # % # % # % # % # % # % # % # % # % # % # % # % # % # %


percussive = read.table(file.choose(), header = TRUE)
attach(percussive)

head(percussive)
tail(percussive)

str(percussive)
table(subject)
table(treatment)
table(time)
time = time - 12
treatment = factor(treatment)


treatment1 = treatment[1:70]
levels(treatment)[1] = "hfpv"
levels(treatment)[2] = "control"


library(lattice)

xyplot(pafi ~ time | treatment, type = "b", groups = subject,
       xlab = "time (hour)", ylab = expression(PaO[2] / FiO[2]))



ancovasbagliatocross = lm(pafi ~ time * treatment)
summary(ancovasbagliatocross)

ancovasbagliatoplus = lm(pafi ~ time + treatment)
summary(ancovasbagliatoplus)

AIC(ancovasbagliatocross, ancovasbagliatoplus)

######## analisi appropriata #######

library(lme4)


relazione1 = pafi ~ time * treatment + ( time  | subject  )
modello1 = lmer(relazione1)
summary(modello1)

relazione2 = pafi ~ time + treatment + ( time  | subject  )
modello2 = lmer(relazione2)
summary(modello2)

### attenzione, stiamo SBAGLIANDO !!!!

AIC(modello1, modello2)

### dobbiamo usare il principio di Massima Verosimiglianza




modello1_ML = lmer(relazione1, REML = FALSE)
# summary(modello1_ML)
modello2_ML = lmer(relazione2, REML = FALSE)
AIC(modello1_ML, modello2_ML)



