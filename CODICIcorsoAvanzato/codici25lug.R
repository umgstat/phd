
percussive = read.table(file.choose(), header = TRUE)
attach(percussive)

head(percussive)
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


######## analisi appropriata  #######


library(lme4)


effettimisti1 = pafi ~ time * treatment + (time | subject)


modellomisto1 = lmer(effettimisti1)
summary(modellomisto1)



effettimisti1 = pafi ~ time * treatment + (time | subject)
effettimisti2 = pafi ~ time + treatment + (time | subject)
modellomisto2 = lmer(effettimisti2)
summary(modellomisto2)


AIC(modellomisto2, modellomisto1) # sbagliato

sciocco = c(59, 34, 37, 20)
sd(sciocco)


effettimisti1 = pafi ~ time * treatment + (time | subject)
effettimisti2 = pafi ~ time + treatment + (time | subject)

modellomisto1.ML = lmer(effettimisti1, REML = FALSE)
modellomisto2.ML = lmer(effettimisti2, REML = FALSE)
AIC(modellomisto2.ML, modellomisto1.ML) # giusto


summary(modellomisto1.ML)
summary(modellomisto1)





