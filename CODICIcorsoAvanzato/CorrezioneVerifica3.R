
www = "http://www.biostatisticaumg.it/dataset/pcr.csv"
pcr = read.csv(www, header = TRUE)

str(pcr)

head(pcr)
tail(pcr)

attach(pcr)

(TargetName = factor(TargetName))

table(TargetName)
table(Well)




####################################################
#
#  soluzione proposta dal gruppo azzurro
#
####################################################

model = lm(CtGene ~ Well * TargetName)
summary(model)

model_plus = lm(CtGene ~ Well + TargetName)
summary(model_plus)

AIC(model, model_plus)

library(lme4)

relazione1 = CtGene ~ Well * TargetName + (Well | BiolRep)
modello1_ml = lmer(relazione1, data = pcr, REML = FALSE)
summary(modello1_ml)

relazione2 = CtGene ~ Well + TargetName + (Well | BiolRep)
modello2_ml = lmer(relazione2, data = pcr, REML = FALSE)
summary(modello2_ml)

AIC(modello1_ml, modello2_ml)

relazione3 = CtGene ~ Well * TargetName + (Well | TechRep)
modello3_ml = lmer(relazione3, data = pcr, REML = FALSE)
summary(modello3_ml)

relazione4 = CtGene ~ Well + TargetName + (Well | TechRep)
modello4_ml = lmer(relazione4, data = pcr, REML = FALSE)
summary(modello4_ml)

AIC(modello3_ml, modello4_ml)

AIC(modello1_ml, modello3_ml)


####################################################
#
#  soluzione proposta dal gruppo giallo
#
####################################################



#Espressione genica rispetto alla linea cellulare
ipotesi1 = (CtGene - CtHK) ~ Well
modello1 = lm(ipotesi1)
summary(modello1)

#Espressione genica rispetto al gene target + interazione con la linea cellulare
ipotesi2 = (CtGene - CtHK) ~ TargetName * Well
modello2 = lm(ipotesi2)
summary(modello2)


AIC(modello1, modello2)

# il modello 2 risulta essere migliore secondo il criterio di AIC

library(lme4)

#Espressione genica rispetto al gene target con effetto di replica biologica
#e replica tecnica della misurazione

ipotesi3 = (CtGene - CtHK) ~ TargetName + (1|BiolRep) + (1|TechRep)
modello3 = lmer(ipotesi3, REML = FALSE)
summary(modello3)

AIC(modello2, modello3)


#il modello 2 risulra essere migliore secondo il criterio di AIC
#anche in confronto al modello 3 con random effect






####################################################
#
#  soluzione proposta da massimo 
#
####################################################


DeltaCt = CtGene - CtHK


mod <- lmer(DeltaCt ~ TargetName + (1|Well/BiolRep), data = pcr)
summary(mod)

# La sintassi (1|Well/BiolRep) crea un modello con:
# Random intercept per ciascun Well
# Random intercept per ciascuna BiolRep all'interno ("nested") di Well

# altro modo di scrivere:

mod <- lmer(DeltaCt ~ TargetName + (1 | Well) + (1 | Well:BiolRep), data = pcr)
summary(mod)


### miglioriamo:


meglio <- lmer(DeltaCt ~ TargetName + (1|Well), data = pcr)
summary(meglio)

# Estrarre varianze
varcomp <- as.data.frame(VarCorr(meglio))
varcomp

# ICC: varianza gruppo / (var gruppo + var residuo)
icc <- varcomp$vcov[2] / sum(varcomp$vcov)
icc


#	Interpretazione
#	ICC< 0.05	Quasi tutta la variabilità è "interna"
#	ICC 0.05–0.20	Piccola dipendenza intra-cluster
#	ICC 0.20–0.40	Moderata
#	ICC > 0.40	Alta dipendenza intra-cluster

# ICC  = 78% significa che molta variabilità è spiegata dai pozzetti (GS14p28, iPSCLp6/67, iPSCSLEp53)







####################################################
#
#  la questione dei multiple comparison
#
####################################################


table(TargetName)
summary(meglio)$coef


## 1.  ?? forse CHCHD2 e IDO1 non differiscono in senso statistico ??


TargetName_1 = TargetName
levels(TargetName_1)


levels(TargetName_1)[1]

levels(TargetName_1)[1] = "CHCHD2_&_ID01"
levels(TargetName_1)
levels(TargetName_1)[3] = "CHCHD2_&_ID01"
levels(TargetName_1)


meglio_ML = lmer(DeltaCt ~ TargetName + (1|Well), data = pcr, REML = FALSE)
prova1_ML = lmer(DeltaCt ~ TargetName_1 + (1|Well), data = pcr, REML = FALSE)
AIC(meglio_ML, prova1_ML)

### ottime notizie, tratteniamo , CHCHD2 ed ID01 si equivalgono in senso statistico

meglio1 = lmer(DeltaCt ~ TargetName_1 + (1|Well), data = pcr)
summary(meglio1)
table(TargetName_1)




## 2.  ?? forse EPH4, invece,  differisce  (t = 3.7) da CHCHD2 e IDO1 ??



TargetName_2 = TargetName_1
levels(TargetName_2)


levels(TargetName_2)[1] = "CHCHD2_&_ID01_&_EPHA4"
levels(TargetName_2)

levels(TargetName_2)[2] = "CHCHD2_&_ID01_&_EPHA4"
levels(TargetName_2)


meglio1_ML = lmer(DeltaCt ~ TargetName_1 + (1|Well), data = pcr, REML = FALSE)
prova2_ML = lmer(DeltaCt ~ TargetName_2 + (1|Well), data = pcr, REML = FALSE)
AIC(meglio1_ML, prova2_ML)

## come pensavamo, brutta idea quella di fare CHCHD2_&_ID01_&_EPHA4


modellodapubblicare = lmer(DeltaCt ~ TargetName_1 + (1|Well), data = pcr)
summary(modellodapubblicare)


