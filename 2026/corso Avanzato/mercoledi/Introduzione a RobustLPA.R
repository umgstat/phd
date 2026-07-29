# === Introduzione ====
# Laboratorio di Latent Profile Analysis (LPA)
# Dalla simulazione dei dati neuropsicologici all'LPA Robusta

rm(list = ls())

# === Step 1 ====
# CARICAMENTO LIBRERIE

library(MASS)       # Per simulazioni multivariate
library(e1071)      # Per calcolare la skewness (asimmetria)
library(ggplot2)    # Per i grafici avanzati
library(tidyr)      # Per la manipolazione dei dati (pivot_longer)
library(dplyr)      # Per la gestione del dataset
library(pheatmap)   # Per le heatmap dei profili
library(tidyLPA)    # Per l'LPA classica
library(RobustLPA)  # Per l'LPA robusta

set.seed(42) # Fissiamo il seed per la riproducibilità


# === Step 2 ====
# SIMULAZIONE DATI (I 5 TEST NEUROPSICOLOGICI)

# Definisco il numero di soggetti per i due profili latenti reali (Sani e Patologici)
N_sani <- 150
N_pat  <- 100
N_tot  <- N_sani + N_pat

# Matrice di correlazione teorica tra i test
cor_mat <- matrix(c(
  1.00,  0.55,  0.60, -0.45, -0.50,
  0.55,  1.00,  0.65, -0.50, -0.45,
  0.60,  0.65,  1.00, -0.40, -0.40,
  -0.45, -0.50, -0.40,  1.00,  0.70,
  -0.50, -0.45, -0.40,  0.70,  1.00
), nrow = 5, ncol = 5)

# Parametri PROFILO SANO (Accuratezza alta, RT bassi)
mu_sani  <- c(85, 82, 88, 6.1, 6.3) 
sd_sani  <- c(8, 7, 9, 0.15, 0.20)
cov_sani <- diag(sd_sani) %*% cor_mat %*% diag(sd_sani)

# Parametri PROFILO PATOLOGICO (Popolazione "pulita")
mu_pat  <- c(60, 58, 65, 6.6, 6.8) 
sd_pat  <- c(12, 11, 10, 0.25, 0.30) 
cov_pat <- diag(sd_pat) %*% cor_mat %*% diag(sd_pat)

# Generazione Dati Multivariati Base
dati_sani <- mvrnorm(N_sani, mu = mu_sani, Sigma = cov_sani)
dati_pat  <- mvrnorm(N_pat,  mu = mu_pat,  Sigma = cov_pat)

dataset_lpa <- as.data.frame(rbind(dati_sani, dati_pat))
colnames(dataset_lpa) <- c("Memoria", "Attenzione", "Funzioni_Esecutive", "RT_Stroop", "RT_TMT")

# Trasformazione Esponenziale dei RT 
dataset_lpa$RT_Stroop <- round(exp(dataset_lpa$RT_Stroop))
dataset_lpa$RT_TMT    <- round(exp(dataset_lpa$RT_TMT))

# --- OUTLIER ISOLATI (Il motore per far brillare RobustLPA) ---
# Inietto un 5% di outlier severi ma CLINICAMENTE PLAUSIBILI (code lunghe).
n_outliers   <- round(N_pat * 0.05) 
idx_outliers <- (N_sani + 1):(N_sani + n_outliers)

# Abbasso i valori per attaccarli alla coda della distribuzione principale
dataset_lpa$RT_Stroop[idx_outliers] <- round(runif(n_outliers, min = 1400, max = 1900))
dataset_lpa$RT_TMT[idx_outliers]    <- round(runif(n_outliers, min = 1800, max = 2400))

# Aggiungo identificatori (ID e Profilo Reale)
dataset_lpa$Profilo_Reale <- c(rep("Sano", N_sani), rep("Patologico", N_pat))
dataset_lpa$ID            <- 1:N_tot

# Riordino le colonne
dataset_lpa <- dataset_lpa[, c("ID", "Profilo_Reale", "Memoria", "Attenzione", "Funzioni_Esecutive", "RT_Stroop", "RT_TMT")]


# === Step 3 ====
# ESPLORAZIONE VISIVA E VERIFICA ASSUNTI

# Heatmap dei pattern di risposta
pheatmap(
  mat = dataset_lpa[, 3:7],
  cluster_cols = FALSE,
  scale = "column",
  cluster_rows = TRUE,
  show_rownames = FALSE,
  color = colorRampPalette(c("#2166AC", "white", "#B2182B"))(50),
  main = "Pattern di risposta individuali"
)

# Verifica statistica asimmetria
cat("Asimmetria RT Stroop:", skewness(dataset_lpa$RT_Stroop), "\n")
cat("Asimmetria RT TMT:", skewness(dataset_lpa$RT_TMT), "\n")

# Grafico delle distribuzioni
df_plot <- pivot_longer(dataset_lpa, cols = c("Memoria", "RT_Stroop"), names_to = "Variabile", values_to = "Valore")

ggplot(df_plot, aes(x = Valore, fill = Profilo_Reale)) +
  geom_density(alpha = 0.6) +
  facet_wrap(~ Variabile, scales = "free") +
  scale_fill_manual(values = c("Sano" = "#2166AC", "Patologico" = "#B2182B")) +
  theme_minimal(base_size = 14) +
  labs(
    title = "Distribuzione delle Variabili per Profilo Latente",
    subtitle = "Presenza di asimmetria e outlier isolati nei Tempi di Reazione",
    x = "Punteggio / Millisecondi",
    y = "Densità",
    fill = "Profilo Reale"
  ) +
  theme(legend.position = "bottom")


# === Step 4 ====
# PREPARAZIONE DATI E LPA CLASSICA (tidyLPA)

# Standardizzazione classica delle variabili
dati_per_lpa <- dataset_lpa %>%
  select(Memoria, Attenzione, Funzioni_Esecutive, RT_Stroop, RT_TMT) %>%
  scale()

# Esecuzione LPA classica 
# Riduciamo n_profiles a 1:2 per evitare il crash del Modello 6 per mancanza di dati
lpa_models <- dati_per_lpa %>%
  estimate_profiles(n_profiles = 1:2, models = c(1, 2, 3, 6))

# Tabella di Fit 
# Usiamo select di dplyr (molto più sicuro delle parentesi quadre base R)
tabella_fit <- get_fit(lpa_models) %>%
  select(Model, Classes, parameters, n, AIC, BIC, SABIC, Entropy, prob_min, prob_max)

print(tabella_fit)

# Estrazione sicura del modello selezionato (Modello 6, 2 Profili)
# Senza usare indici numerici a rischio rottura
best_model_estimates <- get_estimates(lpa_models) %>%
  filter(Model == 6, Classes == 2)

# Grafico dei Profili tidyLPA
plot_data <- best_model_estimates %>%
  filter(Category == "Means") %>%
  mutate(
    ci_lower = Estimate - (1.96 * se),
    ci_upper = Estimate + (1.96 * se),
    Class = as.factor(Class)
  )

ggplot(plot_data, aes(x = Parameter, y = Estimate, color = Class, group = Class)) +
  geom_errorbar(aes(ymin = ci_lower, ymax = ci_upper), width = 0.15, position = position_dodge(width = 0.4), linewidth = 0.8) +
  geom_point(position = position_dodge(width = 0.4), size = 3.5) +
  geom_line(position = position_dodge(width = 0.4), alpha = 0.4, linewidth = 1) +
  theme_minimal(base_size = 14) +
  labs(
    title = "Profili Latenti Identificati (tidyLPA)",
    subtitle = "Medie stimate e Intervalli di Confidenza (95%)",
    x = "Variabili Osservate",
    y = "Media Stimata (z-score)",
    color = "Profilo Stimato"
  ) +
  theme(legend.position = "bottom", panel.grid.major.x = element_blank())


# === Step 5 ====
# IL PROBLEMA DEGLI OUTLIER E L'INTRODUZIONE AI PESI DI HUBER

rt_stroop_pat <- dataset_lpa$RT_Stroop[dataset_lpa$Profilo_Reale == "Patologico"]
media_classica <- mean(rt_stroop_pat)
media_robusta  <- MASS::huber(rt_stroop_pat)$mu

grafico_confronto <- ggplot(data = data.frame(RT_Stroop = rt_stroop_pat), aes(x = RT_Stroop)) +
  geom_histogram(fill = "lightgray", color = "darkgray", bins = 25, alpha = 0.8) +
  geom_vline(xintercept = media_classica, color = "#D55E00", linetype = "dashed", linewidth = 1.2) +
  geom_vline(xintercept = media_robusta, color = "#0072B2", linetype = "solid", linewidth = 1.2) +
  annotate("text", x = media_classica, y = Inf, label = paste("Media Classica:", round(media_classica)), 
           color = "#D55E00", vjust = 2, hjust = -0.05, fontface = "bold") +
  annotate("text", x = media_robusta, y = Inf, label = paste("Media Huber:", round(media_robusta)), 
           color = "#0072B2", vjust = 4, hjust = 1.05, fontface = "bold") +
  theme_minimal(base_size = 14) +
  labs(
    title = "Resistenza agli Outlier: Media Classica vs Media Huber",
    subtitle = "Tempi di Reazione Stroop nel Gruppo Patologico",
    x = "RT Stroop (ms)",
    y = "Frequenza"
  ) +
  theme(plot.title = element_text(face = "bold", size = 16), panel.grid.minor = element_blank())

print(grafico_confronto)


# === Step 6 ====
# ESECUZIONE RobustLPA

robust_fit <- estimate_profiles_robust(data = dati_per_lpa, n_profiles = 1:4, models = 1:6)
print(round(robust_fit$fit_table, 2))


# === Step 7 ====
# CONFRONTO PARAMETRI TEORICI DELLA POPOLAZIONE VS STIME (tidyLPA vs RobustLPA)

# PARAMETRI TEORICI DELLA POPOLAZIONE (La baseline prima dell'inquinamento)
sd_ln_pat  <- sqrt((exp(sd_pat[4:5]^2) - 1) * exp(2 * mu_pat[4:5] + sd_pat[4:5]^2))
sd_ln_sani <- sqrt((exp(sd_sani[4:5]^2) - 1) * exp(2 * mu_sani[4:5] + sd_sani[4:5]^2))

reali_popolazione <- data.frame(
  Profilo = rep(c("Patologico", "Sano"), each = 5),
  Variabile = rep(c("Memoria", "Attenzione", "Funzioni_Esecutive", "RT_Stroop", "RT_TMT"), 2),
  Media_Teorica = c(
    mu_pat[1:3],  round(exp(mu_pat[4:5] + (sd_pat[4:5]^2)/2)),  
    mu_sani[1:3], round(exp(mu_sani[4:5] + (sd_sani[4:5]^2)/2))  
  ),
  SD_Teorica = c(
    sd_pat[1:3],  round(sd_ln_pat), 
    sd_sani[1:3], round(sd_ln_sani)
  )
)

means_center <- attr(dati_per_lpa, "scaled:center")
sds_scale    <- attr(dati_per_lpa, "scaled:scale")

# ESTRAZIONE E DE-STANDARDIZZAZIONE DA RobustLPA (Con Assegnazione Dinamica)
m6_p2 <- robust_fit$models$model_6_profiles_2
var_names <- c("Memoria", "Attenzione", "Funzioni_Esecutive", "RT_Stroop", "RT_TMT")

memoria_c1 <- m6_p2$means[[1]][1] * sds_scale["Memoria"] + means_center["Memoria"]
memoria_c2 <- m6_p2$means[[2]][1] * sds_scale["Memoria"] + means_center["Memoria"]

etichette_robust <- if(memoria_c1 > memoria_c2) c("Sano", "Patologico") else c("Patologico", "Sano")

estimates_robust <- data.frame(
  Profilo = rep(etichette_robust, each = 5),
  Variabile = rep(var_names, 2),
  Media_z = c(m6_p2$means[[1]], m6_p2$means[[2]]),
  SD_z    = c(sqrt(diag(m6_p2$covariances[[1]])), sqrt(diag(m6_p2$covariances[[2]])))
) %>%
  mutate(
    Media_RobustLPA = Media_z * sds_scale[Variabile] + means_center[Variabile],
    SD_RobustLPA    = SD_z * sds_scale[Variabile]
  ) %>%
  select(Profilo, Variabile, Media_RobustLPA, SD_RobustLPA)

# ESTRAZIONE E DE-STANDARDIZZAZIONE DA tidyLPA (Con Assegnazione Dinamica)
estimates_tidy <- get_estimates(lpa_models) %>%
  filter(Model == 6, Classes == 2) %>%
  filter(Category %in% c("Means", "Variances")) %>%
  mutate(
    Category = ifelse(Category == "Means", "Media", "SD"),
    Estimate = ifelse(Category == "SD", sqrt(Estimate), Estimate)
  ) %>%
  select(Class, Parameter, Category, Estimate) %>%
  pivot_wider(names_from = Category, values_from = Estimate) %>%
  rename(Profilo = Class, Variabile = Parameter) 

memoria_tidy_c1 <- estimates_tidy$Media[estimates_tidy$Profilo == 1 & estimates_tidy$Variabile == "Memoria"]
memoria_tidy_c2 <- estimates_tidy$Media[estimates_tidy$Profilo == 2 & estimates_tidy$Variabile == "Memoria"]

mem_t_c1_real <- memoria_tidy_c1 * sds_scale["Memoria"] + means_center["Memoria"]
mem_t_c2_real <- memoria_tidy_c2 * sds_scale["Memoria"] + means_center["Memoria"]

estimates_tidy <- estimates_tidy %>%
  mutate(Profilo = case_when(
    Profilo == 1 & mem_t_c1_real > mem_t_c2_real ~ "Sano",
    Profilo == 1 & mem_t_c1_real <= mem_t_c2_real ~ "Patologico",
    Profilo == 2 & mem_t_c2_real > mem_t_c1_real ~ "Sano",
    Profilo == 2 & mem_t_c2_real <= mem_t_c1_real ~ "Patologico"
  )) %>%
  mutate(
    Media_tidyLPA = Media * sds_scale[Variabile] + means_center[Variabile],
    SD_tidyLPA    = SD * sds_scale[Variabile]
  ) %>%
  select(Profilo, Variabile, Media_tidyLPA, SD_tidyLPA)

# TABELLA COMPARATIVA ED ERRORE ASSOLUTO
confronto_parametri <- reali_popolazione %>%
  inner_join(estimates_tidy, by = c("Profilo", "Variabile")) %>%
  inner_join(estimates_robust, by = c("Profilo", "Variabile")) %>%
  mutate(
    Bias_tidyLPA   = abs(Media_Teorica - Media_tidyLPA),
    Bias_RobustLPA = abs(Media_Teorica - Media_RobustLPA)
  ) %>%
  mutate(across(where(is.numeric), ~round(.x, 2)))

print(as.data.frame(confronto_parametri))

# GRAFICO DIMOSTRATIVO
df_grafico_errore <- confronto_parametri %>%
  select(Profilo, Variabile, Bias_tidyLPA, Bias_RobustLPA) %>%
  pivot_longer(cols = starts_with("Bias"), names_to = "Pacchetto", values_to = "Errore_Assoluto") %>%
  mutate(Pacchetto = ifelse(grepl("tidyLPA", Pacchetto), "tidyLPA (Classico)", "RobustLPA (Robusto)"))

ggplot(df_grafico_errore, aes(x = Variabile, y = Errore_Assoluto, fill = Pacchetto)) +
  geom_bar(stat = "identity", position = "dodge", width = 0.7) +
  facet_wrap(~ Profilo) +
  scale_fill_manual(values = c("tidyLPA (Classico)" = "#D55E00", "RobustLPA (Robusto)" = "#0072B2")) +
  theme_minimal(base_size = 14) +
  labs(
    title = "Accuratezza della Stima rispetto ai Parametri Teorici",
    subtitle = "RobustLPA abbatte la distorsione sui Tempi di Reazione asimmetrici",
    x = "Test Neuropsicologico",
    y = "Errore Assoluto |Media Teorica - Media Stimata|",
    fill = "Algoritmo"
  ) +
  theme(
    legend.position = "bottom",
    axis.text.x = element_text(angle = 45, hjust = 1),
    panel.grid.minor = element_blank()
  )
