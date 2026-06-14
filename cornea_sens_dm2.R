# =============================================================================
# SENSIBILIDAD CORNEAL EN PACIENTES CON DM2 — SCRIPT DE PUBLICACIÓN
# Reproduce TODOS los cálculos del manuscrito (versión final).
#
# Dr. Wainer Manuel Sena Rivas
# Servicio de Oftalmología, INDEN — Hospital Escuela Dr. Jorge Abraham Hazoury Bahlés
# Universidad Iberoamericana (UNIBE), Santo Domingo, República Dominicana
# Correo: w.sena@prof.unibe.edu.do | ORCID: 0009-0006-4261-9241
# Repo: wmsenar/tesis-oftalmologia-2026 | 2026
#
# NOTA: este script está armonizado con el artículo final.
#   - Modelo PRINCIPAL: 3 predictores (tiempo, NPD, HTA); IC de perfil; AIC 263.2
#   - Modelo de SENSIBILIDAD: 7 predictores; LRT vs principal p=0.336; AIC 266.7
#   - Modelo PPO (sección 10): análisis de sensibilidad para la violación del
#     supuesto de Brant en el tiempo de evolución; ajustado con clm (ordinal pkg)
# =============================================================================

# ---- Paquetes ----
# install.packages(c("MASS","tidyverse","janitor","binom","psych","pwr","brant"))
library(MASS)        # polr
library(tidyverse)   # dplyr, ggplot2, stringr, etc.
library(janitor)     # clean_names
library(lubridate)   # dmy
library(binom)       # binom.confint (IC Wilson)
library(psych)       # cohen.kappa
library(pwr)         # pwr.r.test (referencia Pearson)
library(brant)       # prueba de Brant

set.seed(42)


# =============================================================================
# 1. CARGA Y LIMPIEZA
# =============================================================================
df <- read_csv("base_sensibilidad_corneal.csv") %>%
  clean_names() %>%
  mutate(across(where(is.character), str_trim)) %>%
  mutate(across(where(is.character), ~ case_when(
    .x %in% c("si","sí","SI","Sí","SÍ") ~ "Si",
    .x %in% c("no","NO","No")           ~ "NO",
    TRUE                                ~ .x)))

# ---- Conversión de variables ----
df <- df %>%
  mutate(
    edad                            = as.numeric(edad),
    tiempo_de_evolucion_de_diabetes = as.numeric(tiempo_de_evolucion_de_diabetes),
    hb_a1c_num                      = as.numeric(gsub("%", "", hb_a1c)),
    glucemia_num                    = as.numeric(str_trim(gsub("mg/dL|mg/dl|/dL|/dl", "", glucemia_reciente))),
    sexo                            = factor(sexo),
    hipertension_arterial           = factor(hipertension_arterial,           levels = c("NO","Si")),
    neuropatia_periferica_diabetica = factor(neuropatia_periferica_diabetica, levels = c("NO","Si")),
    central_od                      = as.numeric(central_od),
    central_os                      = as.numeric(central_os)
  )

# ---- Variable binocular: PEOR OJO (mínimo) y desenlace ordinal ----
# Codificación por ojo: 0 = severa | 1 = moderada | 2 = leve | 3 = normal
df <- df %>%
  mutate(
    sensibilidad_minima = pmin(central_od, central_os, na.rm = TRUE),
    severidad_sensibilidad = factor(case_when(
      sensibilidad_minima == 3 ~ "Normal",
      sensibilidad_minima == 2 ~ "Leve",
      sensibilidad_minima == 1 ~ "Moderada",
      sensibilidad_minima == 0 ~ "Severa"),
      levels = c("Normal","Leve","Moderada","Severa"), ordered = TRUE),
    sensibilidad_alterada = factor(case_when(
      sensibilidad_minima == 3 ~ "NO",
      sensibilidad_minima  < 3 ~ "Si"),
      levels = c("NO","Si"))
  )

N <- nrow(df)
cat("\n== Distribución de severidad ==\n"); print(table(df$severidad_sensibilidad))


# =============================================================================
# 2. PREVALENCIA DE ALTERACIÓN CORNEAL + IC 95% (Wilson)
# =============================================================================
x_alt <- sum(df$sensibilidad_alterada == "Si")
ic_prev <- binom.confint(x_alt, N, conf.level = 0.95, methods = "wilson")
cat(sprintf("\n== Prevalencia: %.1f%%  IC95%% [%.1f, %.1f] ==\n",
            100*ic_prev$mean, 100*ic_prev$lower, 100*ic_prev$upper))
# esperado: 60.2%  [51.0, 68.7]


# =============================================================================
# 3. NORMALIDAD (Shapiro-Wilk) Y CORRELACIONES (Spearman)
# =============================================================================
sw <- sapply(df[c("edad","hb_a1c_num","glucemia_num","tiempo_de_evolucion_de_diabetes")],
             function(x) shapiro.test(x)$p.value)
cat("\n== Shapiro-Wilk (p) ==\n"); print(round(sw, 4))

sev_num <- as.numeric(df$severidad_sensibilidad)
cor_tiempo <- cor.test(df$tiempo_de_evolucion_de_diabetes, sev_num, method = "spearman")
cor_hba1c  <- cor.test(df$hb_a1c_num,   sev_num, method = "spearman")
cor_gluc   <- cor.test(df$glucemia_num, sev_num, method = "spearman")
cat(sprintf("\n== Spearman vs severidad ==\nTiempo : rho=%.3f p=%.4f\nHbA1c  : rho=%.3f p=%.4f\nGlucemia: rho=%.3f p=%.4f\n",
            cor_tiempo$estimate, cor_tiempo$p.value,
            cor_hba1c$estimate,  cor_hba1c$p.value,
            cor_gluc$estimate,   cor_gluc$p.value))
# esperado: tiempo rho=0.381 p<0.001 | HbA1c rho=0.085 p=0.373 | glucemia rho=0.053 p=0.577


# =============================================================================
# 4. ANÁLISIS DE SENSIBILIDAD — MÍNIMO EFECTO DETECTABLE (MDE)
#    Reemplaza la potencia post-hoc (circular). Reporta el efecto más pequeño
#    detectable con la muestra disponible. NO usa el efecto observado.
#
#    METODO PRINCIPAL: ajuste de Spearman (Bonett & Wright, 2000), apropiado
#    porque la correlación del estudio es de Spearman.  SE^2 = (1 + rho^2/2)/(n-3)
#    Se imprime también pwr.r.test (Pearson) solo como referencia.
# =============================================================================
n_mde <- sum(complete.cases(df$tiempo_de_evolucion_de_diabetes, sev_num))
alpha <- 0.05

mde_spearman <- function(power, n, alpha, iter = 100) {
  za <- qnorm(1 - alpha/2); zp <- qnorm(power); r <- 0.30
  for (i in seq_len(iter)) r <- tanh((za + zp) * sqrt((1 + r^2/2)/(n - 3)))
  r
}
mde_sp_80 <- mde_spearman(0.80, n_mde, alpha)
mde_sp_90 <- mde_spearman(0.90, n_mde, alpha)
mde_pe_80 <- pwr.r.test(n = n_mde, sig.level = alpha, power = 0.80)$r
mde_pe_90 <- pwr.r.test(n = n_mde, sig.level = alpha, power = 0.90)$r

cat(sprintf("\n== MDE (n=%d, alfa=%.2f bilateral) ==\n", n_mde, alpha))
cat(sprintf("Spearman (Bonett-Wright) [PRINCIPAL]: 80%%=%.3f | 90%%=%.3f\n", mde_sp_80, mde_sp_90))
cat(sprintf("Pearson  (pwr.r.test)    [referencia]: 80%%=%.3f | 90%%=%.3f\n", mde_pe_80, mde_pe_90))
cat(sprintf("Efecto observado (rho=%.3f) supera el MDE 90%%: %s\n",
            cor_tiempo$estimate, ifelse(cor_tiempo$estimate > mde_sp_90, "SI", "NO")))
# esperado (Spearman): 80% = 0.265 | 90% = 0.306  -> ESTOS van en el artículo
# --> Reportar en el artículo el valor del método de Spearman (3 decimales).


# =============================================================================
# 5. CONCORDANCIA INTEROCULAR (kappa de Cohen) — justifica el "peor ojo"
# =============================================================================
ck <- cohen.kappa(cbind(df$central_od, df$central_os))
n_concord <- sum(df$central_od == df$central_os)
n_discord <- N - n_concord
cat(sprintf("\n== Concordancia interocular ==\nKappa (no ponderado): %.3f\nAcuerdo bruto: %.1f%% (%d/%d)\nDiscordancia: %.1f%% (%d)\n",
            ck$kappa, 100*n_concord/N, n_concord, N, 100*n_discord/N, n_discord))
# esperado: kappa 0.536 | acuerdo 69.9% (79/113) | discordancia 30.1% (34)


# =============================================================================
# 6. ANÁLISIS BIVARIADO (normal vs alterada)
# =============================================================================
cat("\n== Mann-Whitney (tiempo ~ alterada) ==\n")
print(wilcox.test(tiempo_de_evolucion_de_diabetes ~ sensibilidad_alterada, data = df))

fisher_npd <- fisher.test(table(df$sensibilidad_alterada, df$neuropatia_periferica_diabetica))
cat(sprintf("\n== Fisher NPD vs alteración == OR=%.2f IC95%% [%.2f, %.2f] p=%.4f\n",
            fisher_npd$estimate, fisher_npd$conf.int[1], fisher_npd$conf.int[2], fisher_npd$p.value))
# esperado: OR bivariado NPD ~ 3.83 [1.47, 10.97]


# =============================================================================
# 7. REGRESIÓN LOGÍSTICA ORDINAL — MODELO PRINCIPAL (parsimonioso, 3 var)
# =============================================================================
m_principal <- polr(
  severidad_sensibilidad ~ tiempo_de_evolucion_de_diabetes +
    neuropatia_periferica_diabetica + hipertension_arterial,
  data = df, Hess = TRUE)

ct  <- coef(summary(m_principal))
pv  <- pnorm(abs(ct[, "t value"]), lower.tail = FALSE) * 2
or  <- exp(coef(m_principal))
ci  <- exp(confint(m_principal))          # IC de perfil (profile)
tabla_principal <- round(cbind(OR = or, ci, p = pv[names(or)]), 3)
cat("\n== MODELO PRINCIPAL (3 var; AIC =", round(AIC(m_principal),1), ") ==\n")
print(tabla_principal)
# esperado: tiempo 1.084 (1.033-1.140) p=0.001 | NPD 2.892 (1.351-6.319) p=0.007
#           HTA 2.064 (0.980-4.442) p=0.059 [marginal] | AIC 263.2


# =============================================================================
# 8. MODELO DE SENSIBILIDAD (7 var) + LRT
# =============================================================================
m_sensib <- polr(
  severidad_sensibilidad ~ tiempo_de_evolucion_de_diabetes +
    neuropatia_periferica_diabetica + hipertension_arterial +
    hb_a1c_num + glucemia_num + sexo + edad,
  data = df, Hess = TRUE)

ct7 <- coef(summary(m_sensib))
pv7 <- pnorm(abs(ct7[, "t value"]), lower.tail = FALSE) * 2
or7 <- exp(cbind(OR = coef(m_sensib), confint(m_sensib)))
cat("\n== MODELO SENSIBILIDAD (7 var; AIC =", round(AIC(m_sensib),1), ") ==\n")
print(round(cbind(or7, p = pv7[rownames(or7)]), 3))
cat(sprintf("\nLRT principal vs sensibilidad: "))
print(anova(m_principal, m_sensib))
# esperado: AIC 266.7 | LRT p = 0.336 (las 4 variables extra no aportan)


# =============================================================================
# 9. SUPUESTO DE ODDS PROPORCIONALES — Brant SOBRE EL MODELO PRINCIPAL
# =============================================================================
cat("\n== Prueba de Brant (modelo principal) ==\n")
print(brant(m_principal))
# Nota: aviso de celda vacía esperado (n pequeño en 'Severa'). El tiempo viola
# el supuesto de forma robusta -> su OR se interpreta como efecto PROMEDIO a
# través de umbrales (declarado como limitación en el artículo).


# =============================================================================
# 10. ANÁLISIS DE SENSIBILIDAD — ODDS PROPORCIONALES PARCIALES (clm)
#     La prueba de Brant detectó violación para el tiempo de evolución.
#     Se ajusta un PPO con clm (paquete 'ordinal') que permite al tiempo
#     tener un coeficiente distinto en cada umbral (nominal=).
#     Si logLik del PPO NO es NaN, el modelo convergió y los resultados
#     se reportan como análisis de sensibilidad (Tabla 5 del artículo).
# =============================================================================
library(ordinal)

clm_po <- clm(
  severidad_sensibilidad ~
    tiempo_de_evolucion_de_diabetes + neuropatia_periferica_diabetica +
    hipertension_arterial,
  data = df
)

clm_ppo <- clm(
  severidad_sensibilidad ~
    neuropatia_periferica_diabetica + hipertension_arterial,
  nominal = ~ tiempo_de_evolucion_de_diabetes,
  data = df
)

cat("\n== Convergencia PPO ==\n")
cat("clm_po  logLik:", logLik(clm_po),  "\n")
cat("clm_ppo logLik:", logLik(clm_ppo), "\n")

cat("\n== AIC ==\n")
cat("PO completo:", round(AIC(clm_po),  1),
    "| PO parcial:", round(AIC(clm_ppo), 1), "\n")

cat("\n== LRT PO vs PPO ==\n")
print(anova(clm_po, clm_ppo))

co_ppo <- coef(summary(clm_ppo))
idx    <- grep("tiempo_de_evolucion", rownames(co_ppo))
tabla_tiempo_umbral <- data.frame(
  Umbral  = c("Normal vs ≥Leve", "≤Leve vs ≥Moderada", "≤Moderada vs Severa"),
  OR      = round(exp(-co_ppo[idx, "Estimate"]), 3),
  IC_low  = round(exp(-co_ppo[idx, "Estimate"] - 1.96 * co_ppo[idx, "Std. Error"]), 3),
  IC_high = round(exp(-co_ppo[idx, "Estimate"] + 1.96 * co_ppo[idx, "Std. Error"]), 3),
  p       = round(co_ppo[idx, "Pr(>|z|)"], 4)
)
cat("\n== OR del tiempo por umbral (PPO) ==\n")
print(tabla_tiempo_umbral, row.names = FALSE)

write.csv(tabla_tiempo_umbral, "resultados/or_tiempo_ppo_clm.csv", row.names = FALSE)


# =============================================================================
# 11. SESSION INFO (reproducibilidad)
# =============================================================================
cat("\n== sessionInfo ==\n"); print(sessionInfo())
