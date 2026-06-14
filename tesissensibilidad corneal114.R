# =====================================================
# TESIS: SENSIBILIDAD CORNEAL EN PACIENTES CON DM2
#
# Dr. Wainer Manuel Sena Rivas
# Servicio de Oftalmología, INDEN — Hospital Escuela Dr. Jorge Abraham Hazoury Bahlés
# Universidad Iberoamericana (UNIBE), Santo Domingo, República Dominicana
# Correo: w.sena@prof.unibe.edu.do | ORCID: 0009-0006-4261-9241
# 2026
# =====================================================




# =====================================================
# 1. PAQUETES
# =====================================================

library(MASS)
library(tidyverse)
library(janitor)
library(stringr)
library(lubridate)
library(naniar)
library(skimr)
library(gtsummary)
library(flextable)
library(officer)
library(writexl)
library(readxl)
library(broom)
library(ggplot2)
library(ggtext)
library(scales)
library(dplyr)
library(pwr)

# =====================================================
# 2. CARPETAS DE SALIDA
# =====================================================

dir.create("graficos",           showWarnings = FALSE)
dir.create("tablas",             showWarnings = FALSE)
dir.create("resultados",         showWarnings = FALSE)
dir.create("figuras/figuras 2",  showWarnings = FALSE, recursive = TRUE)


# =====================================================
# 3. CARGA Y LIMPIEZA INICIAL
# =====================================================

df <- read_csv("base_sensibilidad_corneal.csv") %>%
  clean_names() %>%
  mutate(across(where(is.character), str_trim)) %>%
  mutate(across(
    where(is.character),
    ~ case_when(
      .x %in% c("si", "sí", "SI", "Sí", "SÍ") ~ "Si",
      .x %in% c("no", "NO", "No")               ~ "NO",
      TRUE                                       ~ .x
    )
  ))


# =====================================================
# 4. CONVERSIÓN DE VARIABLES
# =====================================================

df <- df %>%
  mutate(
    edad                            = as.numeric(edad),
    fecha_de_evaluacion             = dmy(fecha_de_evaluacion),
    tiempo_de_evolucion_de_diabetes = as.numeric(tiempo_de_evolucion_de_diabetes),
    hb_a1c_num                      = as.numeric(gsub("%", "", hb_a1c)),
    glucemia_num                    = as.numeric(
      str_trim(gsub("mg/dL|mg/dl|/dL|/dl", "", glucemia_reciente))
    ),
    sexo                             = factor(sexo),
    tratamiento                      = factor(tratamiento),
    hipertension_arterial            = factor(hipertension_arterial,           levels = c("NO", "Si")),
    dislipidemia                     = factor(dislipidemia,                    levels = c("NO", "Si")),
    neuropatia_periferica_diabetica  = factor(neuropatia_periferica_diabetica, levels = c("NO", "Si")),
    nefropatia_diabetica             = factor(nefropatia_diabetica,            levels = c("NO", "Si")),
    tabaquismo                       = factor(tabaquismo,                      levels = c("NO", "Si")),
    retinopatia_diabetica_od         = factor(retinopatia_diabetica_od),
    retinopatia_diabetica_os         = factor(retinopatia_diabetica_os),
    central_od  = as.numeric(central_od),
    central_os  = as.numeric(central_os),
    pio_od      = as.numeric(pio_od),
    pio_os      = as.numeric(pio_os),
    but_od      = as.numeric(but_od),
    but_os      = as.numeric(but_os)
  )


# =====================================================
# 5. VARIABLES DERIVADAS
# =====================================================

df <- df %>%
  mutate(
    # Variable binocular: peor ojo (mínimo)
    sensibilidad_minima = pmin(central_od, central_os, na.rm = TRUE),
    
    severidad_sensibilidad = factor(
      case_when(
        sensibilidad_minima == 3 ~ "Normal",
        sensibilidad_minima == 2 ~ "Leve",
        sensibilidad_minima == 1 ~ "Moderada",
        sensibilidad_minima == 0 ~ "Severa"
      ),
      levels  = c("Normal", "Leve", "Moderada", "Severa"),
      ordered = TRUE
    ),
    
    sensibilidad_alterada = factor(
      case_when(
        sensibilidad_minima == 3 ~ "NO",
        sensibilidad_minima  < 3 ~ "Si"
      ),
      levels = c("NO", "Si")
    ),
    
    control_glucemico = factor(
      case_when(
        hb_a1c_num <  7                        ~ "Buen control",
        hb_a1c_num >= 7 & hb_a1c_num < 9      ~ "Control subóptimo",
        hb_a1c_num >= 9                        ~ "Mal control"
      ),
      levels  = c("Buen control", "Control subóptimo", "Mal control"),
      ordered = TRUE
    ),
    
    control_glucemico_hba1c = factor(
      case_when(
        hb_a1c_num <  7                        ~ "Controlado",
        hb_a1c_num >= 7 & hb_a1c_num < 9      ~ "Mal controlado",
        hb_a1c_num >= 9                        ~ "Muy mal controlado",
        TRUE                                   ~ NA_character_
      ),
      levels  = c("Controlado", "Mal controlado", "Muy mal controlado"),
      ordered = TRUE
    ),
    
    glucemia_categoria = factor(
      case_when(
        glucemia_num <  126                         ~ "Menor de 126 mg/dL",
        glucemia_num >= 126 & glucemia_num < 200    ~ "126-199 mg/dL",
        glucemia_num >= 200                         ~ "≥200 mg/dL"
      ),
      levels  = c("Menor de 126 mg/dL", "126-199 mg/dL", "≥200 mg/dL"),
      ordered = TRUE
    )
  )


# =====================================================
# 6. VALIDACIÓN INICIAL
# =====================================================

str(df)
summary(df)
colSums(is.na(df))
skim(df)
vis_miss(df)

table(df$severidad_sensibilidad)
table(df$sensibilidad_alterada)
table(df$control_glucemico_hba1c)
table(df$glucemia_categoria)


# =====================================================
# 7. EXPORTAR BASE LIMPIA
# =====================================================

write.csv(df, "resultados/base_sensibilidad_corneal_113_limpia.csv", row.names = FALSE)
write_xlsx(list("dataset_limpio" = df), "resultados/base_sensibilidad_corneal_113_limpia.xlsx")
saveRDS(df, "resultados/base_sensibilidad_corneal_113_limpia.rds")


# =====================================================
# 8. TABLA 1 — DESCRIPTIVA GENERAL
# =====================================================

tabla1 <- df %>%
  dplyr::select(
    edad, sexo, tiempo_de_evolucion_de_diabetes,
    hb_a1c_num, glucemia_num,
    hipertension_arterial, dislipidemia,
    neuropatia_periferica_diabetica, nefropatia_diabetica,
    severidad_sensibilidad
  ) %>%
  tbl_summary(
    statistic = list(
      all_continuous()  ~ "{mean} ± {sd}",
      all_categorical() ~ "{n} ({p}%)"
    ),
    digits  = all_continuous() ~ 1,
    missing = "no",
    label   = list(
      edad                           ~ "Edad (años)",
      sexo                           ~ "Sexo",
      tiempo_de_evolucion_de_diabetes ~ "Tiempo de evolución DM2 (años)",
      hb_a1c_num                     ~ "HbA1c (%)",
      glucemia_num                   ~ "Glucemia reciente (mg/dL)",
      hipertension_arterial          ~ "Hipertensión arterial",
      dislipidemia                   ~ "Dislipidemia",
      neuropatia_periferica_diabetica ~ "Neuropatía periférica diabética",
      nefropatia_diabetica           ~ "Nefropatía diabética",
      severidad_sensibilidad         ~ "Severidad de sensibilidad corneal"
    )
  ) %>%
  bold_labels()

tabla1

doc1 <- read_docx() %>%
  body_add_par("Tabla 1. Características generales de la muestra (N = 113)",
               style = "heading 1") %>%
  body_add_flextable(as_flex_table(tabla1))
print(doc1, target = "tablas/tabla1_descriptiva_general.docx")


# =====================================================
# 9. TABLA 2 — INFERENCIAL POR SEVERIDAD
# =====================================================

tabla2 <- df %>%
  dplyr::select(
    edad, hb_a1c_num, glucemia_num,
    tiempo_de_evolucion_de_diabetes,
    neuropatia_periferica_diabetica,
    severidad_sensibilidad
  ) %>%
  tbl_summary(
    by       = severidad_sensibilidad,
    statistic = list(
      all_continuous()  ~ "{mean} ± {sd}",
      all_categorical() ~ "{n} ({p}%)"
    ),
    digits  = all_continuous() ~ 1,
    missing = "no",
    label   = list(
      edad                           ~ "Edad (años)",
      hb_a1c_num                     ~ "HbA1c (%)",
      glucemia_num                   ~ "Glucemia reciente (mg/dL)",
      tiempo_de_evolucion_de_diabetes ~ "Tiempo de evolución DM2 (años)",
      neuropatia_periferica_diabetica ~ "Neuropatía periférica diabética"
    )
  ) %>%
  add_p() %>%
  bold_labels()

tabla2

doc2 <- read_docx() %>%
  body_add_par("Tabla 2. Variables clínicas según severidad de sensibilidad corneal",
               style = "heading 1") %>%
  body_add_flextable(as_flex_table(tabla2))
print(doc2, target = "tablas/tabla2_severidad_sensibilidad.docx")


# =====================================================
# 10. NORMALIDAD — SHAPIRO-WILK
# =====================================================

sink("resultados/shapiro_tests.txt")
tryCatch({
  cat("Shapiro-Wilk — edad:\n");             print(shapiro.test(df$edad))
  cat("Shapiro-Wilk — HbA1c:\n");            print(shapiro.test(df$hb_a1c_num))
  cat("Shapiro-Wilk — glucemia:\n");          print(shapiro.test(df$glucemia_num))
  cat("Shapiro-Wilk — tiempo evolución:\n"); print(shapiro.test(df$tiempo_de_evolucion_de_diabetes))
}, finally = sink())


# =====================================================
# 11. CORRELACIONES DE SPEARMAN
# =====================================================

sink("resultados/correlaciones_spearman.txt")
tryCatch({
  cat("Spearman — tiempo evolución vs severidad:\n")
  cor_tiempo <- cor.test(df$tiempo_de_evolucion_de_diabetes,
                         as.numeric(df$severidad_sensibilidad), method = "spearman")
  print(cor_tiempo)
  cat("\nSpearman — HbA1c vs severidad:\n")
  print(cor.test(df$hb_a1c_num,
                 as.numeric(df$severidad_sensibilidad), method = "spearman"))
  cat("\nSpearman — glucemia vs severidad:\n")
  print(cor.test(df$glucemia_num,
                 as.numeric(df$severidad_sensibilidad), method = "spearman"))
}, finally = sink())

# =====================================================
# 11b. ANÁLISIS DE SENSIBILIDAD — EFECTO MÍNIMO DETECTABLE (MDE)
#      Reemplaza el análisis de potencia post-hoc (circular).
# =====================================================

rho_obs <- as.numeric(cor_tiempo$estimate)        # del bloque 11
n_mde   <- sum(complete.cases(                     # n real de pares completos
  df$tiempo_de_evolucion_de_diabetes,
  as.numeric(df$severidad_sensibilidad)))
alpha   <- 0.05

# MDE para correlación: rho = tanh( (z_alpha/2 + z_pot) / sqrt(n-3) )
# Ajuste Spearman (Bonett & Wright, 2000): SE^2 = (1 + rho^2/2)/(n-3)
mde_spearman <- function(power, n, alpha, iter = 50) {
  za <- qnorm(1 - alpha / 2); zp <- qnorm(power)
  r <- 0.30
  for (i in seq_len(iter)) r <- tanh((za + zp) * sqrt((1 + r^2 / 2) / (n - 3)))
  r
}

mde_80 <- mde_spearman(0.80, n_mde, alpha)
mde_90 <- mde_spearman(0.90, n_mde, alpha)

sink("resultados/analisis_sensibilidad_mde.txt")
cat("ANÁLISIS DE SENSIBILIDAD (EFECTO MÍNIMO DETECTABLE)\n")
cat(sprintf("n = %d | alfa = %.2f (bilateral) | rho observado = %.3f\n\n",
            n_mde, alpha, rho_obs))
cat(sprintf("MDE potencia 80%%: rho = %.3f\n", mde_80))
cat(sprintf("MDE potencia 90%%: rho = %.3f\n", mde_90))
cat(sprintf("\nEl efecto observado (%.3f) %s el MDE en ambos niveles de potencia.\n",
            rho_obs, ifelse(rho_obs > mde_90, "supera", "NO supera")))
sink()


# =====================================================
# 12. COMPARACIÓN BINARIA — NORMAL VS ALTERADA
# =====================================================

sink("resultados/analisis_binario_sensibilidad.txt")
tryCatch({
  cat("Mann-Whitney — tiempo evolución:\n")
  print(wilcox.test(tiempo_de_evolucion_de_diabetes ~ sensibilidad_alterada, data = df))
  cat("\nMann-Whitney — HbA1c:\n")
  print(wilcox.test(hb_a1c_num ~ sensibilidad_alterada, data = df))
  cat("\nMann-Whitney — glucemia:\n")
  print(wilcox.test(glucemia_num ~ sensibilidad_alterada, data = df))
  cat("\nMann-Whitney — edad:\n")
  print(wilcox.test(edad ~ sensibilidad_alterada, data = df))
  cat("\nFisher exacto — NPD vs sensibilidad alterada:\n")
  print(fisher.test(table(df$sensibilidad_alterada, df$neuropatia_periferica_diabetica)))
}, finally = sink())

# OR bivariado NPD
fisher_npd    <- fisher.test(table(df$sensibilidad_alterada, df$neuropatia_periferica_diabetica))
p_fisher_npd  <- ifelse(fisher_npd$p.value < 0.001, "p < 0.001",
                        paste0("p = ", round(fisher_npd$p.value, 3)))
cat("Fisher NPD:", p_fisher_npd,
    "| OR =", round(fisher_npd$estimate, 2),
    "| IC 95%:", round(fisher_npd$conf.int[1], 2), "-", round(fisher_npd$conf.int[2], 2), "\n")


# =====================================================
# 13. REGRESIÓN LOGÍSTICA BINARIA
# =====================================================

modelo_binario <- glm(
  sensibilidad_alterada ~
    tiempo_de_evolucion_de_diabetes + hb_a1c_num + glucemia_num +
    neuropatia_periferica_diabetica + edad,
  data   = df,
  family = binomial
)

summary(modelo_binario)

or_binario <- exp(cbind(OR = coef(modelo_binario), confint.default(modelo_binario)))
round(or_binario, 3)

write.csv(or_binario, "resultados/or_modelo_logistico_binario.csv")

sink("resultados/modelo_logistico_binario.txt")
tryCatch({ summary(modelo_binario); or_binario }, finally = sink())


# =====================================================
# 14. REGRESIÓN LOGÍSTICA ORDINAL — 7 VARIABLES
#     Modelo final reconciliado con la tesis
# =====================================================

levels(df$severidad_sensibilidad)

modelo_ordinal <- polr(
  severidad_sensibilidad ~
    tiempo_de_evolucion_de_diabetes +
    neuropatia_periferica_diabetica +
    hipertension_arterial +
    hb_a1c_num +
    glucemia_num +
    sexo +
    edad,
  data = df,
  Hess = TRUE
)

summary(modelo_ordinal)

# Coeficientes con p-valores
coef_table <- coef(summary(modelo_ordinal))
p_values   <- pnorm(abs(coef_table[, "t value"]), lower.tail = FALSE) * 2
coef_table <- cbind(coef_table, "p value" = round(p_values, 4))
coef_table

# OR con IC 95%
or_ordinal <- exp(cbind(OR = coef(modelo_ordinal), confint.default(modelo_ordinal)))
round(or_ordinal, 3)

cat("\nAIC modelo ordinal 7 variables:", round(AIC(modelo_ordinal), 1), "\n")

# Exportar
write.csv(coef_table, "resultados/coeficientes_modelo_ordinal_7v.csv")
write.csv(or_ordinal, "resultados/or_modelo_ordinal_7v.csv")

sink("resultados/modelo_logistico_ordinal_7v.txt")
tryCatch({ summary(modelo_ordinal); coef_table; or_ordinal }, finally = sink())


# =====================================================
# 15. DIAGNÓSTICOS DEL MODELO ORDINAL
# =====================================================

# ── 15a. COMPARACIÓN DE MODELOS POR AIC ──────────
modelo_5v <- polr(
  severidad_sensibilidad ~
    tiempo_de_evolucion_de_diabetes + hb_a1c_num + glucemia_num +
    neuropatia_periferica_diabetica + edad,
  data = df, Hess = TRUE
)

modelo_reducido <- polr(
  severidad_sensibilidad ~
    tiempo_de_evolucion_de_diabetes + neuropatia_periferica_diabetica,
  data = df, Hess = TRUE
)

sink("resultados/comparacion_aic_modelos.txt")
cat("=== COMPARACIÓN DE MODELOS POR AIC ===\n")
cat("Modelo 5v (sin sexo, sin HTA):", round(AIC(modelo_5v),      1), "\n")
cat("Modelo 7v — MODELO FINAL:     ", round(AIC(modelo_ordinal), 1), "\n")
cat("Modelo reducido (tiempo+NPD): ", round(AIC(modelo_reducido),1), "\n")
cat("\nDelta AIC (reducido vs 7v):", round(AIC(modelo_reducido) - AIC(modelo_ordinal), 1), "\n")
sink()

cat("\n=== AIC ===\n")
cat("Modelo 5v:", round(AIC(modelo_5v),      1), "\n")
cat("Modelo 7v:", round(AIC(modelo_ordinal), 1), "\n")
cat("Reducido: ", round(AIC(modelo_reducido),1), "\n")


# ── 15b. TEST DE PROPORCIONALIDAD DE ODDS (Brant manual) ──
brant_manual <- function(model) {
  data_model <- model$model
  y          <- data_model[, 1]
  levels_y   <- levels(y)
  n_levels   <- length(levels_y)
  predictors <- names(coef(model))
  
  results <- data.frame(Variable = character(), chi2 = numeric(),
                        df = numeric(), p_valor = numeric(),
                        conclusion = character(), stringsAsFactors = FALSE)
  
  for (pred in predictors) {
    beta_ordinal <- coef(model)[pred]
    se_ordinal   <- sqrt(diag(vcov(model)))[pred]
    beta_bin     <- c()
    se_bin       <- c()
    
    for (k in 1:(n_levels - 1)) {
      y_bin    <- as.integer(as.numeric(y) > k)
      df_bin   <- data_model
      df_bin$y_bin <- y_bin
      formula_bin  <- as.formula(paste("y_bin ~", pred))
      tryCatch({
        mod_bin  <- glm(formula_bin, data = df_bin, family = binomial)
        beta_bin <- c(beta_bin, coef(mod_bin)[pred])
        se_bin   <- c(se_bin,   summary(mod_bin)$coefficients[pred, "Std. Error"])
      }, error = function(e) { beta_bin <<- c(beta_bin, NA); se_bin <<- c(se_bin, NA) })
    }
    
    valid <- !is.na(beta_bin)
    if (sum(valid) >= 2) {
      diffs  <- beta_bin[valid] - beta_ordinal
      vars   <- se_bin[valid]^2 + se_ordinal^2
      chi2   <- sum(diffs^2 / vars)
      df_chi <- sum(valid) - 1
      p_val  <- pchisq(chi2, df = df_chi, lower.tail = FALSE)
      results <- rbind(results, data.frame(
        Variable   = pred,
        chi2       = round(chi2, 3),
        df         = df_chi,
        p_valor    = round(p_val, 4),
        conclusion = ifelse(p_val < 0.05, "VIOLACION (p<0.05)", "Cumple"),
        stringsAsFactors = FALSE
      ))
    }
  }
  return(results)
}

brant_results <- brant_manual(modelo_ordinal)

sink("resultados/test_brant_proporcionalidad.txt")
cat("=== TEST DE PROPORCIONALIDAD DE ODDS (Brant manual) ===\n")
cat("H0: el efecto del predictor es el mismo en todos los umbrales\n")
cat("p < 0.05 indica VIOLACION del supuesto\n\n")
print(brant_results, row.names = FALSE)
cat("\nVariables con posible violación:", sum(brant_results$p_valor < 0.05),
    "de", nrow(brant_results), "\n")
sink()

cat("\n=== BRANT — PROPORCIONALIDAD ===\n")
print(brant_results, row.names = FALSE)


# ── 15c. CONCORDANCIA INTEROCULAR OD vs OS ───────
od_cat <- factor(
  ifelse(df$central_od == 3, "Normal",
         ifelse(df$central_od == 2, "Leve",
                ifelse(df$central_od == 1, "Moderada", "Severa"))),
  levels = c("Normal", "Leve", "Moderada", "Severa"), ordered = TRUE
)
os_cat <- factor(
  ifelse(df$central_os == 3, "Normal",
         ifelse(df$central_os == 2, "Leve",
                ifelse(df$central_os == 1, "Moderada", "Severa"))),
  levels = c("Normal", "Leve", "Moderada", "Severa"), ordered = TRUE
)

tab_conc     <- table(OD = od_cat, OS = os_cat)
acuerdo_obs  <- round(sum(diag(tab_conc)) / sum(tab_conc) * 100, 1)
n_total      <- sum(tab_conc)
p_obs        <- sum(diag(tab_conc)) / n_total
p_esp        <- sum((rowSums(tab_conc) / n_total) * (colSums(tab_conc) / n_total))
kappa_val    <- round((p_obs - p_esp) / (1 - p_esp), 3)
asimetricos  <- sum(as.numeric(od_cat) != as.numeric(os_cat))

sink("resultados/concordancia_interocular.txt")
cat("=== CONCORDANCIA INTEROCULAR OD vs OS ===\n\n")
cat("Tabla de concordancia:\n"); print(tab_conc)
cat("\nAcuerdo observado bruto:", acuerdo_obs, "%\n")
cat("Kappa de Cohen:", kappa_val, "\n")
cat("Interpretación:",
    ifelse(kappa_val < 0.2, "Leve",
           ifelse(kappa_val < 0.4, "Aceptable",
                  ifelse(kappa_val < 0.6, "Moderada",
                         ifelse(kappa_val < 0.8, "Sustancial", "Casi perfecta")))), "\n")
cat("\nPacientes con asimetría OD≠OS:", asimetricos,
    "(", round(asimetricos / n_total * 100, 1), "%)\n")
cat("Pacientes sin asimetría OD=OS:", n_total - asimetricos,
    "(", round((n_total - asimetricos) / n_total * 100, 1), "%)\n")
sink()

cat("\nKappa interocular:", kappa_val,
    "| Acuerdo observado:", acuerdo_obs, "%",
    "| Asimetría:", round(asimetricos / n_total * 100, 1), "%\n")


# =====================================================
# 16. PALETA ACADÉMICA Y TEMA
# =====================================================

pal_severidad <- c("#C6DEFF", "#74B9E0", "#2166AC", "#1A3A5C")

tema_academico <- theme_classic(base_size = 13) +
  theme(
    plot.title        = element_textbox_simple(
      size = 13, face = "bold", color = "#1A3A5C",
      halign = 0.5, margin = margin(b = 12)
    ),
    plot.subtitle     = element_text(size = 10.5, color = "#4A4A4A",
                                     hjust = 0.5, margin = margin(b = 10)),
    plot.caption      = element_text(size = 9, color = "#888888",
                                     hjust = 0, margin = margin(t = 8)),
    axis.title        = element_text(size = 11, color = "#2D2D2D", face = "bold"),
    axis.title.x      = element_text(margin = margin(t = 8)),
    axis.title.y      = element_text(margin = margin(r = 8)),
    axis.text         = element_text(size = 10.5, color = "#3A3A3A"),
    axis.line         = element_line(color = "#B0B8C1", linewidth = 0.4),
    axis.ticks        = element_line(color = "#B0B8C1", linewidth = 0.4),
    axis.ticks.length = unit(3, "pt"),
    panel.background  = element_rect(fill = "#FAFBFD", color = NA),
    plot.background   = element_rect(fill = "white",   color = NA),
    panel.grid.major  = element_line(color = "#E8EDF4", linewidth = 0.4),
    panel.grid.minor  = element_line(color = "#F2F5FA", linewidth = 0.25),
    panel.border      = element_rect(color = "#C8D0DC", fill = NA, linewidth = 0.4),
    legend.title      = element_text(size = 10, color = "#1A3A5C", face = "bold"),
    legend.text       = element_text(size = 9.5),
    legend.background = element_rect(fill = "white", color = "#E0E6F0", linewidth = 0.3),
    legend.key        = element_rect(fill = "transparent"),
    legend.margin     = margin(4, 8, 4, 8),
    plot.margin       = margin(14, 18, 12, 12)
  )

# Funciones auxiliares
n_label <- function(x) {
  data.frame(y = median(x, na.rm = TRUE), label = paste0("n = ", sum(!is.na(x))))
}

kruskal_p <- function(df, var, grupo) {
  kt <- kruskal.test(df[[var]] ~ df[[grupo]])
  if (kt$p.value < 0.001) "p < 0.001" else paste0("p = ", round(kt$p.value, 3))
}

wilcox_p <- function(df, var, grupo) {
  wt <- wilcox.test(df[[var]] ~ df[[grupo]])
  if (wt$p.value < 0.001) "p < 0.001" else paste0("p = ", round(wt$p.value, 3))
}


# =====================================================
# 17. GRÁFICOS G1–G6 (sin cambios)
# =====================================================

# ── G1. HISTOGRAMA GLUCEMIA ───────────────────────
g1 <- ggplot(df, aes(x = glucemia_num)) +
  geom_histogram(binwidth = 50, fill = "#4393C3", color = "#1A3A5C",
                 linewidth = 0.4, alpha = 0.85) +
  geom_vline(xintercept = mean(df$glucemia_num, na.rm = TRUE),
             color = "#D4550A", linetype = "dashed", linewidth = 0.7) +
  annotate("text",
           x = mean(df$glucemia_num, na.rm = TRUE) + 15, y = Inf,
           vjust = 1.5, hjust = 0,
           label = paste0("Media = ", round(mean(df$glucemia_num, na.rm = TRUE), 1)),
           size = 3.2, color = "#D4550A") +
  labs(
    title    = "Distribución de glucemia reciente en pacientes con DM2 (n = 113)",
    subtitle = paste0("Mediana = ", median(df$glucemia_num, na.rm = TRUE),
                      " mg/dL  |  RIC = ", IQR(df$glucemia_num, na.rm = TRUE)),
    x        = "Glucemia (mg/dL)", y = "Frecuencia",
    caption  = "Línea discontinua: media muestral"
  ) +
  tema_academico

# ── G2. HISTOGRAMA HbA1c ─────────────────────────
g2 <- ggplot(df, aes(x = hb_a1c_num)) +
  geom_histogram(binwidth = 1, fill = "#4393C3", color = "#1A3A5C",
                 linewidth = 0.4, alpha = 0.85) +
  geom_vline(xintercept = mean(df$hb_a1c_num, na.rm = TRUE),
             color = "#D4550A", linetype = "dashed", linewidth = 0.7) +
  annotate("text",
           x = mean(df$hb_a1c_num, na.rm = TRUE) + 0.2, y = Inf,
           vjust = 1.5, hjust = 0,
           label = paste0("Media = ", round(mean(df$hb_a1c_num, na.rm = TRUE), 1), "%"),
           size = 3.2, color = "#D4550A") +
  labs(
    title    = "Distribución de HbA1c en pacientes con DM2 (n = 113)",
    subtitle = paste0("Mediana = ", median(df$hb_a1c_num, na.rm = TRUE),
                      "%  |  RIC = ", round(IQR(df$hb_a1c_num, na.rm = TRUE), 1)),
    x        = "HbA1c (%)", y = "Frecuencia",
    caption  = "Línea discontinua: media muestral"
  ) +
  tema_academico

# ── G3. BOXPLOT — Tiempo DM2 por severidad ───────
p3 <- kruskal_p(df, "tiempo_de_evolucion_de_diabetes", "severidad_sensibilidad")

g3 <- ggplot(df, aes(x = severidad_sensibilidad, y = tiempo_de_evolucion_de_diabetes,
                     fill = severidad_sensibilidad)) +
  geom_boxplot(color = "#1A3A5C", outlier.color = "#1A3A5C",
               outlier.shape = 16, outlier.size = 1.5,
               linewidth = 0.5, alpha = 0.85, width = 0.55) +
  stat_summary(fun.data = n_label, geom = "text",
               size = 3, color = "#1A3A5C", vjust = -0.5) +
  scale_fill_manual(values = pal_severidad, guide = "none") +
  labs(
    title    = "Tiempo de evolución de DM2 según severidad de sensibilidad corneal (n = 113)",
    subtitle = paste0("Kruskal-Wallis: ", p3),
    x        = "Severidad de sensibilidad corneal",
    y        = "Tiempo de evolución DM2 (años)"
  ) +
  tema_academico

# ── G4. BOXPLOT — HbA1c por severidad ────────────
p4 <- kruskal_p(df, "hb_a1c_num", "severidad_sensibilidad")

g4 <- ggplot(df, aes(x = severidad_sensibilidad, y = hb_a1c_num,
                     fill = severidad_sensibilidad)) +
  geom_boxplot(color = "#1A3A5C", outlier.color = "#1A3A5C",
               outlier.shape = 16, outlier.size = 1.5,
               linewidth = 0.5, alpha = 0.85, width = 0.55) +
  stat_summary(fun.data = n_label, geom = "text",
               size = 3, color = "#1A3A5C", vjust = -0.5) +
  scale_fill_manual(values = pal_severidad, guide = "none") +
  labs(
    title    = "HbA1c según severidad de sensibilidad corneal (n = 113)",
    subtitle = paste0("Kruskal-Wallis: ", p4),
    x        = "Severidad de sensibilidad corneal",
    y        = "HbA1c (%)"
  ) +
  tema_academico

# ── G5. SCATTERPLOT — Tiempo DM2 vs severidad ────
r_val <- cor(as.numeric(df$severidad_sensibilidad),
             df$tiempo_de_evolucion_de_diabetes,
             use = "complete.obs", method = "spearman")

g5 <- ggplot(df, aes(x = tiempo_de_evolucion_de_diabetes,
                     y = as.numeric(severidad_sensibilidad))) +
  geom_point(color = "#2166AC", alpha = 0.55, size = 2,
             position = position_jitter(height = 0.08, seed = 42)) +
  geom_smooth(method = "lm", se = TRUE,
              color = "#D4550A", fill = "#D4550A", alpha = 0.10, linewidth = 1) +
  scale_y_continuous(breaks = 1:4, labels = levels(df$severidad_sensibilidad)) +
  annotate("text", x = Inf, y = -Inf, hjust = 1.05, vjust = -1,
           label = paste0("ρ = ", round(r_val, 2), "  (Spearman)"),
           size = 3.3, color = "#1A3A5C") +
  labs(
    title    = "Correlación entre tiempo de evolución de DM2 y severidad de sensibilidad corneal (n = 113)",
    subtitle = "Regresión lineal con intervalo de confianza 95%",
    x        = "Tiempo evolución DM2 (años)",
    y        = "Severidad de sensibilidad corneal"
  ) +
  tema_academico

# ── G6. BOXPLOT BINARIO ───────────────────────────
p6 <- wilcox_p(df, "tiempo_de_evolucion_de_diabetes", "sensibilidad_alterada")

g6 <- ggplot(df, aes(x = sensibilidad_alterada, y = tiempo_de_evolucion_de_diabetes,
                     fill = sensibilidad_alterada)) +
  geom_boxplot(color = "#1A3A5C", linewidth = 0.5, alpha = 0.85,
               width = 0.45, outlier.shape = NA) +
  geom_jitter(color = "#1A3A5C", alpha = 0.30, width = 0.12,
              size = 1.4, shape = 16) +
  stat_summary(fun.data = n_label, geom = "text",
               size = 3, color = "#1A3A5C", vjust = -0.5) +
  scale_fill_manual(values = c("NO" = "#74B9E0", "Si" = "#2166AC"), guide = "none") +
  labs(
    title    = "Tiempo de evolución de DM2 según sensibilidad corneal (n = 113)",
    subtitle = paste0("U de Mann-Whitney: ", p6),
    x        = "Sensibilidad corneal alterada",
    y        = "Tiempo de evolución DM2 (años)"
  ) +
  tema_academico


# =====================================================
# 18. GRÁFICO 7 — BARRAS NEUROPATÍA VS SENSIBILIDAD
#     (Gráfico 7 en la tesis — sin cambios)
# =====================================================

fisher_p   <- fisher.test(df$neuropatia_periferica_diabetica, df$sensibilidad_alterada)
p_fisher   <- ifelse(fisher_p$p.value < 0.001, "p < 0.001",
                     paste0("p = ", round(fisher_p$p.value, 3)))

df_bar <- df %>%
  count(neuropatia_periferica_diabetica, sensibilidad_alterada) %>%
  group_by(neuropatia_periferica_diabetica) %>%
  mutate(prop = n / sum(n), label = paste0(round(prop * 100, 1), "%")) %>%
  ungroup()

g7 <- ggplot(df_bar, aes(x    = neuropatia_periferica_diabetica,
                         y    = prop,
                         fill = sensibilidad_alterada)) +
  geom_col(color = "#1A3A5C", linewidth = 0.4, alpha = 0.88, position = "stack") +
  geom_text(aes(label = label),
            position = position_stack(vjust = 0.5),
            size = 3.3, color = "white", fontface = "bold") +
  scale_fill_manual(values = c("NO" = "#1A3A5C", "Si" = "#4393C3"),
                    name   = "Sensibilidad alterada") +
  scale_y_continuous(labels = percent_format(accuracy = 1),
                     expand = expansion(mult = c(0, 0.04))) +
  labs(
    title    = "Neuropatía periférica diabética y sensibilidad corneal alterada (n = 113)",
    subtitle = paste0("Fisher exacto: ", p_fisher),
    x        = "Neuropatía periférica diabética",
    y        = "Proporción",
    caption  = "Porcentajes dentro de cada grupo de neuropatía"
  ) +
  tema_academico +
  theme(legend.position = "right")


# =====================================================
# 19. GRÁFICO 8 — FOREST PLOT MODELO ORDINAL 7V
#     (Gráfico 8 en la tesis — versión final)
# =====================================================

forest_df <- data.frame(
  variable = rownames(or_ordinal),
  OR       = or_ordinal[, "OR"],
  IC_low   = or_ordinal[, "2.5 %"],
  IC_high  = or_ordinal[, "97.5 %"]
) |>
  dplyr::filter(variable %in% c(
    "tiempo_de_evolucion_de_diabetes",
    "neuropatia_periferica_diabeticaSi",
    "hipertension_arterialSi",
    "hb_a1c_num",
    "glucemia_num",
    "sexoMasculino",
    "edad"
  )) |>
  dplyr::mutate(
    variable = factor(variable,
                      levels = c(
                        "edad",
                        "glucemia_num",
                        "hb_a1c_num",
                        "sexoMasculino",
                        "hipertension_arterialSi",
                        "neuropatia_periferica_diabeticaSi",
                        "tiempo_de_evolucion_de_diabetes"
                      ),
                      labels = c(
                        "Edad",
                        "Glucemia",
                        "HbA1c",
                        "Sexo masculino",
                        "Hipertensión arterial",
                        "Neuropatía periférica",
                        "Tiempo evolución DM2"
                      )
    ),
    or_label = paste0("OR = ", round(OR, 2))
  )

g8 <- ggplot(forest_df, aes(x = OR, y = variable)) +
  
  geom_vline(xintercept = 1, linetype = "dashed",
             color = "#AAAAAA", linewidth = 0.65) +
  
  geom_errorbarh(aes(xmin = IC_low, xmax = IC_high),
                 height = 0.18, linewidth = 0.9, color = "#1A3A5C") +
  
  geom_point(size = 3.5, shape = 18, color = "#1A3A5C") +
  
  geom_text(aes(x = OR, label = or_label),
            hjust = -0.25, vjust = 0.4,
            size = 3.2, color = "#1A3A5C", family = "Arial") +
  
  scale_x_continuous(
    limits = c(0.7, 8.5),
    breaks = c(1, 2, 4, 6),
    expand = expansion(mult = c(0.02, 0.05))
  ) +
  
  labs(
    title    = "Modelo de regresión logística ordinal",
    subtitle = "OR con intervalo de confianza 95%  |  Variable dependiente: severidad de sensibilidad corneal",
    x        = "Odds Ratio",
    y        = NULL
  ) +
  
  tema_academico +
  theme(panel.grid.major.y = element_blank())


# =====================================================
# 20. EXPORTAR TODOS LOS GRÁFICOS
# =====================================================

graficos <- list(
  "01_histograma_glucemia"          = g1,
  "02_histograma_hba1c"             = g2,
  "03_boxplot_tiempo_severidad"     = g3,
  "04_boxplot_hba1c_severidad"      = g4,
  "05_scatter_tiempo_severidad"     = g5,
  "06_boxplot_tiempo_binario"       = g6,
  "07_barras_neuropatia_sensibilidad" = g7,
  "08_forest_plot_ordinal_7v"       = g8
)

for (nombre in names(graficos)) {
  ggsave(
    filename = paste0("figuras/figuras 2/", nombre, ".png"),
    plot     = graficos[[nombre]],
    width    = ifelse(nombre == "08_forest_plot_ordinal_7v", 9, 8),
    height   = 5.5,
    dpi      = 300,
    units    = "in",
    bg       = "white"
  )
}

message("✓ 8 gráficos exportados en /figuras/figuras 2/")


# =====================================================
# QQ PLOTS — Verificación visual de normalidad
# =====================================================

library(ggplot2)
library(patchwork)

# Función para generar QQ plot individual
qq_plot <- function(data, var, label) {
  p_shapiro <- shapiro.test(data[[var]])$p.value
  
  ggplot(data, aes(sample = .data[[var]])) +
    stat_qq() +
    stat_qq_line() +
    labs(
      title = label,
      subtitle = paste0("Shapiro-Wilk: p = ", round(p_shapiro, 3)),
      x = "Cuantiles teóricos",
      y = "Cuantiles observados"
    ) +
    theme_minimal() +
    theme(
      plot.title = element_text(face = "bold", hjust = 0.5),
      plot.subtitle = element_text(hjust = 0.5)
    )
}
qq1 <- qq_plot(df, "edad", "Edad (años)")
qq2 <- qq_plot(df, "hb_a1c_num", "HbA1c (%)")
qq3 <- qq_plot(df, "glucemia_num", "Glucemia (mg/dL)")
qq4 <- qq_plot(df, "tiempo_de_evolucion_de_diabetes", "Tiempo evolución DM2 (años)")

qq1
qq2
qq3
qq4

# Combinar en un solo gráfico con patchwork
g_qq <- (qq1 | qq2) / (qq3 | qq4) +
  plot_annotation(
    title    = "Gráficos Q-Q — Evaluación visual de normalidad (n = 113)",
    subtitle = "Línea discontinua naranja: distribución normal teórica  |  Puntos azules: datos observados",
    caption  = "Prueba de Shapiro-Wilk: p < 0.05 indica desviación significativa de la normalidad",
    theme    = theme(
      plot.title    = element_text(face = "bold", size = 13,
                                   color = "#1A3A5C", hjust = 0.5),
      plot.subtitle = element_text(size = 9.5, color = "#555555", hjust = 0.5),
      plot.caption  = element_text(size = 9,   color = "#888888", hjust = 0)
    )
  )



# =====================================================
# 15d. MODELO DE ODDS PROPORCIONALES PARCIALES (VGAM)
#      
# =====================================================

install.packages("VGAM")   # si no lo tienes instalado
library(VGAM)

# ── (1) PUENTE: modelo de odds proporcionales COMPLETO en VGAM ──
#     Debe reproducir tu polr(). Sirve para confirmar que la
#     dirección de los OR es la misma antes de relajar nada.
fit_po <- vglm(
  severidad_sensibilidad ~
    tiempo_de_evolucion_de_diabetes +
    neuropatia_periferica_diabetica +
    hipertension_arterial +
    hb_a1c_num + glucemia_num + sexo + edad,
  family = cumulative(parallel = TRUE, reverse = TRUE),
  data   = df
)
cat("\n== OR modelo PO completo (deben coincidir con polr) ==\n")
print(round(exp(coef(fit_po)), 3))   # ignora los 3 interceptos

# ── (2) MODELO DE ODDS PROPORCIONALES PARCIALES ──
#     SOLO 'tiempo' obtiene un coeficiente por cada umbral.
fit_ppo <- vglm(
  severidad_sensibilidad ~
    tiempo_de_evolucion_de_diabetes +
    neuropatia_periferica_diabetica +
    hipertension_arterial +
    hb_a1c_num + glucemia_num + sexo + edad,
  family = cumulative(parallel = FALSE ~ tiempo_de_evolucion_de_diabetes,
                      reverse = TRUE),
  data   = df
)
cat("\n== Resumen modelo odds parciales ==\n")
print(summary(fit_ppo))

# ── (3) OR del TIEMPO por umbral (lo interesante) ──
co  <- coef(summary(fit_ppo))
idx <- grep("tiempo_de_evolucion", rownames(co))   # 3 filas: una por umbral
tabla_tiempo <- data.frame(
  Umbral  = c("Normal vs ≥Leve",
              "≤Leve vs ≥Moderada",
              "≤Moderada vs Severa"),
  OR      = round(exp(co[idx, "Estimate"]), 3),
  IC_low  = round(exp(co[idx, "Estimate"] - 1.96 * co[idx, "Std. Error"]), 3),
  IC_high = round(exp(co[idx, "Estimate"] + 1.96 * co[idx, "Std. Error"]), 3),
  p       = round(co[idx, "Pr(>|z|)"], 4)
)
cat("\n== OR del tiempo de evolución, umbral por umbral ==\n")
print(tabla_tiempo, row.names = FALSE)

# ── (4) ¿Vale la pena relajar el supuesto? ──
#     Test de razón de verosimilitud: PO (restringido) vs PPO.
#     H0: el modelo proporcional completo es suficiente.
cat("\n== Test de razón de verosimilitud (PO vs PPO) ==\n")
print(lrtest(fit_ppo, fit_po))
cat("\nAIC PO completo :", round(AIC(fit_po), 1),
    "\nAIC PO parcial  :", round(AIC(fit_ppo), 1), "\n")

# Exportar
write.csv(tabla_tiempo, "resultados/or_tiempo_odds_parciales.csv", row.names = FALSE)

# install.packages("ordinal")
library(ordinal)

# Modelo PO completo (equivalente a tu polr)
clm_po <- clm(severidad_sensibilidad ~
                tiempo_de_evolucion_de_diabetes + neuropatia_periferica_diabetica +
                hipertension_arterial + hb_a1c_num + glucemia_num + sexo + edad,
              data = df)

# Modelo PARCIAL: 'tiempo' con efecto por umbral (va en nominal=)
clm_ppo <- clm(severidad_sensibilidad ~
                 neuropatia_periferica_diabetica + hipertension_arterial +
                 hb_a1c_num + glucemia_num + sexo + edad,
               nominal = ~ tiempo_de_evolucion_de_diabetes,
               data = df)

cat("\n== Convergencia ==\n")
cat("clm_po  logLik:", logLik(clm_po),  "\n")
cat("clm_ppo logLik:", logLik(clm_ppo), "\n")   # si NO es NaN, convergió

cat("\n== AIC ==\n")
cat("PO completo:", round(AIC(clm_po), 1), "| PO parcial:", round(AIC(clm_ppo), 1), "\n")

cat("\n== Test PO vs PPO ==\n")
print(anova(clm_po, clm_ppo))   # H0: el modelo PO simple es suficiente