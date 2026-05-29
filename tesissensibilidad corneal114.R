# # =====================================================
# TESIS: SENSIBILIDAD CORNEAL EN PACIENTES CON DM2
#
# =====================================================

# =====================================================
# 1. PAQUETES
# =====================================================
# Dentro de tu proyecto en RStudio:
usethis::use_git()        # Inicializa Git local
usethis::use_github()     # Crea el repositorio en GitHub y sube todo
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

# =====================================================
# 2. CARPETAS DE SALIDA
# =====================================================

dir.create("graficos", showWarnings = FALSE)
dir.create("tablas", showWarnings = FALSE)
dir.create("resultados", showWarnings = FALSE)

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
      .x %in% c("no", "NO", "No") ~ "NO",
      TRUE ~ .x
    )
  ))

# =====================================================
# 4. CONVERSIÓN DE VARIABLES
# =====================================================

df <- df %>%
  mutate(
    edad = as.numeric(edad),
    
    fecha_de_evaluacion = dmy(fecha_de_evaluacion),
    
    tiempo_de_evolucion_de_diabetes =
      as.numeric(tiempo_de_evolucion_de_diabetes),
    
    hb_a1c_num = as.numeric(gsub("%", "", hb_a1c)),
    
    glucemia_num = as.numeric(
      str_trim(gsub("mg/dL|mg/dl|mg/dl|/dL|/dl", "", glucemia_reciente))
    ),
    
    sexo = factor(sexo),
    tratamiento = factor(tratamiento),
    
    hipertension_arterial = factor(hipertension_arterial, levels = c("NO", "Si")),
    dislipidemia = factor(dislipidemia, levels = c("NO", "Si")),
    neuropatia_periferica_diabetica = factor(neuropatia_periferica_diabetica, levels = c("NO", "Si")),
    nefropatia_diabetica = factor(nefropatia_diabetica, levels = c("NO", "Si")),
    tabaquismo = factor(tabaquismo, levels = c("NO", "Si")),
    
    retinopatia_diabetica_od = factor(retinopatia_diabetica_od),
    retinopatia_diabetica_os = factor(retinopatia_diabetica_os),
    
    central_od = as.numeric(central_od),
    central_os = as.numeric(central_os),
    
    pio_od = as.numeric(pio_od),
    pio_os = as.numeric(pio_os),
    but_od = as.numeric(but_od),
    but_os = as.numeric(but_os)
  )

# =====================================================
# 5. VARIABLES DERIVADAS
# =====================================================

df <- df %>%
  mutate(
    sensibilidad_minima = pmin(central_od, central_os, na.rm = TRUE),
    
    severidad_sensibilidad = case_when(
      sensibilidad_minima == 3 ~ "Normal",
      sensibilidad_minima == 2 ~ "Leve",
      sensibilidad_minima == 1 ~ "Moderada",
      sensibilidad_minima == 0 ~ "Severa"
    ),
    
    severidad_sensibilidad = factor(
      severidad_sensibilidad,
      levels = c("Normal", "Leve", "Moderada", "Severa"),
      ordered = TRUE
    ),
    
    sensibilidad_alterada = case_when(
      sensibilidad_minima == 3 ~ "NO",
      sensibilidad_minima < 3 ~ "Si"
    ),
    
    sensibilidad_alterada = factor(
      sensibilidad_alterada,
      levels = c("NO", "Si")
    ),
    
    control_glucemico = case_when(
      hb_a1c_num < 7 ~ "Buen control",
      hb_a1c_num >= 7 & hb_a1c_num < 9 ~ "Control subóptimo",
      hb_a1c_num >= 9 ~ "Mal control"
    ),
    
    control_glucemico = factor(
      control_glucemico,
      levels = c("Buen control", "Control subóptimo", "Mal control"),
      ordered = TRUE
    ),
    
    glucemia_categoria = case_when(
      glucemia_num < 126 ~ "Menor de 126 mg/dL",
      glucemia_num >= 126 & glucemia_num < 200 ~ "126-199 mg/dL",
      glucemia_num >= 200 ~ "≥200 mg/dL"
    ),
    
    glucemia_categoria = factor(
      glucemia_categoria,
      levels = c("Menor de 126 mg/dL", "126-199 mg/dL", "≥200 mg/dL"),
      ordered = TRUE
    )
  )

# =====================================================
# 6. VALIDACIÓN INICIAL
# =====================================================

str(df)
summary(df)
colSums(is.na(df))

table(df$severidad_sensibilidad)
table(df$sensibilidad_alterada)
table(df$control_glucemico)
table(df$glucemia_categoria)

vis_miss(df)
miss_var_summary(df)
skim(df)

# =====================================================
# 7. EXPORTAR BASE LIMPIA
# =====================================================

write.csv(
  df,
  "resultados/base_sensibilidad_corneal_113_limpia.csv",
  row.names = FALSE
)

write_xlsx(
  list("dataset_limpio" = df),
  "resultados/base_sensibilidad_corneal_113_limpia.xlsx"
)

saveRDS(
  df,
  "resultados/base_sensibilidad_corneal_113_limpia.rds"
)

# =====================================================
# 8. TABLA 1: DESCRIPTIVA GENERAL
# =====================================================

tabla1 <- df %>%
  dplyr::select(
    edad,
    sexo,
    tiempo_de_evolucion_de_diabetes,
    hb_a1c_num,
    glucemia_num,
    hipertension_arterial,
    dislipidemia,
    neuropatia_periferica_diabetica,
    nefropatia_diabetica,
    severidad_sensibilidad
  ) %>%
  tbl_summary(
    statistic = list(
      all_continuous() ~ "{mean} ± {sd}",
      all_categorical() ~ "{n} ({p}%)"
    ),
    digits = all_continuous() ~ 1,
    missing = "no",
    label = list(
      edad ~ "Edad (años)",
      sexo ~ "Sexo",
      tiempo_de_evolucion_de_diabetes ~ "Tiempo de evolución DM2 (años)",
      hb_a1c_num ~ "HbA1c (%)",
      glucemia_num ~ "Glucemia reciente (mg/dL)",
      hipertension_arterial ~ "Hipertensión arterial",
      dislipidemia ~ "Dislipidemia",
      neuropatia_periferica_diabetica ~ "Neuropatía periférica diabética",
      nefropatia_diabetica ~ "Nefropatía diabética",
      severidad_sensibilidad ~ "Severidad de sensibilidad corneal"
    )
  ) %>%
  bold_labels()

tabla1

####categorización HbA1c
df <- df %>%
  mutate(
    control_glucemico_hba1c = case_when(
      hb_a1c_num < 7 ~ "Controlado",
      hb_a1c_num >= 7 & hb_a1c_num < 9 ~ "Mal controlado",
      hb_a1c_num >= 9 ~ "Muy mal controlado",
      TRUE ~ NA_character_
    ),
    control_glucemico_hba1c = factor(
      control_glucemico_hba1c,
      levels = c("Controlado", "Mal controlado", "Muy mal controlado"),
      ordered = TRUE
    )
  )
table(df$control_glucemico_hba1c, useNA = "ifany")

round(prop.table(table(df$control_glucemico_hba1c)) * 100, 1)
names(df)

table(df$control_glucemico_hba1c, useNA = "ifany")

round(prop.table(table(df$control_glucemico_hba1c)) * 100, 1)
cat("Distribución de control glucémico por HbA1c\n\n")

tabla_hba1c <- table(df$control_glucemico_hba1c)

print(tabla_hba1c)

cat("\nPorcentajes:\n")

print(round(prop.table(tabla_hba1c) * 100, 1))

# =====================================================
# 9. TABLA 2: INFERENCIAL POR SEVERIDAD
# =====================================================

tabla2 <- df %>%
  dplyr::select(
    edad,
    hb_a1c_num,
    glucemia_num,
    tiempo_de_evolucion_de_diabetes,
    neuropatia_periferica_diabetica,
    severidad_sensibilidad
  ) %>%
  tbl_summary(
    by = severidad_sensibilidad,
    statistic = list(
      all_continuous() ~ "{mean} ± {sd}",
      all_categorical() ~ "{n} ({p}%)"
    ),
    digits = all_continuous() ~ 1,
    missing = "no",
    label = list(
      edad ~ "Edad (años)",
      hb_a1c_num ~ "HbA1c (%)",
      glucemia_num ~ "Glucemia reciente (mg/dL)",
      tiempo_de_evolucion_de_diabetes ~ "Tiempo de evolución DM2 (años)",
      neuropatia_periferica_diabetica ~ "Neuropatía periférica diabética"
    )
  ) %>%
  add_p() %>%
  bold_labels()

tabla2

doc2 <- read_docx() %>%
  body_add_par(
    "Tabla 2. Variables clínicas según severidad de sensibilidad corneal",
    style = "heading 1"
  ) %>%
  body_add_flextable(as_flex_table(tabla2))

print(
  doc2,
  target = "tablas/tabla2_severidad_sensibilidad.docx"
)

kruskal.test(
  severidad_sensibilidad ~ control_glucemico_hba1c,
  data = df
)
# =====================================================
# 10. NORMALIDAD
# =====================================================

sink("resultados/shapiro_tests.txt")
tryCatch({
  shapiro.test(df$edad)
  shapiro.test(df$hb_a1c_num)
  shapiro.test(df$glucemia_num)
  shapiro.test(df$tiempo_de_evolucion_de_diabetes)
}, finally = sink())

# =====================================================
# 11. GRÁFICOS DE NORMALIDAD
# =====================================================

png("graficos/histograma_hba1c.png", width = 3000, height = 2000, res = 300)
hist(df$hb_a1c_num, main = "Histograma HbA1c", xlab = "HbA1c (%)")
dev.off()

png("graficos/qqplot_hba1c.png", width = 3000, height = 2000, res = 300)
qqnorm(df$hb_a1c_num)
qqline(df$hb_a1c_num)
dev.off()

png("graficos/histograma_glucemia.png", width = 3000, height = 2000, res = 300)
hist(df$glucemia_num, main = "Histograma glucemia", xlab = "Glucemia (mg/dL)")
dev.off()

png("graficos/qqplot_glucemia.png", width = 3000, height = 2000, res = 300)
qqnorm(df$glucemia_num)
qqline(df$glucemia_num)
dev.off()

# =====================================================
# 12. CORRELACIONES DE SPEARMAN
# =====================================================

sink("resultados/correlaciones_spearman.txt")
tryCatch({
  cor.test(
    df$tiempo_de_evolucion_de_diabetes,
    as.numeric(df$severidad_sensibilidad),
    method = "spearman"
  )
  
  cor.test(
    df$hb_a1c_num,
    as.numeric(df$severidad_sensibilidad),
    method = "spearman"
  )
  
  cor.test(
    df$glucemia_num,
    as.numeric(df$severidad_sensibilidad),
    method = "spearman"
  )
}, finally = sink())

# =====================================================
# 13. COMPARACIÓN BINARIA
# Sensibilidad normal vs alterada
# =====================================================

sink("resultados/analisis_binario_sensibilidad.txt")
tryCatch({
  wilcox.test(tiempo_de_evolucion_de_diabetes ~ sensibilidad_alterada, data = df)
  wilcox.test(hb_a1c_num ~ sensibilidad_alterada, data = df)
  wilcox.test(glucemia_num ~ sensibilidad_alterada, data = df)
  wilcox.test(edad ~ sensibilidad_alterada, data = df)
  fisher.test(table(df$sensibilidad_alterada, df$neuropatia_periferica_diabetica))
}, finally = sink())

grafico_mann_whitney <- ggplot(
  df,
  aes(
    x = sensibilidad_alterada,
    y = tiempo_de_evolucion_de_diabetes
  )
) +
  geom_boxplot() +
  geom_jitter(width = 0.1, alpha = 0.5) +
  labs(
    title = "Tiempo de evolución de DM2 según sensibilidad corneal",
    x = "Sensibilidad corneal",
    y = "Tiempo de evolución DM2 (años)"
  ) +
  theme_minimal(base_size = 14)

grafico_mann_whitney

ggsave(
  "graficos/mann_whitney_tiempo_dm2.png",
  grafico_mann_whitney,
  width = 10,
  height = 7,
  dpi = 300
)

# =====================================================
# 14. REGRESIÓN LOGÍSTICA BINARIA
# =====================================================

modelo_binario <- glm(
  sensibilidad_alterada ~
    tiempo_de_evolucion_de_diabetes +
    hb_a1c_num +
    glucemia_num +
    neuropatia_periferica_diabetica +
    edad,
  data = df,
  family = binomial
)

summary(modelo_binario)

or_binario <- exp(cbind(
  OR = coef(modelo_binario),
  confint.default(modelo_binario)
))

or_binario

write.csv(
  or_binario,
  "resultados/or_modelo_logistico_binario.csv"
)

sink("resultados/modelo_logistico_binario.txt")
tryCatch({
  summary(modelo_binario)
  or_binario
}, finally = sink())

# =====================================================
# 15. REGRESIÓN LOGÍSTICA ORDINAL
# =====================================================

levels(df$severidad_sensibilidad)

modelo_ordinal <- polr(
  severidad_sensibilidad ~
    tiempo_de_evolucion_de_diabetes +
    hb_a1c_num +
    glucemia_num +
    neuropatia_periferica_diabetica +
    edad,
  data = df,
  Hess = TRUE
)

summary(modelo_ordinal)

coef_table <- coef(summary(modelo_ordinal))

p_values <- pnorm(
  abs(coef_table[, "t value"]),
  lower.tail = FALSE
) * 2

coef_table <- cbind(
  coef_table,
  "p value" = p_values
)

coef_table

or_ordinal <- exp(cbind(
  OR = coef(modelo_ordinal),
  confint.default(modelo_ordinal)
))

or_ordinal

write.csv(coef_table, "resultados/coeficientes_modelo_ordinal.csv")
write.csv(or_ordinal, "resultados/or_modelo_ordinal.csv")

sink("resultados/modelo_logistico_ordinal.txt")
tryCatch({
  summary(modelo_ordinal)
  coef_table
  or_ordinal
}, finally = sink())
library(MASS)

# ── Reconstruir modelo y forest_df ─────────────
modelo_ordinal <- polr(
  severidad_sensibilidad ~
    tiempo_de_evolucion_de_diabetes +
    hb_a1c_num +
    glucemia_num +
    neuropatia_periferica_diabetica +
    edad,
  data  = df,
  Hess  = TRUE
)

or_ordinal <- exp(cbind(
  OR     = coef(modelo_ordinal),
  confint.default(modelo_ordinal)
))

forest_df <- data.frame(
  variable = rownames(or_ordinal),
  OR       = or_ordinal[, "OR"],
  IC_low   = or_ordinal[, "2.5 %"],
  IC_high  = or_ordinal[, "97.5 %"]
)

# Etiquetas legibles
forest_df$variable <- factor(
  forest_df$variable,
  levels = c(
    "tiempo_de_evolucion_de_diabetes",
    "neuropatia_periferica_diabeticaSi",
    "hb_a1c_num",
    "glucemia_num",
    "edad"
  ),
  labels = c(
    "Tiempo evolución DM2",
    "Neuropatía periférica",
    "HbA1c",
    "Glucemia",
    "Edad"
  )
)

# ── 7. FOREST PLOT ────────────────────────────
ggplot(forest_df, aes(x = OR, y = variable)) +
  geom_vline(xintercept = 1, linetype = "dashed",
             color = "#B0B8C1", linewidth = 0.7) +
  geom_errorbarh(aes(xmin = IC_low, xmax = IC_high),
                 height = 0.2, color = "#4393C3", linewidth = 0.8) +
  geom_point(color = "#1A3A5C", size = 3) +
  labs(
    title = "Modelo de regresión logística ordinal",
    x     = "Odds Ratio",
    y     = NULL
  ) +
  tema_academico

# =====================================================
# 16. FIGURAS PUBLICABLES
# =====================================================

grafico_tiempo <- ggplot(
  df,
  aes(
    x = severidad_sensibilidad,
    y = tiempo_de_evolucion_de_diabetes
  )
) +
  geom_boxplot() +
  labs(
    title = "Tiempo de evolución de DM2 según severidad de sensibilidad corneal",
    x = "Severidad de sensibilidad corneal",
    y = "Tiempo de evolución DM2 (años)"
  ) +
  theme_minimal(base_size = 14)

ggsave(
  "graficos/boxplot_tiempo_evolucion_vs_severidad.png",
  grafico_tiempo,
  width = 10,
  height = 7,
  dpi = 300
)

grafico_hba1c <- ggplot(
  df,
  aes(
    x = severidad_sensibilidad,
    y = hb_a1c_num
  )
) +
  geom_boxplot() +
  labs(
    title = "HbA1c según severidad de sensibilidad corneal",
    x = "Severidad de sensibilidad corneal",
    y = "HbA1c (%)"
  ) +
  theme_minimal(base_size = 14)

ggsave(
  "graficos/boxplot_hba1c_vs_severidad.png",
  grafico_hba1c,
  width = 10,
  height = 7,
  dpi = 300
)

grafico_neuropatia <- ggplot(
  df,
  aes(
    x = neuropatia_periferica_diabetica,
    fill = sensibilidad_alterada
  )
) +
  geom_bar(position = "fill") +
  scale_fill_manual(
    values = c("NO" = "#1F3B73", "Si" = "#4F81BD")
  ) +
  labs(
    title = "Neuropatía periférica y sensibilidad corneal alterada",
    x = "Neuropatía periférica diabética",
    y = "Proporción",
    fill = "Sensibilidad alterada"
  ) +
  theme_minimal(base_size = 14)

grafico_neuropatia

ggsave(
  "graficos/neuropatia_vs_sensibilidad.png",
  grafico_neuropatia,
  width = 10,
  height = 7,
  dpi = 300
)

# =====================================================
# 17. FOREST PLOT MODELO ORDINAL
# =====================================================

forest_data <- tidy(modelo_ordinal) %>%
  filter(
    term %in% c(
      "tiempo_de_evolucion_de_diabetes",
      "hb_a1c_num",
      "glucemia_num",
      "neuropatia_periferica_diabeticaSi",
      "edad"
    )
  ) %>%
  mutate(
    OR = exp(estimate),
    conf.low = exp(estimate - 1.96 * std.error),
    conf.high = exp(estimate + 1.96 * std.error),
    term = case_match(
      term,
      "tiempo_de_evolucion_de_diabetes" ~ "Tiempo evolución DM2",
      "hb_a1c_num" ~ "HbA1c",
      "glucemia_num" ~ "Glucemia",
      "neuropatia_periferica_diabeticaSi" ~ "Neuropatía periférica",
      "edad" ~ "Edad"
    )
  )

forest_plot <- ggplot(
  forest_data,
  aes(
    x = term,
    y = OR,
    ymin = conf.low,
    ymax = conf.high
  )
) +
  geom_pointrange() +
  geom_hline(yintercept = 1, linetype = 2) +
  coord_flip() +
  labs(
    title = "Modelo de regresión logística ordinal",
    x = "",
    y = "Odds Ratio"
  ) +
  theme_minimal(base_size = 14)

ggsave(
  "graficos/forest_plot_modelo_ordinal.png",
  forest_plot,
  width = 10,
  height = 7,
  dpi = 300
)

# =====================================================
# 18. CORRELACIÓN GRÁFICA
# =====================================================

grafico_correlacion <- ggplot(
  df,
  aes(
    x = tiempo_de_evolucion_de_diabetes,
    y = as.numeric(severidad_sensibilidad)
  )
) +
  geom_jitter(width = 0.3, height = 0.1) +
  geom_smooth(method = "lm", se = TRUE) +
  scale_y_continuous(
    breaks = 1:4,
    labels = c("Normal", "Leve", "Moderada", "Severa")
  ) +
  labs(
    title = "Tiempo de evolución DM2 y severidad de sensibilidad corneal",
    x = "Tiempo evolución DM2 (años)",
    y = "Severidad de sensibilidad corneal"
  ) +
  theme_minimal(base_size = 14)

ggsave(
  "graficos/correlacion_tiempo_dm2_severidad.png",
  grafico_correlacion,
  width = 10,
  height = 7,
  dpi = 300
)

######
# ─────────────────────────────────────────────
#  PALETA ACADÉMICA — AZUL MARINO
# ─────────────────────────────────────────────
library(ggplot2)
library(scales)

pal_severidad <- c("#C6DEFF", "#74B9E0", "#2166AC", "#1A3A5C")

tema_academico <- theme_classic(base_size = 13) +
  theme(
    plot.title       = element_text(face = "bold", size = 14, color = "#1A3A5C", hjust = 0.5),
    axis.title       = element_text(size = 12, color = "#2D2D2D"),
    axis.text        = element_text(size = 11, color = "#2D2D2D"),
    axis.line        = element_line(color = "#B0B8C1"),
    axis.ticks       = element_line(color = "#B0B8C1"),
    panel.grid.major = element_line(color = "#EEF4FB", linewidth = 0.5),
    panel.grid.minor = element_blank(),
    legend.title     = element_text(size = 11, color = "#1A3A5C"),
    legend.text      = element_text(size = 10),
    plot.margin      = margin(10, 15, 10, 10)
  )


# ── 1. HISTOGRAMA GLUCEMIA ─────────────────────
ggplot(df, aes(x = glucemia_num)) +
  geom_histogram(
    binwidth = 50, fill = "#4393C3", color = "#1A3A5C", linewidth = 0.4
  ) +
  labs(
    title = "Histograma glucemia",
    x     = "Glucemia (mg/dL)",
    y     = "Frecuencia"
  ) +
  tema_academico


# ── 2. HISTOGRAMA HbA1c ───────────────────────
ggplot(df, aes(x = hb_a1c_num)) +
  geom_histogram(
    binwidth = 1, fill = "#4393C3", color = "#1A3A5C", linewidth = 0.4
  ) +
  labs(
    title = "Histograma HbA1c",
    x     = "HbA1c (%)",
    y     = "Frecuencia"
  ) +
  tema_academico


# ── 3. BOXPLOT — Tiempo DM2 por severidad ─────
ggplot(df, aes(x = severidad_sensibilidad, y = tiempo_de_evolucion_de_diabetes,
               fill = severidad_sensibilidad)) +
  geom_boxplot(color = "#1A3A5C", outlier.color = "#1A3A5C",
               outlier.shape = 16, outlier.size = 1.8, linewidth = 0.5) +
  scale_fill_manual(values = pal_severidad, guide = "none") +
  labs(
    title = "Tiempo de evolución de DM2 según severidad de sensibilidad corneal",
    x     = "Severidad de sensibilidad corneal",
    y     = "Tiempo de evolución DM2 (años)"
  ) +
  tema_academico


# ── 4. BOXPLOT — HbA1c por severidad ──────────
ggplot(df, aes(x = severidad_sensibilidad, y = hb_a1c_num,
               fill = severidad_sensibilidad)) +
  geom_boxplot(color = "#1A3A5C", outlier.color = "#1A3A5C",
               outlier.shape = 16, outlier.size = 1.8, linewidth = 0.5) +
  scale_fill_manual(values = pal_severidad, guide = "none") +
  labs(
    title = "HbA1c según severidad de sensibilidad corneal",
    x     = "Severidad de sensibilidad corneal",
    y     = "HbA1c (%)"
  ) +
  tema_academico


# ── 5. SCATTERPLOT + REGRESIÓN ────────────────
ggplot(df, aes(x = tiempo_de_evolucion_de_diabetes,
               y = as.numeric(severidad_sensibilidad))) +
  geom_point(color = "#2166AC", alpha = 0.65, size = 2) +
  geom_smooth(method = "lm", se = TRUE,
              color = "#D4550A", fill = "#D4550A", alpha = 0.12, linewidth = 1) +
  scale_y_continuous(
    breaks = 1:4,
    labels = levels(df$severidad_sensibilidad)
  ) +
  labs(
    title = "Tiempo de evolución DM2 y severidad de sensibilidad corneal",
    x     = "Tiempo evolución DM2 (años)",
    y     = "Severidad de sensibilidad corneal"
  ) +
  tema_academico


# ── 6. BOXPLOT BINARIO — Sí / No ──────────────
ggplot(df, aes(x = sensibilidad_alterada, y = tiempo_de_evolucion_de_diabetes,
               fill = sensibilidad_alterada)) +
  geom_boxplot(color = "#1A3A5C", linewidth = 0.5,
               outlier.color = "#1A3A5C", outlier.shape = 16, outlier.size = 1.8) +
  geom_jitter(color = "#2166AC", alpha = 0.35, width = 0.15, size = 1.4) +
  scale_fill_manual(
    values = c("NO" = "#74B9E0", "Si" = "#2166AC"),
    guide  = "none"
  ) +
  labs(
    title = "Tiempo de evolución de DM2 según sensibilidad corneal",
    x     = "Sensibilidad corneal",
    y     = "Tiempo de evolución DM2 (años)"
  ) +
  tema_academico


# ── 7. FOREST PLOT ────────────────────────────
# (requiere que tengas forest_df con columnas: variable, OR, IC_low, IC_high)
ggplot(forest_df, aes(x = OR, y = variable)) +
  geom_vline(xintercept = 1, linetype = "dashed",
             color = "#B0B8C1", linewidth = 0.7) +
  geom_errorbarh(aes(xmin = IC_low, xmax = IC_high),
                 height = 0.2, color = "#4393C3", linewidth = 0.8) +
  geom_point(color = "#1A3A5C", size = 3) +
  labs(
    title = "Modelo de regresión logística ordinal",
    x     = "Odds Ratio",
    y     = NULL
  ) +
  tema_academico

# ── Guardar gráficos en objetos ────────────────
g1 <- ggplot(df, aes(x = glucemia_num)) +
  geom_histogram(binwidth = 50, fill = "#4393C3", color = "#1A3A5C", linewidth = 0.4) +
  labs(title = "Histograma glucemia", x = "Glucemia (mg/dL)", y = "Frecuencia") +
  tema_academico

g2 <- ggplot(df, aes(x = hb_a1c_num)) +
  geom_histogram(binwidth = 1, fill = "#4393C3", color = "#1A3A5C", linewidth = 0.4) +
  labs(title = "Histograma HbA1c", x = "HbA1c (%)", y = "Frecuencia") +
  tema_academico

g3 <- ggplot(df, aes(x = severidad_sensibilidad, y = tiempo_de_evolucion_de_diabetes,
                     fill = severidad_sensibilidad)) +
  geom_boxplot(color = "#1A3A5C", outlier.color = "#1A3A5C",
               outlier.shape = 16, outlier.size = 1.8, linewidth = 0.5) +
  scale_fill_manual(values = pal_severidad, guide = "none") +
  labs(title = "Tiempo de evolución de DM2 según severidad de sensibilidad corneal",
       x = "Severidad de sensibilidad corneal", y = "Tiempo de evolución DM2 (años)") +
  tema_academico

g4 <- ggplot(df, aes(x = severidad_sensibilidad, y = hb_a1c_num,
                     fill = severidad_sensibilidad)) +
  geom_boxplot(color = "#1A3A5C", outlier.color = "#1A3A5C",
               outlier.shape = 16, outlier.size = 1.8, linewidth = 0.5) +
  scale_fill_manual(values = pal_severidad, guide = "none") +
  labs(title = "HbA1c según severidad de sensibilidad corneal",
       x = "Severidad de sensibilidad corneal", y = "HbA1c (%)") +
  tema_academico

g5 <- ggplot(df, aes(x = tiempo_de_evolucion_de_diabetes,
                     y = as.numeric(severidad_sensibilidad))) +
  geom_point(color = "#2166AC", alpha = 0.65, size = 2) +
  geom_smooth(method = "lm", se = TRUE,
              color = "#D4550A", fill = "#D4550A", alpha = 0.12, linewidth = 1) +
  scale_y_continuous(breaks = 1:4, labels = levels(df$severidad_sensibilidad)) +
  labs(title = "Tiempo de evolución DM2 y severidad de sensibilidad corneal",
       x = "Tiempo evolución DM2 (años)", y = "Severidad de sensibilidad corneal") +
  tema_academico

g6 <- ggplot(df, aes(x = sensibilidad_alterada, y = tiempo_de_evolucion_de_diabetes,
                     fill = sensibilidad_alterada)) +
  geom_boxplot(color = "#1A3A5C", linewidth = 0.5,
               outlier.color = "#1A3A5C", outlier.shape = 16, outlier.size = 1.8) +
  geom_jitter(color = "#2166AC", alpha = 0.35, width = 0.15, size = 1.4) +
  scale_fill_manual(values = c("NO" = "#74B9E0", "Si" = "#2166AC"), guide = "none") +
  labs(title = "Tiempo de evolución de DM2 según sensibilidad corneal",
       x = "Sensibilidad corneal", y = "Tiempo de evolución DM2 (años)") +
  tema_academico

g7 <- ggplot(forest_df, aes(x = OR, y = variable)) +
  geom_vline(xintercept = 1, linetype = "dashed", color = "#B0B8C1", linewidth = 0.7) +
  geom_errorbarh(aes(xmin = IC_low, xmax = IC_high),
                 height = 0.2, color = "#4393C3", linewidth = 0.8) +
  geom_point(color = "#1A3A5C", size = 3) +
  labs(title = "Modelo de regresión logística ordinal", x = "Odds Ratio", y = NULL) +
  tema_academico


# ── Exportar todos en PNG 300 dpi ─────────────
graficos <- list(
  "01_histograma_glucemia"         = g1,
  "02_histograma_hba1c"            = g2,
  "03_boxplot_tiempo_severidad"    = g3,
  "04_boxplot_hba1c_severidad"     = g4,
  "05_scatter_tiempo_severidad"    = g5,
  "06_boxplot_tiempo_binario"      = g6,
  "07_forest_plot_ordinal"         = g7
)

# Crea la carpeta si no existe
dir.create("figuras", showWarnings = FALSE)

for (nombre in names(graficos)) {
  ggsave(
    filename = paste0("figuras/", nombre, ".png"),
    plot     = graficos[[nombre]],
    width    = 8,
    height   = 5.5,
    dpi      = 300,
    units    = "in",
    bg       = "white"
  )
}

message("✓ 7 gráficos exportados en /figuras/")


###################################################
##Estilización de gráficos
###################################################

# ─────────────────────────────────────────────
#  PAQUETES
# ─────────────────────────────────────────────
library(ggplot2)
library(dplyr)
library(MASS)
library(ggtext)      # títulos con markdown
library(scales)
# install.packages(c("ggtext")) si no los tienes


# ─────────────────────────────────────────────
#  TEMA REFINADO — PUBLICACIÓN CIENTÍFICA
# ─────────────────────────────────────────────
pal_severidad <- c("#C6DEFF", "#74B9E0", "#2166AC", "#1A3A5C")

tema_academico <- theme_classic(base_size = 13) +
  theme(
    # Título
    plot.title        = element_textbox_simple(
      size = 13, face = "bold", color = "#1A3A5C",
      halign = 0.5, margin = margin(b = 12)
    ),
    plot.subtitle     = element_text(size = 10.5, color = "#4A4A4A",
                                     hjust = 0.5, margin = margin(b = 10)),
    plot.caption      = element_text(size = 9, color = "#888888",
                                     hjust = 0, margin = margin(t = 8)),
    
    # Ejes
    axis.title        = element_text(size = 11, color = "#2D2D2D", face = "bold"),
    axis.title.x      = element_text(margin = margin(t = 8)),
    axis.title.y      = element_text(margin = margin(r = 8)),
    axis.text         = element_text(size = 10.5, color = "#3A3A3A"),
    axis.line         = element_line(color = "#B0B8C1", linewidth = 0.4),
    axis.ticks        = element_line(color = "#B0B8C1", linewidth = 0.4),
    axis.ticks.length = unit(3, "pt"),
    
    # Panel
    panel.background  = element_rect(fill = "#FAFBFD", color = NA),
    plot.background   = element_rect(fill = "white", color = NA),
    panel.grid.major  = element_line(color = "#E8EDF4", linewidth = 0.4),
    panel.grid.minor  = element_line(color = "#F2F5FA", linewidth = 0.25),
    panel.border      = element_rect(color = "#C8D0DC", fill = NA, linewidth = 0.4),
    
    # Leyenda
    legend.title      = element_text(size = 10, color = "#1A3A5C", face = "bold"),
    legend.text       = element_text(size = 9.5),
    legend.background = element_rect(fill = "white", color = "#E0E6F0", linewidth = 0.3),
    legend.key        = element_rect(fill = "transparent"),
    legend.margin     = margin(4, 8, 4, 8),
    
    plot.margin       = margin(14, 18, 12, 12)
  )


# ─────────────────────────────────────────────
#  FUNCIONES AUXILIARES
# ─────────────────────────────────────────────

# Etiqueta "n = X" para boxplots
n_label <- function(x) {
  data.frame(y = median(x, na.rm = TRUE),
             label = paste0("n = ", sum(!is.na(x))))
}

# p-valor Kruskal-Wallis formateado
kruskal_p <- function(df, var, grupo) {
  kt <- kruskal.test(df[[var]] ~ df[[grupo]])
  p  <- kt$p.value
  if (p < 0.001) "p < 0.001" else paste0("p = ", round(p, 3))
}

# p-valor Wilcoxon formateado
wilcox_p <- function(df, var, grupo) {
  wt <- wilcox.test(df[[var]] ~ df[[grupo]])
  p  <- wt$p.value
  if (p < 0.001) "p < 0.001" else paste0("p = ", round(p, 3))
}


# ─────────────────────────────────────────────
#  GRÁFICOS
# ─────────────────────────────────────────────

# ── 1. HISTOGRAMA GLUCEMIA ────────────────────
g1 <- ggplot(df, aes(x = glucemia_num)) +
  geom_histogram(
    binwidth = 50, fill = "#4393C3", color = "#1A3A5C",
    linewidth = 0.4, alpha = 0.85
  ) +
  geom_vline(xintercept = mean(df$glucemia_num, na.rm = TRUE),
             color = "#D4550A", linetype = "dashed", linewidth = 0.7) +
  annotate("text",
           x     = mean(df$glucemia_num, na.rm = TRUE) + 15,
           y     = Inf, vjust = 1.5, hjust = 0,
           label = paste0("Media = ", round(mean(df$glucemia_num, na.rm = TRUE), 1)),
           size  = 3.2, color = "#D4550A") +
  labs(
    title   = "Distribución de glucemia",
    subtitle = paste0("n = ", sum(!is.na(df$glucemia_num)),
                      "  |  Mediana = ", median(df$glucemia_num, na.rm = TRUE), " mg/dL",
                      "  |  RIC = ", IQR(df$glucemia_num, na.rm = TRUE)),
    x       = "Glucemia (mg/dL)",
    y       = "Frecuencia",
    caption = "Línea discontinua: media muestral"
  ) +
  tema_academico


# ── 2. HISTOGRAMA HbA1c ───────────────────────
g2 <- ggplot(df, aes(x = hb_a1c_num)) +
  geom_histogram(
    binwidth = 1, fill = "#4393C3", color = "#1A3A5C",
    linewidth = 0.4, alpha = 0.85
  ) +
  geom_vline(xintercept = mean(df$hb_a1c_num, na.rm = TRUE),
             color = "#D4550A", linetype = "dashed", linewidth = 0.7) +
  annotate("text",
           x     = mean(df$hb_a1c_num, na.rm = TRUE) + 0.2,
           y     = Inf, vjust = 1.5, hjust = 0,
           label = paste0("Media = ", round(mean(df$hb_a1c_num, na.rm = TRUE), 1), "%"),
           size  = 3.2, color = "#D4550A") +
  labs(
    title    = "Distribución de HbA1c",
    subtitle = paste0("n = ", sum(!is.na(df$hb_a1c_num)),
                      "  |  Mediana = ", median(df$hb_a1c_num, na.rm = TRUE), "%",
                      "  |  RIC = ", round(IQR(df$hb_a1c_num, na.rm = TRUE), 1)),
    x        = "HbA1c (%)",
    y        = "Frecuencia",
    caption  = "Línea discontinua: media muestral"
  ) +
  tema_academico


# ── 3. BOXPLOT — Tiempo DM2 por severidad ─────
p3 <- kruskal_p(df, "tiempo_de_evolucion_de_diabetes", "severidad_sensibilidad")

g3 <- ggplot(df, aes(x = severidad_sensibilidad,
                     y = tiempo_de_evolucion_de_diabetes,
                     fill = severidad_sensibilidad)) +
  geom_boxplot(color = "#1A3A5C", outlier.color = "#1A3A5C",
               outlier.shape = 16, outlier.size = 1.5,
               linewidth = 0.5, alpha = 0.85, width = 0.55) +
  stat_summary(fun.data = n_label, geom = "text",
               size = 3, color = "#1A3A5C", vjust = -0.5) +
  scale_fill_manual(values = pal_severidad, guide = "none") +
  labs(
    title    = "Tiempo de evolución de DM2 según severidad de sensibilidad corneal",
    subtitle = paste0("Kruskal-Wallis: ", p3),
    x        = "Severidad de sensibilidad corneal",
    y        = "Tiempo de evolución DM2 (años)"
  ) +
  tema_academico


# ── 4. BOXPLOT — HbA1c por severidad ──────────
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
    title    = "HbA1c según severidad de sensibilidad corneal",
    subtitle = paste0("Kruskal-Wallis: ", p4),
    x        = "Severidad de sensibilidad corneal",
    y        = "HbA1c (%)"
  ) +
  tema_academico


# ── 5. SCATTERPLOT + REGRESIÓN ────────────────
r_val <- cor(
  as.numeric(df$severidad_sensibilidad),
  df$tiempo_de_evolucion_de_diabetes,
  use = "complete.obs", method = "spearman"
)

g5 <- ggplot(df, aes(x = tiempo_de_evolucion_de_diabetes,
                     y = as.numeric(severidad_sensibilidad))) +
  geom_point(color = "#2166AC", alpha = 0.55, size = 2,
             position = position_jitter(height = 0.08, seed = 42)) +
  geom_smooth(method = "lm", se = TRUE,
              color = "#D4550A", fill = "#D4550A",
              alpha = 0.10, linewidth = 1) +
  scale_y_continuous(
    breaks = 1:4,
    labels = levels(df$severidad_sensibilidad)
  ) +
  annotate("text", x = Inf, y = -Inf, hjust = 1.05, vjust = -1,
           label = paste0("ρ = ", round(r_val, 2), "  (Spearman)"),
           size = 3.3, color = "#1A3A5C") +
  labs(
    title    = "Tiempo de evolución DM2 y severidad de sensibilidad corneal",
    subtitle = "Regresión lineal con intervalo de confianza 95%",
    x        = "Tiempo evolución DM2 (años)",
    y        = "Severidad de sensibilidad corneal"
  ) +
  tema_academico


# ── 6. BOXPLOT BINARIO ────────────────────────
p6 <- wilcox_p(df, "tiempo_de_evolucion_de_diabetes", "sensibilidad_alterada")

g6 <- ggplot(df, aes(x = sensibilidad_alterada,
                     y = tiempo_de_evolucion_de_diabetes,
                     fill = sensibilidad_alterada)) +
  geom_boxplot(color = "#1A3A5C", linewidth = 0.5, alpha = 0.85,
               width = 0.45, outlier.shape = NA) +
  geom_jitter(color = "#1A3A5C", alpha = 0.30, width = 0.12,
              size = 1.4, shape = 16) +
  stat_summary(fun.data = n_label, geom = "text",
               size = 3, color = "#1A3A5C", vjust = -0.5) +
  scale_fill_manual(
    values = c("NO" = "#74B9E0", "Si" = "#2166AC"),
    guide  = "none"
  ) +
  labs(
    title    = "Tiempo de evolución de DM2 según sensibilidad corneal",
    subtitle = paste0("U de Mann-Whitney: ", p6),
    x        = "Sensibilidad corneal alterada",
    y        = "Tiempo de evolución DM2 (años)"
  ) +
  tema_academico


# ── 7. FOREST PLOT ────────────────────────────
g7 <- ggplot(forest_df, aes(x = OR, y = variable)) +
  geom_rect(aes(xmin = -Inf, xmax = Inf,
                ymin = as.numeric(variable) - 0.5,
                ymax = as.numeric(variable) + 0.5,
                fill = as.numeric(variable) %% 2 == 0),
            alpha = 0.03) +
  scale_fill_manual(values = c("TRUE" = "#EEF4FB", "FALSE" = "white"),
                    guide = "none") +
  geom_vline(xintercept = 1, linetype = "dashed",
             color = "#B0B8C1", linewidth = 0.6) +
  geom_errorbarh(aes(xmin = IC_low, xmax = IC_high),
                 height = 0.18, color = "#4393C3", linewidth = 0.9) +
  geom_point(color = "#1A3A5C", size = 3.2, shape = 18) +
  geom_text(aes(label = paste0("OR = ", round(OR, 2))),
            hjust = -0.25, size = 3, color = "#2D2D2D") +
  labs(
    title    = "Modelo de regresión logística ordinal",
    subtitle = "OR con intervalo de confianza 95%  |  Variable dependiente: severidad de sensibilidad corneal",
    x        = "Odds Ratio",
    y        = NULL
  ) +
  tema_academico +
  theme(panel.grid.major.y = element_blank())


# ─────────────────────────────────────────────
#  EXPORTAR
# ─────────────────────────────────────────────
dir.create("figuras/figuras 2", recursive = TRUE, showWarnings = FALSE)

graficos <- list(
  "01_histograma_glucemia"      = g1,
  "02_histograma_hba1c"         = g2,
  "03_boxplot_tiempo_severidad" = g3,
  "04_boxplot_hba1c_severidad"  = g4,
  "05_scatter_tiempo_severidad" = g5,
  "06_boxplot_tiempo_binario"   = g6,
  "07_forest_plot_ordinal"      = g7
)

for (nombre in names(graficos)) {
  ggsave(
    filename = paste0("figuras/figuras 2/", nombre, ".png"),
    plot     = graficos[[nombre]],
    width    = 8, height = 5.5,
    dpi      = 300, units = "in",
    bg       = "white"
  )
}

message("✓ 7 gráficos exportados en /figuras/figuras 2/")

# ── Fisher exacto ─────────────────────────────
neuro    <- df$neuropatia_periferica_diabetica
sensi    <- df$sensibilidad_alterada

fisher_p <- fisher.test(neuro, sensi)
p_fisher <- ifelse(fisher_p$p.value < 0.001, "p < 0.001",
                   paste0("p = ", round(fisher_p$p.value, 3)))

# ── Datos con proporciones y etiquetas ────────
df_bar <- df %>%
  count(neuropatia_periferica_diabetica, sensibilidad_alterada) %>%
  group_by(neuropatia_periferica_diabetica) %>%
  mutate(
    prop  = n / sum(n),
    label = paste0(round(prop * 100, 1), "%")
  ) %>%
  ungroup()

# ── Gráfico ───────────────────────────────────
g8 <- ggplot(df_bar,
             aes(x    = neuropatia_periferica_diabetica,
                 y    = prop,
                 fill = sensibilidad_alterada)) +
  geom_col(color = "#1A3A5C", linewidth = 0.4, alpha = 0.88,
           position = "stack") +
  geom_text(aes(label = label),
            position = position_stack(vjust = 0.5),
            size = 3.3, color = "white", fontface = "bold") +
  scale_fill_manual(
    values = c("NO" = "#1A3A5C", "Si" = "#4393C3"),
    name   = "Sensibilidad alterada"
  ) +
  scale_y_continuous(labels = percent_format(accuracy = 1),
                     expand = expansion(mult = c(0, 0.04))) +
  labs(
    title    = "Neuropatía periférica y sensibilidad corneal alterada",
    subtitle = paste0("Fisher exacto: ", p_fisher),
    x        = "Neuropatía periférica diabética",
    y        = "Proporción",
    caption  = "Porcentajes dentro de cada grupo de neuropatía"
  ) +
  tema_academico +
  theme(legend.position = "right")

# ── Exportar ──────────────────────────────────
ggsave(
  filename = "figuras/figuras 2/08_barras_neuropatia_sensibilidad.png",
  plot     = g8,
  width    = 7, height = 5.5,
  dpi      = 300, units = "in",
  bg       = "white"
)

message("✓ g8 exportado")

################
g8_v2 <- ggplot(forest_df_v2, aes(x = OR, y = variable)) +
  # Bandas alternas tenues
  geom_rect(aes(xmin = -Inf, xmax = Inf,
                ymin = as.numeric(variable) - 0.5,
                ymax = as.numeric(variable) + 0.5,
                fill = as.numeric(variable) %% 2 == 0),
            alpha = 0.03) +
  scale_fill_manual(values = c("TRUE" = "#EEF4FB", "FALSE" = "white"),
                    guide = "none") +
  geom_vline(
    xintercept = 1,
    linetype   = "dashed",
    color      = "#B0B8C1",      # consistente con tema_academico
    linewidth  = 0.65
  ) +
  geom_errorbarh(
    aes(xmin = IC_low, xmax = IC_high),
    height    = 0.18,
    linewidth = 0.9,
    color     = "#4393C3"        # azul medio, igual que g7
  ) +
  geom_point(
    size  = 3.5,
    shape = 18,
    color = "#1A3A5C"
  ) +
  geom_text(
    aes(x = OR, label = or_label),
    hjust = -0.25,
    vjust =  0.4,
    size  =  3.2,
    color = "#2D2D2D"            # texto neutro, no azul marino
  ) +
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

ggsave(
  filename = "figuras/figuras 2/08_forest_plot_ordinal_v2.png",
  plot     = g8_v2,
  width    = 9,
  height   = 5.5,
  dpi      = 300,
  units    = "in",
  bg       = "white"
)

message("✓ Gráfico 8 exportado")