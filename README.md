# Sensibilidad Corneal en Pacientes con Diabetes Mellitus Tipo 2

**Tesis para optar por el título de Doctor en Medicina**  
Universidad Iberoamericana (UNIBE) — Escuela de Medicina  

**Autor:** Dr. Wainer Manuel Sena Rivas  
**Institución de estudio:** Instituto Nacional de Diabetes, Endocrinología y Nutrición (INDEN), Santo Domingo, República Dominicana  
**Período de recolección:** Marzo–Mayo 2026  

---

## Descripción

Estudio observacional, descriptivo-analítico y transversal que evalúa la **prevalencia y factores clínicos asociados a la alteración de la sensibilidad corneal** en 113 pacientes con DM2 atendidos en el INDEN mediante estimulación mecánica con algodón estandarizado.

Este repositorio contiene el código R completo para reproducir todos los análisis estadísticos, tablas y figuras del trabajo.

---

## Hallazgos principales

| Hallazgo | Resultado |
|---|---|
| Prevalencia de alteración corneal | 60% (leve 35%, moderada 16%, severa 8.8%) |
| Predictor más consistente | Tiempo de evolución DM2 (OR = 1.10; IC 95%: 1.04–1.16) |
| Neuropatía periférica diabética | OR = 3.02 (IC 95%: 1.32–6.90) |
| Hipertensión arterial | OR = 2.47 (IC 95%: 1.13–5.40) |
| Correlación Spearman tiempo/severidad | ρ = 0.381; p < 0.001 |
| Concordancia interocular (κ) | 0.536 (moderada); acuerdo observado 69.9% |
| AIC modelo final (7 variables) | 266.7 |

---

## Estructura del repositorio

```
├── tesissensibilidad_corneal_FINAL.R   # Script principal — análisis completo
├── base_sensibilidad_corneal.csv       # Base de datos original (no incluida)
├── resultados/
│   ├── base_sensibilidad_corneal_113_limpia.csv
│   ├── base_sensibilidad_corneal_113_limpia.rds
│   ├── shapiro_tests.txt
│   ├── correlaciones_spearman.txt
│   ├── analisis_binario_sensibilidad.txt
│   ├── or_modelo_logistico_binario.csv
│   ├── coeficientes_modelo_ordinal_7v.csv
│   ├── or_modelo_ordinal_7v.csv
│   ├── comparacion_aic_modelos.txt
│   ├── test_brant_proporcionalidad.txt
│   └── concordancia_interocular.txt
├── tablas/
│   ├── tabla1_descriptiva_general.docx
│   └── tabla2_severidad_sensibilidad.docx
└── figuras/figuras 2/
    ├── 01_histograma_glucemia.png
    ├── 02_histograma_hba1c.png
    ├── 03_boxplot_tiempo_severidad.png
    ├── 04_boxplot_hba1c_severidad.png
    ├── 05_scatter_tiempo_severidad.png
    ├── 06_boxplot_tiempo_binario.png
    ├── 07_barras_neuropatia_sensibilidad.png
    └── 08_forest_plot_ordinal_7v.png
```

> **Nota:** La base de datos original no está incluida en el repositorio por razones de confidencialidad y protección de datos de los participantes.

---

## Metodología estadística

| Análisis | Prueba | Justificación |
|---|---|---|
| Normalidad | Shapiro-Wilk | Variables cuantitativas no cumplen normalidad |
| Comparación 2 grupos | Mann-Whitney U | Variable no normal, grupos independientes |
| Comparación 4 grupos | Kruskal-Wallis | Equivalente no paramétrico del ANOVA |
| Correlación | Spearman (ρ) | Variables ordinales / continuas no normales |
| Asociación 2×2 | Fisher exacto | Frecuencias esperadas < 5 en subgrupos |
| Modelo multivariado | Regresión logística ordinal `polr()` | Variable dependiente ordinal de 4 categorías |

**Supuestos verificados:**
- Proporcionalidad de odds: test de Brant manual (tiempo de evolución p = 0.0295; demás variables cumplen)
- Concordancia interocular: κ = 0.536, acuerdo observado 69.9%
- Variable dependiente: peor ojo (mínimo entre OD y OS) — una observación por paciente

---

## Reproducibilidad

```r
# Versión de R utilizada
R version 4.5.2

# Paquetes principales
MASS        # polr() — regresión logística ordinal
tidyverse   # manipulación de datos
gtsummary   # tablas descriptivas
ggplot2     # visualizaciones
ggtext      # títulos con formato en gráficos
broom       # tidying de modelos
```

Para reproducir el análisis completo:

```r
# 1. Clonar el repositorio
# 2. Colocar la base de datos en la raíz del proyecto como:
#    base_sensibilidad_corneal.csv
# 3. Ejecutar el script principal
source("tesissensibilidad_corneal_FINAL.R")
```

---

## Consideraciones éticas

El estudio fue aprobado por el Comité de Ética de Investigación (CEI) de UNIBE. Todos los participantes firmaron consentimiento informado. Los datos están anonimizados y no se incluyen en este repositorio.

---

## Referencia

> Sena Rivas, W.M. (2026). *Sensibilidad corneal en pacientes con diabetes mellitus tipo 2 atendidos en el INDEN, Santo Domingo, República Dominicana, enero–mayo 2026.* Trabajo Profesional Final, Universidad Iberoamericana (UNIBE).

---

## Contacto

**Dr. Wainer Manuel Sena Rivas**  
Residente de Oftalmología — INDEN  
Universidad Iberoamericana (UNIBE)
