# tesis-oftalmologia-2026

*Pipeline de análisis reproducible — Tesis de especialidad en Oftalmología.*

Código que respalda la tesis **"Sensibilidad corneal en pacientes con diabetes mellitus
tipo 2"** (Residencia de Oftalmología, UNIBE / INDEN, 2026).

> **Repositorio de tesis:** https://github.com/wmsenar/tesis-oftalmologia-2026  
> **Repositorio del artículo:** https://github.com/wmsenar/Cornea-Sens-DM2026  
> **Autor:** Dr. Wainer Manuel Sena Rivas — ORCID: [0009-0006-4261-9241](https://orcid.org/0009-0006-4261-9241)  
> **Correo:** w.sena@prof.unibe.edu.do  
> **Ética:** Protocolo aprobado por el CEI-UNIBE (N.° ACECEI2026-551)

---

## Archivos

| Archivo | Descripción |
|---|---|
| `tesissensibilidad corneal114.R` | **Script de tesis.** Modelo de 7 variables como primario; análisis completo para el documento de tesis. |
| `cornea_sens_dm2.R` | **Script de publicación.** Modelo parsimonioso de 3 variables + PPO clm; armonizado con el artículo. |
| `generar_articulo.R` | Generador Word OftalRev vía `officer`/`flextable`. |
| `referencias_corneal.bib` | Bibliografía BibTeX compartida. |
| `vancouver.csl` | Estilo de cita Vancouver. |
| `base_sensibilidad_corneal.csv` | Datos del estudio — **excluidos de git** (datos de pacientes). |

> El manuscrito Quarto (`sena_sensibilidad_corneal_dm2.qmd`) y su render (`.docx`) viven
> en el repositorio del artículo: https://github.com/wmsenar/Cornea-Sens-DM2026

---

## Diferencias entre ambos scripts

| | `tesissensibilidad corneal114.R` | `cornea_sens_dm2.R` |
|---|---|---|
| Modelo principal | 7 variables | 3 variables (parsimonioso) |
| IC | Wald | Perfil (*profile likelihood*) |
| PPO | VGAM (apéndice) | `clm()` paquete `ordinal` |
| Propósito | Tesis | Publicación |

---

## Requisitos

- **R** ≥ 4.5.2
- Paquetes:

```r
install.packages(c("MASS", "tidyverse", "janitor", "lubridate",
                   "binom", "psych", "pwr", "brant", "ordinal",
                   "gtsummary", "flextable", "officer",
                   "ggtext", "patchwork", "scales"))
```

---

## Cómo reproducir

```r
# Script de tesis (modelo 7 variables)
source("tesissensibilidad corneal114.R")

# Script de publicación (modelo 3 variables + PPO)
source("cornea_sens_dm2.R")
```

---

## Resultados clave (modelo de publicación)

| Análisis | Resultado |
|---|---|
| Prevalencia de alteración corneal | 60,2 % (IC 95%: 51,0–68,7) |
| Correlación tiempo vs. severidad (Spearman) | ρ = 0,381; p < 0,001 |
| NPD — OR ajustado (IC perfil) | 2,892 (1,351–6,319); p = 0,007 |
| Tiempo de evolución — OR ajustado | 1,084 (1,033–1,140); p = 0,001 |
| HTA — OR ajustado | 2,064 (0,980–4,442); p = 0,059 |
| AIC modelo principal | 263,2 |
| LRT 3-var vs. 7-var | p = 0,336 |

---

## Disponibilidad de datos

`base_sensibilidad_corneal.csv` está excluido del repositorio. El estudio fue aprobado por
el Comité de Ética de Investigación de la UNIBE (N.° ACECEI2026-551).

---

## Licencia

Código: MIT. Datos: restringidos por confidencialidad clínica.
