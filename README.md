# Sensibilidad corneal en diabetes mellitus tipo 2 — Análisis reproducible

*Reproducible R analysis: corneal sensitivity and its clinical predictors in type 2 diabetes.*

Código y datos que respaldan el artículo derivado del estudio de sensibilidad corneal en
pacientes con diabetes mellitus tipo 2 (DM2). Todos los resultados estadísticos del
manuscrito se reproducen ejecutando un único script de R.

---

## Cita

Pendiente de publicación. Citar como:

> Sena Rivas WM. *[Título del artículo].* [Revista]. 2026. DOI: [pendiente].

*(Completar con los datos definitivos al momento de la aceptación.)*

---

## Archivos

Estructura plana (todos los archivos en la raíz del repositorio):

| Archivo | Descripción |
|---|---|
| `analisis_publicacion_sensibilidad_corneal.R` | **Script principal.** Reproduce todos los cálculos del artículo, de principio a fin. |
| `base_sensibilidad_corneal.csv` | Base de datos del estudio (sin identificadores de pacientes). |
| `README.md` | Este archivo. |

---

## Requisitos

- **R** ≥ 4.5.2
- Paquetes:

```r
install.packages(c("MASS", "tidyverse", "janitor", "lubridate",
                   "binom", "psych", "pwr", "brant"))
```

---

## Cómo reproducir

1. Coloca `base_sensibilidad_corneal.csv` en el directorio de trabajo.
2. Abre `analisis_publicacion_sensibilidad_corneal.R` y ejecútalo **de principio a fin**
   (el script está encadenado: cada bloque depende de los anteriores).
3. La salida en consola incluye, en el mismo orden del artículo, cada cifra reportada.

---

## Qué reproduce el script

| Bloque | Resultado del artículo | Valor esperado |
|---|---|---|
| 2 | Prevalencia de alteración corneal (IC 95% Wilson) | 60.2 % [51.0–68.7] |
| 3 | Correlaciones de Spearman con la severidad | tiempo ρ = 0.381; HbA1c y glucemia no significativas |
| 4 | Mínimo efecto detectable (MDE; Spearman, Bonett-Wright) | ρ ≥ 0.265 (80 %); ρ ≥ 0.306 (90 %) |
| 5 | Concordancia interocular (κ de Cohen) | κ = 0.536; acuerdo 69.9 %; discordancia 30.1 % |
| 6 | Análisis bivariado / Fisher (NPD) | OR ≈ 3.83 |
| 7 | **Modelo ordinal principal (parsimonioso, 3 variables)** | tiempo 1.084; NPD 2.892; HTA 2.064 (marginal); AIC 263.2 |
| 8 | Modelo de sensibilidad (7 variables) + LRT | AIC 266.7; LRT p = 0.336 |
| 9 | Supuesto de odds proporcionales (Brant) sobre el modelo principal | violación para el tiempo de evolución |

---

## Notas metodológicas

- **Variable de desenlace:** severidad de sensibilidad corneal del **peor ojo** (mínimo
  entre ambos ojos), justificado por la concordancia interocular solo moderada (κ = 0.536).
- **Modelo principal:** regresión logística ordinal parsimoniosa de 3 predictores
  (tiempo de evolución, neuropatía periférica diabética, hipertensión arterial). El modelo
  de 7 predictores se reporta únicamente como análisis de sensibilidad; el test de razón de
  verosimilitud (p = 0.336) confirma que las variables adicionales no mejoran el ajuste.
- **Supuesto de odds proporcionales:** la prueba de Brant indica incumplimiento para el
  tiempo de evolución; su OR debe interpretarse como un efecto promedio a través de los
  umbrales de severidad. El modelo de odds proporcionales parciales no produjo estimaciones
  numéricamente estables con el tamaño muestral disponible y, por tanto, no se reporta
  (queda como apéndice comentado en el script).
- **Potencia:** no se calculó tamaño muestral a priori; se reporta el mínimo efecto
  detectable (MDE) en lugar de potencia post-hoc.

---

## Disponibilidad de datos

`base_sensibilidad_corneal.csv` no contiene nombres, números de expediente ni fechas que
permitan reidentificar pacientes. El estudio fue aprobado por el comité de ética
correspondiente.

---

## Licencia

Sugerida: MIT para el código; los datos según la política de la institución y la revista.
*(Definir antes de hacer público el repositorio.)*

---

## Autor

Dr. Wainer Manuel Sena Rivas — 2026
