# ============================================================
#  GENERAR ARTÍCULO OftalRev  (sin Quarto — solo officer)
#  Reutiliza los PNG y tablas que produce cornea_sens_dm2.R
#
#  Dr. Wainer Manuel Sena Rivas
#  Servicio de Oftalmología, INDEN — Hospital Escuela Dr. Jorge Abraham Hazoury Bahlés
#  Universidad Iberoamericana (UNIBE), Santo Domingo, República Dominicana
#  Correo: w.sena@prof.unibe.edu.do | ORCID: 0009-0006-4261-9241
#  2026
#
#  USO:
#   1) Corre primero tu script de análisis (tesissensibilidad_corneal114.R)
#      para que existan los PNG en "figuras/figuras 2/" y el CSV
#      "resultados/or_tiempo_odds_parciales.csv".
#   2) Coloca "referencia_oftalrev.docx" en el directorio de trabajo.
#   3) source("generar_articulo.R")  -> crea "articulo_oftalrev_generado.docx"
# ============================================================

library(officer)
library(flextable)

# ---------- formato base (Times New Roman 12, doble espacio, justificado) ----------
ft_n  <- fp_text(font.size = 12, font.family = "Times New Roman")
ft_b  <- update(ft_n, bold = TRUE)
ft_h  <- fp_text(font.size = 13, font.family = "Times New Roman", bold = TRUE)
pp_b  <- fp_par(text.align = "justify", line_spacing = 2, padding.bottom = 0)
pp_h  <- fp_par(text.align = "left",    line_spacing = 1, padding.top = 8, padding.bottom = 4)

h  <- function(doc, txt) body_add_fpar(doc, fpar(ftext(txt, ft_h), fp_p = pp_h))
p  <- function(doc, txt) body_add_fpar(doc, fpar(ftext(txt, ft_n), fp_p = pp_b))
# párrafo con etiqueta en negrita: lab(doc, "Objetivo:", " texto...")
lab <- function(doc, label, txt) body_add_fpar(doc, fpar(ftext(label, ft_b), ftext(txt, ft_n), fp_p = pp_b))
br <- function(doc) body_add_break(doc)

set_flextable_defaults(font.family = "Times New Roman", font.size = 10)

# ---------- documento base (hereda pie de página con numeración) ----------
plantilla <- "referencia_oftalrev.docx"
doc <- if (file.exists(plantilla)) read_docx(plantilla) else read_docx()

# ============================================================
# HOJA DE IDENTIFICACIÓN
# ============================================================
doc <- h(doc, "Hoja de identificación")
doc <- lab(doc, "Tipo de manuscrito: ", "Artículo original")
doc <- lab(doc, "Título (español): ", "Sensibilidad corneal evaluada mediante estímulo mecánico con algodón en pacientes con diabetes mellitus tipo 2: estudio transversal en un hospital de referencia de la República Dominicana")
doc <- lab(doc, "Title (English): ", "Corneal sensitivity assessed by mechanical cotton stimulation in patients with type 2 diabetes mellitus: a cross-sectional study in a referral hospital in the Dominican Republic")
doc <- lab(doc, "Autores: ", "Wainer Manuel Sena Rivas (autor de correspondencia); Cynthia Cunillera Batlle; Ángel Campusano")
doc <- p(doc, "Residencia de Oftalmología, Instituto Nacional de Diabetes, Endocrinología y Nutrición (INDEN) – Hospital Escuela Dr. Jorge Abraham Hazoury Bahlés; Universidad Iberoamericana (UNIBE). Santo Domingo, Distrito Nacional, República Dominicana.")
doc <- lab(doc, "Autor de correspondencia: ", "Dr. Wainer Manuel Sena Rivas — INDEN, Hospital Escuela Dr. Jorge Abraham Hazoury Bahlés, Santo Domingo, República Dominicana. Teléfono: +18296471816. Correo: w.sena@prof.unibe.edu.do. ORCID: 0009-0006-4261-9241.")
doc <- lab(doc, "Declaración de ética: ", "Conforme al Informe Belmont y la Declaración de Helsinki. Aprobación CEI-UNIBE N.° ACECEI2026-551. Consentimiento informado por escrito.")
doc <- lab(doc, "Conflicto de interés: ", "Ninguno declarado.   Financiación: Sin financiación externa.")
doc <- br(doc)

# ============================================================
# RESUMEN / ABSTRACT
# ============================================================
doc <- h(doc, "Resumen")
doc <- lab(doc, "Objetivo: ", "Evaluar la sensibilidad corneal mediante estímulo mecánico con algodón en pacientes con diabetes mellitus tipo 2 (DM2) y analizar su asociación con variables clínicas y metabólicas.")
doc <- lab(doc, "Materiales y métodos: ", "Estudio observacional, descriptivo-analítico y transversal en 113 pacientes con DM2 atendidos en el Hospital Escuela Dr. Jorge Abraham Hazoury Bahlés (Santo Domingo, República Dominicana), marzo–mayo 2026. La sensibilidad corneal se clasificó en cuatro niveles ordinales. Se aplicaron pruebas no paramétricas (Spearman, Mann-Whitney U, Kruskal-Wallis, Fisher) y regresión logística ordinal.")
doc <- lab(doc, "Resultados: ", "El 60% presentó algún grado de alteración corneal (leve 35%, moderada 16%, severa 8.8%). El tiempo de evolución se correlacionó con la severidad (ρ = 0.381; p < 0.001). En el modelo multivariado, la neuropatía periférica diabética fue el predictor de mayor magnitud (OR = 3.02; IC 95%: 1.32–6.90), seguida de hipertensión arterial (OR = 2.47; IC 95%: 1.13–5.40) y tiempo de evolución (OR = 1.10; IC 95%: 1.04–1.16). Sin asociación con HbA1c (ρ = 0.085; p = 0.373) ni glucemia (ρ = 0.053; p = 0.577).")
doc <- lab(doc, "Conclusión: ", "La alteración de la sensibilidad corneal fue altamente prevalente y se asoció con predictores clínicos accesibles. La estimulación con algodón es una herramienta de tamización viable en entornos con recursos limitados.")
doc <- lab(doc, "Palabras clave: ", "sensibilidad corneal, diabetes mellitus tipo 2, neuropatía diabética, estesiometría.")
doc <- h(doc, "Abstract")
doc <- lab(doc, "Objective: ", "To assess corneal sensitivity using mechanical cotton stimulation in patients with type 2 diabetes mellitus (T2DM) and to analyze its association with clinical and metabolic variables.")
doc <- lab(doc, "Materials and methods: ", "Observational, descriptive-analytical, cross-sectional study in 113 patients with T2DM at the Dr. Jorge Abraham Hazoury Bahlés Teaching Hospital (Santo Domingo, Dominican Republic), March–May 2026. Corneal sensitivity was classified into four ordinal levels. Non-parametric tests and ordinal logistic regression were applied.")
doc <- lab(doc, "Results: ", "Sixty percent showed some corneal impairment (mild 35%, moderate 16%, severe 8.8%). Diabetes duration correlated with severity (ρ = 0.381; p < 0.001). In the multivariable model, diabetic peripheral neuropathy was the strongest predictor (OR = 3.02; 95% CI: 1.32–6.90), followed by hypertension (OR = 2.47; 95% CI: 1.13–5.40) and duration (OR = 1.10; 95% CI: 1.04–1.16). No association with HbA1c or glucose.")
doc <- lab(doc, "Conclusion: ", "Corneal sensitivity impairment was highly prevalent and associated with accessible clinical predictors. Cotton stimulation is a feasible screening tool in resource-limited settings.")
doc <- lab(doc, "Keywords: ", "corneal sensitivity, type 2 diabetes mellitus, diabetic neuropathy, esthesiometry.")
doc <- br(doc)

# ============================================================
# TEXTO
# ============================================================
doc <- h(doc, "Introducción")
doc <- p(doc, "La diabetes mellitus tipo 2 (DM2) es uno de los problemas de salud pública más relevantes a escala mundial; se estima que alrededor de 530 millones de adultos conviven con diabetes, cifra que podría alcanzar los 640 millones hacia 2030 [1]. La inmensa mayoría corresponde a DM2, definida por hiperglucemia crónica derivada de resistencia a la insulina y deterioro de las células beta [1].")
doc <- p(doc, "La neuropatía diabética periférica (NPD) afecta hasta la mitad de los pacientes [2]; en etapas tempranas compromete fibras pequeñas no mielinizadas C y Aδ antes que las de mayor calibre [3]. La córnea, uno de los tejidos más densamente inervados, organiza su red sensitiva trigeminal en el plexo subbasal [4], que sostiene el reflejo de parpadeo, la película lagrimal y la integridad epitelial [5]. La hiperglucemia reduce la densidad de fibras del plexo subbasal [6,7]; la neuropatía corneal diabética se presenta en 47–64% de los pacientes [8].")
doc <- p(doc, "La sensibilidad corneal es el correlato funcional de esa inervación, y se relaciona con la duración de la enfermedad, el control glucémico y la NPD [9,10]. En la República Dominicana no se han publicado estudios que la evalúen mediante métodos clínicos accesibles. El objetivo fue evaluar la sensibilidad corneal mediante estímulo con algodón en pacientes con DM2 y analizar su asociación con indicadores clínicos y metabólicos, en el INDEN, entre marzo y mayo de 2026.")

doc <- h(doc, "Sujetos, materiales y métodos")
doc <- lab(doc, "Diseño y población. ", "Estudio observacional, descriptivo-analítico y transversal. Pacientes con DM2 mayores de 30 años atendidos en el INDEN (marzo–mayo 2026; 526 accesibles). Muestreo no probabilístico por conveniencia: 113 pacientes elegibles. Se excluyeron antecedente de cirugía ocular, lentes de contacto, enfermedad activa de superficie ocular, queratitis herpética previa, neuropatías no diabéticas, medicación tópica ocular o lágrimas con conservantes >3 meses, enfermedades sistémicas que afectan la sensibilidad corneal y embarazo.")
doc <- lab(doc, "Evaluación de la sensibilidad corneal. ", "Estimulación con hebra de algodón estéril (punta de 1–2 cm), sin biomicroscopio, paciente sedente y mirada al frente; estímulo al epitelio central desde el ángulo temporal, 2–3 veces por ojo. Respuesta evaluada combinando reflejo de parpadeo (objetivo) y reporte verbal (subjetivo), clasificada en escala ordinal de cuatro niveles. Ambos ojos por un único observador entrenado [11]. La NPD se determinó por expediente clínico.")
doc <- lab(doc, "Análisis estadístico. ", "R 4.5.2. Concordancia interocular (kappa de Cohen) moderada (κ = 0.536); se adoptó la severidad del peor ojo [8,12]. Normalidad con Shapiro-Wilk. Pruebas no paramétricas: Mann-Whitney U, Kruskal-Wallis, Spearman (ρ) y prueba exacta de Fisher. Modelo principal: regresión logística ordinal (polr, MASS) con tres predictores (AIC = 263,2; IC de perfil); modelo de sensibilidad con siete predictores (AIC = 266,7; LRT p = 0,336). El supuesto de odds proporcionales se verificó con la prueba de Brant; al incumplirlo el tiempo de evolución (p = 0.030), se ajustó un modelo de odds proporcionales parciales (clm, paquete ordinal), comparado con el completo mediante LRT y AIC. Significancia p < 0.05. Código: https://github.com/wmsenar/Cornea-Sens-DM2026")

doc <- h(doc, "Aspecto ético")
doc <- p(doc, "Conforme al Informe Belmont y la Declaración de Helsinki. Consentimiento informado por escrito. Protocolo aprobado por el Comité de Ética de Investigación de la UNIBE (N.° ACECEI2026-551). Se garantizó confidencialidad, anonimato y el derecho a retirarse sin afectar la atención médica.")

doc <- h(doc, "Resultados")
doc <- lab(doc, "Características generales. ", "De 526 identificados, 113 analizados. Edad media 58.5 ± 11.2 años, predominio femenino (64%). Tiempo medio de evolución 11.6 ± 7.9 años; HbA1c 9.0 ± 2.5%; glucemia 190.3 ± 87.7 mg/dL. El 60% mostró compromiso corneal: leve (35%), moderado (16%), severo (8.8%) (Tabla 1; Gráficos 1 y 2).")
doc <- lab(doc, "Análisis por severidad. ", "Diferencias significativas en el tiempo de evolución entre grupos (p < 0.001): moderado y severo con mayor duración (18 ± 7 y 20 ± 4 años) frente a normal y leve (10 ± 8 y 9 ± 6) (Gráfico 3). La NPD se asoció con la severidad (p = 0.002), del 18% (normal) al 61% y 60% (moderado y severo). Sin diferencias para edad, HbA1c ni glucemia (Tabla 2; Gráfico 4).")
doc <- lab(doc, "Correlaciones. ", "El tiempo de evolución correlacionó con la severidad (ρ = 0.381; p < 0.001) (Gráfico 5); sin correlación con HbA1c (ρ = 0.085; p = 0.373) ni glucemia (ρ = 0.053; p = 0.577) (Tabla 3).")
doc <- lab(doc, "Análisis dicotómico. ", "Mayor tiempo de evolución en alterados (p = 0.048) (Gráfico 6); la NPD se asoció con la alteración (OR = 3.83; IC 95%: 1.47–10.97; p = 0.002) (Gráfico 7).")
doc <- lab(doc, "Regresión logística ordinal. ", "Modelo principal (3 variables; AIC = 263.2; IC de perfil): NDP (OR = 2.89; IC 95%: 1.35–6.32; p = 0.007) y tiempo de evolución (OR = 1.08/año; IC 95%: 1.03–1.14; p = 0.001) fueron predictores significativos. La hipertensión arterial mostró tendencia de magnitud relevante (OR = 2.06; IC 95%: 0.98–4.44; p = 0.059). Modelo de sensibilidad 7 variables: AIC = 266.7; LRT p = 0.336 (las 4 variables adicionales no mejoran el ajuste) (Tabla 4; Gráfico 8).")
doc <- lab(doc, "Supuesto de odds proporcionales. ", "La prueba de Brant detectó violación para el tiempo de evolución. El modelo de odds proporcionales parciales (clm, paquete ordinal) evaluó el efecto del tiempo por umbral como análisis de sensibilidad (Tabla 5).")

doc <- h(doc, "Discusión")
doc <- p(doc, "El 60% de los pacientes presentó alteración de la sensibilidad corneal, concordante con prevalencias internacionales (47–64%) [8]. Dua et al. reportaron 23.4% con estesiómetro de Cochet-Bonnet [13]; la diferencia se explica porque el método clínico incluye grados leves. El perfil metabólico converge con Singer et al. en población hispana [14].")
doc <- p(doc, "El tiempo de evolución fue el predictor más consistente (ρ = 0.381; OR = 1.10/año), coherente con el daño axonal acumulado por hiperglucemia crónica, vía del aldosa-reductasa, AGEs y estrés oxidativo [15], y con Lv et al. y Malik et al. [9,12]. La NPD mostró la asociación más fuerte (OR bivariado 3.83; multivariado 3.02); la concordancia entre ambos refuerza su robustez, y la atenuación al ajustar es esperable por colinealidad parcial con el tiempo de evolución, manteniendo significación independiente. Respalda el concepto de la córnea como ventana de la neuropatía de fibras pequeñas, cuyos cambios pueden preceder a otros signos [8,16].")
doc <- p(doc, "La hipertensión arterial emergió como predictor independiente (OR = 2.47), por daño microvascular aditivo de la coexistencia DM2–hipertensión [17]. No hubo asociación con HbA1c ni glucemia: una medición puntual no captura la carga glucémica histórica, y la distribución homogénea de HbA1c entre grupos limitó la detección de un gradiente; resultados similares en Kiyat et al. [18]. El método con algodón resultó aplicable y reproducible [11,19,20].")
doc <- p(doc, "Metodológicamente, el modelo de odds proporcionales se mantuvo como análisis primario por parsimonia; la prueba de Brant reveló incumplimiento para el tiempo de evolución (p = 0.030), por lo que el modelo parcial (mejor ajuste; LRT p = 4.1×10⁻⁵; ΔAIC = 16) muestra que su efecto difiere entre umbrales (Tabla 5), patrón plausible dado el daño axonal acumulativo. El reporte de ambos modelos aporta transparencia y robustez.")
doc <- lab(doc, "Limitaciones. ", "El diseño transversal no infiere causalidad. La ausencia de grupo control no diabético impide comparaciones basales. La muestra, suficiente para las asociaciones principales (potencia 98.8% para la correlación; 86.1% para la NPD), es limitada para el modelo multivariado (IC amplios). La subjetividad se mitigó con estandarización y un único evaluador. La NPD provino del expediente, sin evaluación neurológica estandarizada independiente.")
doc <- lab(doc, "Conclusiones. ", "La alteración de la sensibilidad corneal fue altamente prevalente (60%) y se asoció de forma independiente con la NPD, la hipertensión arterial y el tiempo de evolución, pero no con el control glucémico puntual. Es el primer estudio que la evalúa sistemáticamente en pacientes con DM2 en la República Dominicana mediante un método clínico accesible.")
doc <- lab(doc, "Recomendaciones. ", "Incorporar la evaluación con algodón al seguimiento oftalmológico rutinario del paciente diabético, priorizando DM2 de larga evolución (>10 años), NPD diagnosticada o hipertensión concomitante; capacitar al personal con criterios de derivación; y desarrollar estudios longitudinales y multicéntricos que validen el método frente al estesiómetro de Cochet-Bonnet y la microscopía confocal corneal in vivo.")
doc <- br(doc)

# ============================================================
# REFERENCIAS
# ============================================================
doc <- h(doc, "Referencias")
refs <- c(
"1. American Diabetes Association. Diagnosis and classification of diabetes: Standards of Care in Diabetes—2024. Diabetes Care. 2024;47(Suppl 1):S20–S42.",
"2. Petropoulos IN, Ponirakis G, Khan A, et al. Corneal confocal microscopy: a biomarker for diabetic peripheral neuropathy. Clin Ther. 2021;43(9):1457–1475.",
"3. Oh J. Clinical spectrum and diagnosis of diabetic neuropathies. Korean J Intern Med. 2020;35(5):1059–1069.",
"4. Shaheen BS, Bakir M, Jain S. Corneal nerves in health and disease. Surv Ophthalmol. 2014;59(3):263–285.",
"5. Belmonte C, Acosta MC, Gallar J. Neural basis of sensation in intact and injured corneas. Exp Eye Res. 2004;78(3):513–525.",
"6. Ljubimov AV. Diabetic complications in the cornea. Vision Res. 2017;139:138–152.",
"7. Zhou T, Lee A, Lo ACY, Kwok JSWJ. Diabetic corneal neuropathy: pathogenic mechanisms and therapeutic strategies. Front Pharmacol. 2022;13:816062.",
"8. So WZ, Wong NSQ, Tan HC, et al. Diabetic corneal neuropathy as a surrogate marker for diabetic peripheral neuropathy. Neural Regen Res. 2022;17(10):2172–2178.",
"9. Lv H, Li A, Zhang X, et al. Meta-analysis and review on the changes of tear function and corneal sensitivity in diabetic patients. Acta Ophthalmol. 2014;92(2):e96–e104.",
"10. Cid-Bertomeu P, Vilaltella M, Capilla L, Huerva V. Impact of diabetic peripheral neuropathy on corneal sensitivity and ocular surface. Ophthalmic Res. 2026;69:56–63.",
"11. Galor A, Lighthizer N. Corneal sensitivity testing procedure for ophthalmologic and optometric patients. J Vis Exp. 2024;(210):e66597.",
"12. Malik RA, Kallinikos P, Abbott CA, et al. Corneal confocal microscopy: a non-invasive surrogate of nerve fibre damage and repair in diabetic patients. Diabetologia. 2003;46(5):683–688.",
"13. Dua HS, Said DG, Otri M, et al. Dry eye and corneal sensitivity in diabetic patients. Indian J Ophthalmol. 2025;73(2):88–94.",
"14. Singer M, Ashimatey BS, Zhou X, et al. Corneal sensitivity in diabetic retinopathy: a prospective study with a predominantly Hispanic population. Retina. 2024;44(5):801–808.",
"15. Albers JW, Pop-Busui R. Diabetic neuropathy: mechanisms, emerging treatments, and subtypes. Curr Neurol Neurosci Rep. 2014;14(8):473.",
"16. Bitirgen G, Ozkagnici A, Malik RA, Kerimoglu H. Corneal nerve fibre damage precedes diabetic retinopathy in patients with type 2 diabetes mellitus. Diabet Med. 2014;31(4):431–438.",
"17. Kovacova B, Shotliff K. Eye problems in people with diabetes: more than just diabetic retinopathy. Pract Diabetes. 2022;39(4):27–32.",
"18. Kiyat P, Kose T, Gümüstas B, Barut Selver O. Evaluation of corneal sensitivity and quadrature variability in patients with diabetic neuropathy. Middle East Afr J Ophthalmol. 2022;29(4):200–205.",
"19. Crabtree JR, Tannir S, Tran K, et al. Corneal nerve assessment by aesthesiometry: history, advancements, and future directions. Vision (Basel). 2024;8(2):34.",
"20. Kalteniece A, Ferdousi M, Azmi S, et al. Corneal nerve assessment by aesthesiometry. Diagnostics (Basel). 2025;15:1785."
)
for (r in refs) doc <- p(doc, r)
doc <- br(doc)

# ============================================================
# LEYENDAS DE ILUSTRACIONES
# ============================================================
doc <- h(doc, "Leyendas de ilustraciones")
leyendas <- c(
"Tabla 1. Características generales de la muestra (N = 113). Media ± DE; n (%).",
"Tabla 2. Variables clínicas según severidad de sensibilidad corneal. Kruskal-Wallis (continuas); Fisher (categóricas).",
"Tabla 3. Correlación de Spearman entre variables continuas y severidad corneal.",
"Tabla 4. Modelo de regresión logística ordinal: OR ajustados e IC 95% (n = 113; AIC = 266.7).",
"Tabla 5. Análisis de sensibilidad: OR del tiempo de evolución por umbral (modelo de odds proporcionales parciales, clm). IC 95% Wald.",
"Gráfico 1. Distribución de glucemia reciente (mg/dL) (n = 113).",
"Gráfico 2. Distribución de HbA1c (%) (n = 113).",
"Gráfico 3. Tiempo de evolución de DM2 según severidad corneal (Kruskal-Wallis p < 0.001).",
"Gráfico 4. HbA1c según severidad corneal (Kruskal-Wallis p = 0.70).",
"Gráfico 5. Correlación entre tiempo de evolución y severidad corneal (ρ = 0.381; p < 0.001).",
"Gráfico 6. Tiempo de evolución según alteración corneal (Mann-Whitney p = 0.048).",
"Gráfico 7. Sensibilidad alterada según neuropatía periférica diabética (Fisher p = 0.002).",
"Gráfico 8. Odds ratio e IC 95% del modelo ordinal. La línea vertical marca OR = 1."
)
for (l in leyendas) doc <- p(doc, l)
doc <- br(doc)

# ============================================================
# ILUSTRACIONES — TABLAS
# ============================================================
doc <- h(doc, "Ilustraciones")

mk <- function(df) flextable(df) |> bold(part="header") |>
  set_table_properties(layout="autofit", width=1) |> fontsize(size=10, part="all")

t1 <- data.frame(Variable=c("Edad (años)","Sexo femenino","Sexo masculino","Tiempo de evolución DM2 (años)",
  "HbA1c (%)","Glucemia reciente (mg/dL)","Hipertensión arterial (sí)","Dislipidemia (sí)",
  "Neuropatía periférica diabética (sí)","Nefropatía diabética (sí)","Sensibilidad corneal: normal",
  "Sensibilidad corneal: leve","Sensibilidad corneal: moderada","Sensibilidad corneal: severa"),
  `N = 113`=c("58.5 ± 11.2","72 (64%)","41 (36%)","11.6 ± 7.9","9.0 ± 2.5","190.3 ± 87.7","69 (61%)",
  "36 (32%)","39 (35%)","33 (29%)","45 (40%)","40 (35%)","18 (16%)","10 (8.8%)"), check.names=FALSE)
doc <- body_add_flextable(doc, set_caption(mk(t1), "Tabla 1. Características generales de la muestra (N = 113)."))
doc <- br(doc)

t2 <- data.frame(Variable=c("Edad (años)","HbA1c (%)","Glucemia (mg/dL)","Tiempo evolución DM2 (años)","NPD: no","NPD: sí"),
  `Normal (45)`=c("58 ± 13","8.75 ± 2.48","186 ± 93","10 ± 8","37 (82%)","8 (18%)"),
  `Leve (40)`=c("57 ± 10","9.26 ± 2.48","197 ± 87","9 ± 6","26 (65%)","14 (35%)"),
  `Moderada (18)`=c("64 ± 8","9.15 ± 2.35","181 ± 75","18 ± 7","7 (39%)","11 (61%)"),
  `Severa (10)`=c("58 ± 9","9.22 ± 2.97","198 ± 99","20 ± 4","4 (40%)","6 (60%)"),
  p=c("0.10","0.70","0.80","<0.001","0.002",""), check.names=FALSE)
doc <- body_add_flextable(doc, set_caption(mk(t2), "Tabla 2. Variables clínicas según severidad de sensibilidad corneal."))
doc <- br(doc)

t3 <- data.frame(Variable=c("Tiempo de evolución DM2 (años)","HbA1c (%)","Glucemia reciente (mg/dL)"),
  `ρ de Spearman`=c("0.381","0.085","0.053"), p=c("<0.001","0.373","0.577"), check.names=FALSE)
doc <- body_add_flextable(doc, set_caption(mk(t3), "Tabla 3. Correlación de Spearman con la severidad corneal."))
doc <- br(doc)

t4 <- data.frame(Variable=c("Tiempo de evolución DM2 (años)","Neuropatía diabética periférica","Hipertensión arterial"),
  `OR ajustado`=c("1.084","2.892","2.064"),
  `IC 95% (perfil)`=c("1.033 – 1.140","1.351 – 6.319","0.980 – 4.442"),
  p=c("0.001","0.007","0.059"), check.names=FALSE)
doc <- body_add_flextable(doc, set_caption(mk(t4), "Tabla 4. Modelo de regresión logística ordinal principal (3 variables; n = 113; AIC = 263.2). IC de perfil. Modelo de sensibilidad 7 variables AIC = 266.7, LRT p = 0.336."))
doc <- br(doc)

# Tabla 5 — desde el CSV que produce tu script (sección 15d). Si no existe, deja aviso.
t5_path <- "resultados/or_tiempo_ppo_clm.csv"
if (file.exists(t5_path)) {
  t5 <- read.csv(t5_path, check.names = FALSE, fileEncoding = "UTF-8")
  doc <- body_add_flextable(doc, set_caption(mk(t5),
    "Tabla 5. Análisis de sensibilidad: OR del tiempo de evolución DM2 por umbral de severidad (modelo de odds proporcionales parciales, clm). IC 95% de Wald. Ver texto para LRT y ΔAIC."))
} else {
  doc <- p(doc, "[Tabla 5 — corre cornea_sens_dm2.R para generar resultados/or_tiempo_ppo_clm.csv]")
  message("AVISO: falta ", t5_path)
}
doc <- br(doc)

# ============================================================
# ILUSTRACIONES — FIGURAS (lee los PNG que ya genera tu script)
# ============================================================
fig_dir <- "figuras/figuras 2"
figs <- c(
  "01_histograma_glucemia.png", "02_histograma_hba1c.png",
  "03_boxplot_tiempo_severidad.png", "04_boxplot_hba1c_severidad.png",
  "05_scatter_tiempo_severidad.png", "06_boxplot_tiempo_binario.png",
  "07_barras_neuropatia_sensibilidad.png", "08_forest_plot_ordinal_7v.png")

add_fig <- function(doc, file, w = 6.3, hgt = 4.33) {
  ruta <- file.path(fig_dir, file)
  doc <- br(doc)
  if (file.exists(ruta)) {
    doc <- body_add_img(doc, src = ruta, width = w, height = hgt)
  } else {
    doc <- p(doc, paste0("[Falta la imagen: ", ruta, " — corre tu script de análisis]"))
    message("AVISO: no se encontró ", ruta)
  }
  doc
}
for (i in seq_along(figs)) {
  hh <- if (i == 8) 3.85 else 4.33   # forest plot tiene otra proporción
  doc <- add_fig(doc, figs[i], w = 6.3, hgt = hh)
}

# ============================================================
# GUARDAR
# ============================================================
salida <- "articulo_oftalrev_generado.docx"
print(doc, target = salida)
message("✓ Documento generado: ", normalizePath(salida))
