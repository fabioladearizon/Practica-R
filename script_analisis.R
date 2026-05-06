# script_analisis.R
# Cálculos estadísticos descriptivos
# Ejecutar primero. Los resultados se imprimen en la consola.

library(readxl)
library(moments)

# Carga de datos
colesterol <- read_excel("colesterol.xlsx")
datos <- colesterol$`Niveles de colesterol`
n <- length(datos)

cat("  ANÁLISIS DE COLESTEROL TOTAL\n")
cat("  n =", n, "individuos\n")


# 1. MEDIDAS DE CENTRALIDAD

media <- mean(datos)
mediana <- median(datos)

calcular_moda <- function(x) {
  tabla <- table(x)
  modas <- as.numeric(names(tabla)[tabla == max(tabla)])
  return(modas)
}
moda <- calcular_moda(datos)

cat("\n-- Medidas de centralidad --\n")
cat("Media:   ", round(media, 2), "mg/dL\n")
cat("Mediana: ", mediana, "mg/dL\n")
cat("Moda:    ", moda, "mg/dL\n")


# 2. MEDIDAS DE DISPERSIÓN

varianza <- var(datos)
desv_tipica <- sd(datos)
rango <- diff(range(datos))

cuartiles <- quantile(datos, probs = c(0.25, 0.50, 0.75))
Q1 <- as.numeric(cuartiles[1])
Q3 <- as.numeric(cuartiles[3])
IQR_valor <- Q3 - Q1
cv <- (desv_tipica / media) * 100

cat("\n-- Medidas de dispersión --\n")
cat("Varianza (s²):  ", round(varianza, 2), "\n")
cat("Desv. típica (s):", round(desv_tipica, 2), "mg/dL\n")
cat("Rango:           ", rango, "mg/dL\n")
cat("Q1 (25%):        ", Q1, "mg/dL\n")
cat("Q3 (75%):        ", Q3, "mg/dL\n")
cat("IQR:             ", IQR_valor, "mg/dL\n")
cat("CV:              ", round(cv, 2), "%\n")


# 3. MEDIDAS DE FORMA

asimetria <- skewness(datos)
curtosis_exc <- kurtosis(datos) - 3

cat("\n-- Medidas de forma --\n")
cat("Asimetría (Fisher):", round(asimetria, 4), "\n")
cat("Exceso de curtosis:", round(curtosis_exc, 4), "\n")


# 4. CLASIFICACIÓN POR RIESGO CLÍNICO

n_deseable <- sum(datos < 200)
n_moderado <- sum(datos >= 200 & datos < 240)
n_alto <- sum(datos >= 240)

cat("\n-- Clasificación por riesgo --\n")
cat("Deseable  (<200):    ", n_deseable, "(", round(n_deseable/n*100, 1), "%)\n")
cat("Moderado  (200-239): ", n_moderado, "(", round(n_moderado/n*100, 1), "%)\n")
cat("Alto      (>=240):   ", n_alto, "(", round(n_alto/n*100, 1), "%)\n")
cat("Superan el deseable: ", round((n_moderado + n_alto)/n*100, 1), "%\n")


# 5. DETECCIÓN DE OUTLIERS (criterio boxplot: 1.5*IQR)

lim_inf <- Q1 - 1.5 * IQR_valor
lim_sup <- Q3 + 1.5 * IQR_valor
outliers <- datos[datos < lim_inf | datos > lim_sup]

cat("\n-- Outliers (criterio 1.5*IQR) --\n")
cat("Límite inferior:", lim_inf, "mg/dL\n")
cat("Límite superior:", lim_sup, "mg/dL\n")
cat("Detectados:", length(outliers), "\n")
if (length(outliers) > 0) cat("Valores:", outliers, "mg/dL\n")

cat("  Análisis completado.\n")
cat("  Ahora ejecuta script_graficos.R\n")