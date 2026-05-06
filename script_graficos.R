# script_graficos.R
# Genera el PDF con todos los gráficos e interpretaciones.
# Ejecutar DESPUÉS de script_analisis.R (necesita las variables en memoria).

# Abrimos el PDF
pdf("graficos_colesterol.pdf", width = 10, height = 7)


# PÁGINA 1: Histograma + KDE

k <- ceiling(1 + 3.22 * log10(n))

layout(matrix(c(1, 2), nrow = 1), widths = c(2, 1.3))
par(mar = c(5, 4, 4, 1))

hist(datos,
     breaks = k,
     freq = FALSE,
     main = "Histograma con curva de densidad (KDE)",
     xlab = "Colesterol (mg/dL)",
     ylab = "Densidad",
     col = "lightsteelblue",
     border = "white")
lines(density(datos), col = "firebrick", lwd = 2)
abline(v = media, col = "darkorange", lty = 2, lwd = 2)
abline(v = mediana, col = "purple", lty = 2, lwd = 2)
legend("topright",
       legend = c("KDE",
                  paste("Media:", round(media, 1)),
                  paste("Mediana:", mediana)),
       col = c("firebrick", "darkorange", "purple"),
       lty = c(1, 2, 2), lwd = 2, cex = 0.8)

par(mar = c(5, 1, 4, 1))
plot.new()
text(0.05, 0.95, "Interpretación", font = 2, adj = 0, cex = 1.1)
text(0.05, 0.80, paste0(
  "Se observa una distribución aproximadamente\n",
  "simétrica con leve asimetría positiva.\n",
  "La media (", round(media, 1), " mg/dL) se sitúa\n",
  "ligeramente a la derecha de la mediana\n",
  "(", mediana, " mg/dL), indicando la presencia\n",
  "de algunos valores elevados que tiran de\n",
  "la media hacia arriba.\n\n",
  "La curva KDE suaviza el histograma y\n",
  "muestra una forma cercana a la campana\n",
  "de Gauss, parecida a una distribución\n",
  "normal.\n\n",
  "Nº de bins (Sturges): ", k),
  adj = c(0, 1), cex = 0.85)


# PÁGINA 2: Boxplot

layout(matrix(c(1, 2), nrow = 1), widths = c(1.5, 1.5))
par(mar = c(5, 4, 4, 1))

boxplot(datos,
        main = "Boxplot de niveles de colesterol",
        ylab = "Colesterol (mg/dL)",
        col = "lightsteelblue",
        outcol = "firebrick",
        outpch = 19,
        cex = 1.2)

lim_inf <- Q1 - 1.5 * IQR_valor
lim_sup <- Q3 + 1.5 * IQR_valor
outliers <- datos[datos < lim_inf | datos > lim_sup]

par(mar = c(5, 1, 4, 1))
plot.new()
text(0.05, 0.95, "Interpretación", font = 2, adj = 0, cex = 1.1)
text(0.05, 0.80, paste0(
  "La caja se extiende de Q1 = ", Q1, " a\n",
  "Q3 = ", Q3, " mg/dL, con mediana en ", mediana, ".\n",
  "El IQR = ", IQR_valor, " mg/dL indica la\n",
  "dispersión del 50% central de los datos.\n\n",
  "La mediana está ligeramente desplazada\n",
  "hacia la parte inferior de la caja,\n",
  "confirmando la leve asimetría positiva.\n\n",
  "Se detecta ", length(outliers), " outlier: ", max(outliers), " mg/dL,\n",
  "que supera el límite superior\n",
  "(Q3 + 1.5*IQR = ", lim_sup, " mg/dL).\n",
  "Este valor corresponde a un caso de\n",
  "hipercolesterolemia severa."),
  adj = c(0, 1), cex = 0.85)


# PÁGINA 3: QQ-Plot

layout(matrix(c(1, 2), nrow = 1), widths = c(1.5, 1.5))
par(mar = c(5, 4, 4, 1))

qqnorm(datos,
       main = "QQ-Plot: diagnóstico de normalidad",
       pch = 19, col = "steelblue", cex = 0.8)
qqline(datos, col = "firebrick", lwd = 2)

par(mar = c(5, 1, 4, 1))
plot.new()
text(0.05, 0.95, "Interpretación", font = 2, adj = 0, cex = 1.1)
text(0.05, 0.80, paste0(
  "El QQ-Plot compara los cuantiles de\n",
  "los datos con los de una distribución\n",
  "normal teórica.\n\n",
  "Los puntos se ajustan razonablemente\n",
  "a la línea de referencia, lo que indica\n",
  "que los datos son compatibles con una\n",
  "distribución normal.\n\n",
  "En los extremos (especialmente el valor\n",
  "345 mg/dL) se observan desviaciones,\n",
  "consistentes con la ligera asimetría\n",
  "positiva detectada (coef. = ", round(asimetria, 4), ").\n\n",
  "Esto sugiere que una distribución normal\n",
  "es una buena aproximación para estos\n",
  "datos, aunque con una cola derecha\n",
  "ligeramente más pesada."),
  adj = c(0, 1), cex = 0.85)


# PÁGINA 4: Gráfico de barras por riesgo

niveles <- cut(datos,
               breaks = c(-Inf, 200, 240, Inf),
               labels = c("Deseable\n(<200)", "Moderado\n(200-239)", "Alto\n(>=240)"),
               right = FALSE)
tabla_niveles <- table(niveles)
porcentajes <- round(prop.table(tabla_niveles) * 100, 1)

layout(matrix(c(1, 2), nrow = 1), widths = c(1.5, 1.5))
par(mar = c(5, 4, 4, 1))

bp <- barplot(tabla_niveles,
              col = c("seagreen3", "orange", "firebrick"),
              main = "Distribución por categorías de riesgo",
              ylab = "Número de individuos",
              ylim = c(0, 70))
text(bp, tabla_niveles + 2,
     labels = paste0(tabla_niveles, " (", porcentajes, "%)"),
     cex = 0.9, font = 2)

par(mar = c(5, 1, 4, 1))
plot.new()
text(0.05, 0.95, "Interpretación", font = 2, adj = 0, cex = 1.1)
text(0.05, 0.80, paste0(
  "Clasificación según umbrales clínicos:\n\n",
  "- Deseable (<200):   ", n_deseable, " individuos (",
  round(n_deseable/n*100, 1), "%)\n",
  "- Moderado (200-239): ", n_moderado, " individuos (",
  round(n_moderado/n*100, 1), "%)\n",
  "- Alto (>=240):       ", n_alto, " individuos (",
  round(n_alto/n*100, 1), "%)\n\n",
  "Un ", round((n_moderado + n_alto)/n*100, 1), "% de la muestra\n",
  "supera el nivel deseable de 200 mg/dL.\n\n",
  "Casi un tercio (31.1%) presenta niveles\n",
  "considerados altos. Estos datos son\n",
  "preocupantes desde el punto de vista\n",
  "clínico."),
  adj = c(0, 1), cex = 0.85)


# PÁGINA 5: Resumen y conclusiones (todo en una página)

par(mfrow = c(1, 1), mar = c(2, 2, 3, 2))
plot.new()

text(0.5, 0.97, "Resumen y conclusiones\n\n", font = 2, cex = 1.4, adj = 0.5)

# Columna izquierda: estadísticos
text(0.05, 0.89, "Estadísticos descriptivos", font = 2, cex = 1.0, adj = 0)
text(0.05, 0.85, paste0("Media:       ", round(media, 2), " mg/dL"), adj = 0, cex = 0.8)
text(0.05, 0.82, paste0("Mediana:     ", mediana, " mg/dL"), adj = 0, cex = 0.8)
text(0.05, 0.79, paste0("Desv. típica: ", round(desv_tipica, 2), " mg/dL"), adj = 0, cex = 0.8)
text(0.05, 0.76, paste0("Varianza:    ", round(varianza, 2)), adj = 0, cex = 0.8)
text(0.05, 0.73, paste0("Q1: ", Q1, "  |  Q3: ", Q3), adj = 0, cex = 0.8)
text(0.05, 0.70, paste0("IQR: ", IQR_valor, "  |  Rango: ", rango), adj = 0, cex = 0.8)
text(0.05, 0.67, paste0("CV: ", round(cv, 2), "%"), adj = 0, cex = 0.8)
text(0.05, 0.64, paste0("Asimetría: ", round(asimetria, 4)), adj = 0, cex = 0.8)
text(0.05, 0.61, paste0("Exc. curtosis: ", round(curtosis_exc, 4)), adj = 0, cex = 0.8)

# Columna derecha: riesgo clínico
text(0.55, 0.89, "Riesgo clínico", font = 2, cex = 1.0, adj = 0)
text(0.55, 0.85, paste0("Deseable (<200):   ", n_deseable, " (", round(n_deseable/n*100, 1), "%)"), adj = 0, cex = 0.8)
text(0.55, 0.82, paste0("Moderado (200-239): ", n_moderado, " (", round(n_moderado/n*100, 1), "%)"), adj = 0, cex = 0.8)
text(0.55, 0.79, paste0("Alto (>=240):      ", n_alto, " (", round(n_alto/n*100, 1), "%)"), adj = 0, cex = 0.8)
text(0.55, 0.74, paste0("Superan deseable:  ", round((n_moderado + n_alto)/n*100, 1), "%"), adj = 0, cex = 0.8, font = 2)
text(0.55, 0.69, paste0("Outlier detectado: 345 mg/dL"), adj = 0, cex = 0.8)



# Conclusiones (abajo)
text(0.05, 0.50, "Conclusiones", font = 2, cex = 1.0, adj = 0)
text(0.05, 0.45, "1. La media muestral (221.59 mg/dL) se sitúa en zona de riesgo moderado.", adj = 0, cex = 0.8)
text(0.05, 0.41, "2. La distribución es aproximadamente normal con leve asimetría positiva (media > mediana).", adj = 0, cex = 0.8)
text(0.05, 0.37, "3. La curva KDE y el QQ-Plot confirman que la normal es una buena aproximación.", adj = 0, cex = 0.8)
text(0.05, 0.33, paste0("4. El ", round((n_moderado + n_alto)/n*100, 1), "% de la muestra supera el umbral deseable de 200 mg/dL."), adj = 0, cex = 0.8)
text(0.05, 0.29, paste0("5. El ", round(n_alto/n*100, 1), "% presenta niveles altos (>=240 mg/dL)."), adj = 0, cex = 0.8)
text(0.05, 0.25, "6. Se detecta 1 outlier (345 mg/dL): hipercolesterolemia severa.", adj = 0, cex = 0.8)
text(0.05, 0.21, "7. Los resultados son clínicamente preocupantes y sugerirían medidas preventivas.", adj = 0, cex = 0.8)


# Cerramos el PDF
dev.off()

cat("PDF generado: graficos_colesterol.pdf\n")
cat("Búscalo en la pestaña Files de RStudio.\n")