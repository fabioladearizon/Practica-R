# Práctica R - Análisis de Niveles de Colesterol Total

**Asignatura:** Estadística  
**Universidad Complutense de Madrid**

## Descripción

Análisis estadístico descriptivo de una muestra de 135 observaciones de niveles de colesterol total (mg/dL). Se estudia la distribución de los datos, se calculan estadísticos descriptivos, se representan gráficamente y se valora el riesgo clínico de la muestra según los umbrales médicos habituales.

## Contenido del repositorio

| Archivo | Descripción |
|---------|-------------|
| `colesterol.xlsx` | Datos originales (135 observaciones) |
| `script_analisis.R` | Cálculos estadísticos e interpretación en consola |
| `script_graficos.R` | Genera un PDF con 4 gráficos interpretados y una página de resumen |
| `graficos_colesterol.pdf` | PDF generado con los gráficos y conclusiones |

## Cómo ejecutar

1. Abrir el proyecto en RStudio
2. Asegurarse de que `colesterol.xlsx` está en la carpeta del proyecto
3. Instalar los paquetes necesarios:

```r
install.packages(c("readxl", "moments"))
```

4. Ejecutar `script_analisis.R` → resultados en consola
5. Sin cerrar RStudio, ejecutar `script_graficos.R` → genera `graficos_colesterol.pdf`

## Análisis realizado

### Estadísticos descriptivos
- Medidas de centralidad: media, mediana, moda
- Medidas de dispersión: varianza, desviación típica, rango, IQR, coeficiente de variación
- Medidas de forma: coeficiente de asimetría de Fisher, exceso de curtosis
- Detección de outliers (criterio 1.5 × IQR)

### Análisis gráfico (4 tipos)
- Histograma con curva de densidad (KDE)
- Boxplot con detección de outliers
- QQ-Plot para diagnóstico visual de normalidad
- Gráfico de barras por categorías de riesgo clínico
