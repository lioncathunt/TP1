library(tidyverse)

# --- TABLA II (Barnes & LaMer, 1942) ---
# Condiciones: [NaOH] = 0.012 N, lambda = 550 mmu, Espesor = 20.054 mm

# 1. Carga de los datos experimentales de la Tabla II
# NOTA: Al tiempo t = 0 min, la transmitancia (21.2%) es un valor extrapolado por los autores.
datos_tabla2 <- data.frame(
  tiempo_min = c(0, 11.17, 25.58, 46.83, 52.00, 73.93, 78.00, 85.58, 89.83, 112.58, 119.33, 139.58, 142.33, 159.67, 170.08, 174.33),
  T_porcentaje = c(21.2, 22.9, 25.0, 27.8, 28.4, 30.8, 31.25, 32.0, 32.4, 34.2, 34.8, 36.0, 36.3, 37.1, 37.8, 37.95)
)

# Transmitancia en el equilibrio medida a las 24 horas (indicada al pie de la Tabla II y Tabla IV)
T_equilibrio <- 42.2

# 2. Procesamiento de las variables logarítmicas según el paper
# Convertimos el porcentaje de Transmitancia a escala decimal para el cálculo químico (T / 100)
datos_tabla2$log_T_t   <- log10(datos_tabla2$T_porcentaje / 100)
log_T_e                <- log10(T_equilibrio / 100)

# El paper grafica el término log(log T_t - log T_e)
# Como ambos logaritmos son negativos (valores < 1), la resta (log T_t - log T_e) da positiva.
datos_tabla2$y_figura2 <- log10(datos_tabla2$log_T_t - log_T_e)

# 3. Ajuste Lineal por Cuadrados Mínimos (Regresión Lineal)
ajuste <- lm(y_figura2 ~ tiempo_min, data = datos_tabla2)
summary_ajuste <- summary(ajuste)

# Imprimir los resultados del ajuste en la consola de R
print("--- Resultados de la Regresión Lineal ---")
print(summary_ajuste)

# 4. Graficación de la Figura 2 (Linealización de Pseudo-Primer Orden)
# Configuramos el lienzo
plot(datos_tabla2$tiempo_min, datos_tabla2$y_figura2,
     pch = 21, bg = "lightblue", col = "darkblue", cex = 1.3, lw = 1.5,
     xlab = "Minutes", 
     ylab = "Log (log T_t - log T_e)",
     main = "Recreación de la Figura 2 (Datos Reales de la Tabla II)",
     sub = paste("R² =", round(summary_ajuste$r.squared, 5)))

# Añadimos la recta de ajuste calculada por R
abline(ajuste, col = "red", lty = 1, lw = 2)

# Agregamos una grilla para que se parezca al papel milimetrado del paper original
grid(nx = NULL, ny = NULL, col = "gray", lty = "dotted")