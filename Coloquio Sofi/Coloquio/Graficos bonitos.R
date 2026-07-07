library(tidyverse)

# Determinacion de k'_1+k_2 ----


#Condiciones: [NaOH] = 0.012 N, lambda = 550 nm, Espesor = 20.054 mm

#Datos experimentales de la Tabla II
#La transmitancia (21.2%) a tiempo t = 0 min es un valor extrapolado por los autores.

datos_tabla2 <- data.frame(
  tiempo_min = c(0, 11.17, 25.58, 46.83, 52.00, 73.93, 78.00, 85.58, 89.83, 112.58, 119.33, 139.58, 142.33, 159.67, 170.08, 174.33),
  T_porcentaje = c(21.2, 22.9, 25.0, 27.8, 28.4, 30.8, 31.25, 32.0, 32.4, 34.2, 34.8, 36.0, 36.3, 37.1, 37.8, 37.95)
)

T_equilibrio <- 42.2 # Transmitancia en el equilibrio medida a las 24 horas 


# Convertimos el porcentaje de Transmitancia y calculamos los logaritmos 
datos_tabla2$A_t   <- -log10(datos_tabla2$T_porcentaje / 100)
A_e                <- -log10(T_equilibrio / 100)


# En el paper grafica el término log(log T_t - log T_e) pero debe ser un error de tipeo 
#sino log T_t - log T_e resulta negativo porque siempre log T_e > log T_t.

datos_tabla2$ln_dif_A <- log(datos_tabla2$A_t - A_e)


#Grafico de la Figura 2 usando absorbancias (en el paper se usa log T)

Figura_2_paper <- ggplot(data = datos_tabla2, 
       aes(x=tiempo_min, 
           y=ln_dif_A)
       )+
  labs(
    title = "Figura 1 - Análisis de cinética de Pseudo-Primer Orden",
    x = "Tiempo (min)",
    y = expression(ln~(A[t] - A[e])) 
    )+
  geom_point(size=2, color="black")+
  theme_bw()+
  geom_smooth(method = "lm", color="magenta3")+
  stat_regline_equation(label.x = 100, label.y = -1.5)

Figura_2_paper 

lm_plot_figura_2_paper<-lm(datos_tabla2$ln_dif_A~datos_tabla2$tiempo_min)
summary(lm_plot_figura_2_paper)




# Determinacion de la constante de equilibrio -----------------------------

T_0 <- 0.212   
T_e <- 0.422            
NaOH <- 0.012        

A_0 <- -log10(T_0)
A_e <- -log10(T_e)

Kc_calculada <- (A_0 - A_e) / (A_e * NaOH)

print(Kc_calculada)





# Calculo de carga de la fenolftaleina -----------------------------------------------------------

# Datos extraídos de las Tablas IV y V para evaluar el Efecto Salino Primario a 
# fuerza ionica constante
# Condiciones experimentales: Temperatura = 25°C, lambda = 550 nm


NaOH_M <- c(0.006, 0.008, 0.010, 0.012, 0.014, 0.016, 0.020)

# k1' + k2 (en min^-1 * 10^2)
k1_prima_mas_k2 <- c(0.7568, 0.8560, 0.9638, 1.067, 1.195, 1.331, 1.622) * 10^-2

# Constante de equilibrio Kc medida

Kc<-c(61.78, 63.60, 65.25, 66.52, 67.86, 69.06, 71.00)
tabla_naoh <- data.frame(NaOH_M,k1_prima_mas_k2,Kc)
tabla_naoh$k1 <- k1_prima_mas_k2/(NaOH_M+(1/Kc))

# Grafico Bronsted-Debye

# Para un electrolito 1:1 como el NaOH (Na+ y OH-), la fuerza iónica (mu) 
# es exactamente igual a su molaridad: mu = 0.5 * (1^2 * C + (-1)^2 * C) = C
tabla_naoh$fuerza_ionica <- tabla_naoh$NaOH_M * 1
tabla_naoh$raiz_mu       <- sqrt(tabla_naoh$fuerza_ionica)

# Aplicamos logaritmo base 10 a las constantes para las gráficas cinético-electrostáticas
tabla_naoh$log_k1 <- log10(tabla_naoh$k1)
tabla_naoh$log_Kc <- log10(tabla_naoh$Kc)



Bronsted_Debye <- ggplot(data = tabla_naoh, aes(x=raiz_mu, y=log_k1))+
  labs(
    title = "Figura 2 - Grafico de Bronsted-Debye",
    x = expression(sqrt(mu)),
    y = expression(log~(k[1]))
  )+
  geom_point(size=2, color="black")+
  theme_bw()+
  geom_smooth(method = "lm", color="magenta3")+
  stat_regline_equation(label.x = 0.12, label.y = -0.425)

Bronsted_Debye

lm_plot_figura_3_paper<-lm(tabla_naoh$log_k1~tabla_naoh$raiz_mu)
summary(lm_plot_figura_3_paper)





# Analisis de la influencia de la fuerza ionica  ------------------------------------------------

#FIGURAS 3 Y 4 del paper
  
  # DATOS EXPERIMENTALES (Tabla IV)

  # Serie 1: Variación con NaOH
  serie_naoh <- data.frame(
    NaOH_M = c(0.006, 0.008, 0.010, 0.012, 0.014, 0.016, 0.020),
    Kc     = c(61.78, 63.60, 65.25, 66.52, 67.86, 69.06, 71.00),
    k1     = c(34.12, 36.09, 38.06, 39.47, 41.59, 43.66, 47.59) * 10^-2
  )
serie_naoh$mu      <- serie_naoh$NaOH_M
serie_naoh$sqrt_mu <- sqrt(serie_naoh$mu)

# Serie 2: Variación NaOH + NaCl
serie_sal <- data.frame(
  mu_total = c(8.0, 10.0, 12.0, 14.0, 16.0, 20.0) * 10^-3,
  Kc       = c(63.99, 65.20, 66.41, 68.36, 70.27, 73.43), 
  k1       = c(36.05, 37.80, 39.33, 40.86, 42.10, 43.55) * 10^-2
)
serie_sal$sqrt_mu <- sqrt(serie_sal$mu_total)

#Aplicamos los logaritmos
serie_naoh$log_k1 <- log10(serie_naoh$k1)
serie_naoh$log_Kc <- log10(serie_naoh$Kc)

serie_sal$log_k1  <- log10(serie_sal$k1)
serie_sal$log_Kc  <- log10(serie_sal$Kc)


par(mfrow = c(1, 2))

# CINETICA (log k1 vs sqrt(mu))

# Combinamos los datos para calcular la pendiente 
datos_cinetica <- data.frame(
  x = c(serie_naoh$sqrt_mu, serie_sal$sqrt_mu),
  y = c(serie_naoh$log_k1, serie_sal$log_k1)
)
ajuste_fig3 <- lm(y ~ x, data = datos_cinetica)
pend_fig3   <- coef(ajuste_fig3)["x"]

plot(NULL, xlim = c(0.07, 0.16), ylim = c(-0.50, -0.30),
     xlab = expression(sqrt(mu)), ylab = expression(log ~ k[1]),
     main = "Figura 2: Efecto Salino Primario")

# Cruces para NaOH, Círculos para solo NaOH + NaCl 
points(serie_sal$sqrt_mu, serie_sal$log_k1, pch = 21, bg = "white", col = "black", cex = 1.2)
points(serie_naoh$sqrt_mu, serie_naoh$log_k1,pch = 4, col = "darkblue", lw = 2, cex = 1.2)

abline(ajuste_fig3, col = "magenta3", lty = 1, lw = 2)
grid(col = "gray85", lty = "dashed")
text(0.12, -0.46, paste("Pendiente =", round(pend_fig3, 3)), col = "black")

# EQUILIBRIO (log Kc vs sqrt(mu))

# Combinamos los datos de equilibrio y hacemos el ajuste
datos_equilibrio <- data.frame(
  x = c(serie_naoh$sqrt_mu, serie_sal$sqrt_mu),
  y = c(serie_naoh$log_Kc, serie_sal$log_Kc)
)
ajuste_fig4 <- lm(y ~ x, data = datos_equilibrio)
pend_fig4   <- coef(ajuste_fig4)["x"]

plot(NULL, xlim = c(0.07, 0.16), ylim = c(1.75, 1.90),
     xlab = expression(sqrt(mu)), ylab = expression(log ~ K[c]),
     main = "Figura 3: Comportamiento en el Equilibrio")

points(serie_sal$sqrt_mu, serie_sal$log_Kc,pch = 21, bg = "white", col = "black", cex = 1.2)
points(serie_naoh$sqrt_mu, serie_naoh$log_Kc, pch = 4, col = "darkblue", lw = 2, cex = 1.2 )

abline(ajuste_fig4, col = "magenta3", lty = 1, lw = 2)
grid(col = "gray85", lty = "dashed")
text(0.12, 1.78, paste("Pendiente =", round(pend_fig4, 3)), col = "black")


par(mfrow = c(1, 1))

