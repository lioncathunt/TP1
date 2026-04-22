library(tidyverse)
library(reshape2)
library(plotly)
library(readxl)



output_HHH180_orca_TP3 <- read_excel("Resultados orca HHH180/output HHH180 orca TP3.xlsx")
View(output_HHH180_orca_TP3)

output_HHH135_orca_TP3 <- read_excel("Resultados orca HHH135/output HHH135 orca TP3.xlsx")
View(output_HHH135_orca_TP3)

output_HHH90_orca_TP3 <- read_excel("Resultados orca HHH90/output HHH90 orca TP3.xlsx")
View(output_HHH90_orca_TP3)


# Grafico 180 -------------------------------------------------------------

z_matrix180 <- acast(output_HHH180_orca_TP3, dH1H2 ~ dH1H3, value.var = "Eh")
d_h1h2 <- as.numeric(colnames(z_matrix180))
d_h1h3 <- as.numeric(rownames(z_matrix180))

fig_180_contour <- plot_ly(
  x = ~d_h1h2,
  y = ~d_h1h3,
  z = ~z_matrix180,
  type = "contour",
  colorscale = "Jet",
  autocontour = TRUE,
  line = list(smoothing = 0.85, color ="white", width = 0.5)
) %>%
  layout(
    title = "Mapa de Contornos de Energ´ıa Potencial",
    xaxis = list(title = "R(H-H, ˚A)"),
    yaxis = list(title = "R(H-H, ˚A)"),
    plot_bgcolor = "#e5ecf6"
  )
fig_180_contour

fig_180 <- plot_ly(x = ~d_h1h2, y = ~d_h1h3, z = ~z_matrix180) %>%
  add_surface(
    colorscale = "Jet",
    colorbar = list(title = "Energ´ıa (E)")
  ) %>%
  layout(
    title = "SEP (180°) H + H-H -> H-H + H",
    scene = list(
      xaxis = list(title = "R(H-H, ˚A)"),
      yaxis = list(title = "R(H-H, ˚A)"),
      zaxis = list(title = "Energ´ıa (E)")
    )
  )
fig_180



# Grafico 135 -------------------------------------------------------------


z_matrix135 <- acast(output_HHH135_orca_TP3, dH1H2 ~ dH1H3, value.var = "Eh")
d_h1h2 <- as.numeric(colnames(z_matrix135))
d_h1h3 <- as.numeric(rownames(z_matrix135))

fig_135_contour <- plot_ly(
  x = ~d_h1h2,
  y = ~d_h1h3,
  z = ~z_matrix135,
  type = "contour",
  colorscale = "Jet",
  autocontour = TRUE,
  line = list(smoothing = 0.85, color ="white", width = 0.5)
) %>%
  layout(
    title = "Mapa de Contornos de Energıa Potencial",
    xaxis = list(title = "R(H-H, ˚A)"),
    yaxis = list(title = "R(H-H, ˚A)"),
    plot_bgcolor = "#e5ecf6"
  )
fig_135_contour

fig_135 <- plot_ly(x = ~d_h1h2, y = ~d_h1h3, z = ~z_matrix135) %>%
  add_surface(
    colorscale = "Jet",
    colorbar = list(title = "Energıa (E)")
  ) %>%
  layout(
    title = "SEP (135°) H + H-H -> H-H + H",
    scene = list(
      xaxis = list(title = "R(H-H, ˚A)"),
      yaxis = list(title = "R(H-H, ˚A)"),
      zaxis = list(title = "Energıa (E)")
    )
  )
fig_135


# Grafico 90 --------------------------------------------------------------

z_matrix90<-acast(output_HHH90_orca_TP3, dH1H2 ~ dH1H3, value.var = "Eh")
d_
