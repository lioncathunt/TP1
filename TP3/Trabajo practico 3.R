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
d_h1h2_180 <- as.numeric(colnames(z_matrix180))
d_h1h3_180 <- as.numeric(rownames(z_matrix180))

fig_180_contour <- plot_ly(
  x = ~d_h1h2_180,
  y = ~d_h1h3_180,
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

fig_180 <- plot_ly(x = ~d_h1h2_180, y = ~d_h1h3_180, z = ~z_matrix180) %>%
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


# Grafico 180 enhanced ----------------------------------------------------

library(readxl)
Resultados_orca_HHH180_enhanced_TP3 <- read_excel("Resultados orca HHH180 enhanced/Resultados orca HHH180 enhanced.xlsx")
View(Resultados_orca_HHH180_enhanced)

z_matrix180_enhanced <- acast(Resultados_orca_HHH180_enhanced_TP3, dh1h2 ~ dh1h3, value.var = "eh")
d_h1h2_180_enhanced <- as.numeric(colnames(z_matrix180_enhanced))
d_h1h3_180_enhanced <- as.numeric(rownames(z_matrix180_enhanced))

fig_180_enhanced_contour <- plot_ly(
  x = ~d_h1h2_180_enhanced,
  y = ~d_h1h3_180_enhanced,
  z = ~z_matrix180_enhanced,
  type = "contour",
  colorscale = "Jet",
  autocontour = TRUE,
  line = list(smoothing = 0.85, color ="white", width = 0.5)
) %>%
  layout(
    title = "Mapa de Contornos de Energia Potencial",
    xaxis = list(title = "R(H-H, ˚A)"),
    yaxis = list(title = "R(H-H, ˚A)"),
    plot_bgcolor = "#e5ecf6"
  )
fig_180_enhanced_contour

fig_180_enhanced <- plot_ly(x = ~d_h1h2_180_enhanced, y = ~d_h1h3_180_enhanced, z = ~z_matrix180_enhanced) %>%
  add_surface(
    colorscale = "Jet",
    colorbar = list(title = "Energıa (E)")
  ) %>%
  layout(
    title = "SEP (180°) H + H-H -> H-H + H",
    scene = list(
      xaxis = list(title = "R(H-H, ˚A)"),
      yaxis = list(title = "R(H-H, ˚A)"),
      zaxis = list(title = "Energ´ıa (E)")
    )
  )
fig_180_enhanced


# Grafico 180 enhanced 0.1 Angstrom ---------------------------------------
Output_180_enhanced_0_1 <- read_excel("Resultados orca HHH180 enhanced 0.1/Output_180_enhanced_0.1.xlsx")
View(Output_180_enhanced_0_1)

z_matrix180_enhanced_0.1 <- acast(Output_180_enhanced_0_1, dH1H2 ~ dH1H3, value.var = "Eh")
d_h1h2_180_enhanced_0.1 <- as.numeric(colnames(z_matrix180_enhanced_0.1))
d_h1h3_180_enhanced_0.1 <- as.numeric(rownames(z_matrix180_enhanced_0.1))

fig_180_enhanced_0.1_contour <- plot_ly(
  x = ~d_h1h2_180_enhanced_0.1,
  y = ~d_h1h3_180_enhanced_0.1,
  z = ~z_matrix180_enhanced_0.1,
  type = "contour",
  colorscale = "Jet",
  autocontour = TRUE,
  line = list(smoothing = 0.85, color ="white", width = 0.5)
) %>%
  layout(
    title = "Mapa de Contornos de Energia Potencial",
    xaxis = list(title = "R(H-H, ˚A)"),
    yaxis = list(title = "R(H-H, ˚A)"),
    plot_bgcolor = "#e5ecf6"
  )
fig_180_enhanced_0.1_contour

fig_180_enhanced_0.1 <- plot_ly(x = ~d_h1h2_180_enhanced_0.1, y = ~d_h1h3_180_enhanced_0.1, z = ~z_matrix180_enhanced_0.1) %>%
  add_surface(
    colorscale = "Jet",
    colorbar = list(title = "Energıa (E)")
  ) %>%
  layout(
    title = "SEP (180°) H + H-H -> H-H + H",
    scene = list(
      xaxis = list(title = "R(H-H, ˚A)"),
      yaxis = list(title = "R(H-H, ˚A)"),
      zaxis = list(title = "Energ´ıa (E)")
    )
  )
fig_180_enhanced_0.1


#El estado de transicion se encuentra en x=0.9305556, y=0.9305556 y tiene una energia de -1.596486. Con una resolucion de 0.01Angstrom
# Grafico 135 -------------------------------------------------------------


z_matrix135 <- acast(output_HHH135_orca_TP3, dH1H2 ~ dH1H3, value.var = "Eh")
d_h1h2_135 <- as.numeric(colnames(z_matrix135))
d_h1h3_135 <- as.numeric(rownames(z_matrix135))

fig_135_contour <- plot_ly(
  x = ~d_h1h2_135,
  y = ~d_h1h3_135,
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

fig_135 <- plot_ly(x = ~d_h1h2_135, y = ~d_h1h3_135, z = ~z_matrix135) %>%
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


# Grafico 135 enhanced ----------------------------------------------------
Output_HHH135_enhanced <- read_excel("Resultados orca HHH135 enhanced/Output_HHH135_enhanced.xlsx", 
                                     col_types = c("numeric", "numeric", "numeric"))
View(Output_HHH135_enhanced)

z_matrix135_enhanced <- acast(Output_HHH135_enhanced, dH1H2 ~ dH1H3, value.var = "Eh")
d_h1h2_135_enhanced <- as.numeric(colnames(z_matrix135_enhanced))
d_h1h3_135_enhanced <- as.numeric(rownames(z_matrix135_enhanced))

fig_135_enhanced_contour <- plot_ly(
  x = ~d_h1h2_135_enhanced,
  y = ~d_h1h3_135_enhanced,
  z = ~z_matrix135_enhanced,
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
fig_135_enhanced_contour

fig_135_enhanced <- plot_ly(x = ~d_h1h2_135_enhanced, y = ~d_h1h3_135_enhanced, z = ~z_matrix135_enhanced) %>%
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
fig_135_enhanced


# Grafico 135 enhanced 0.1 Angstrom ---------------------------------------

Output_HHH135_enhanced_0.1 <- read_excel("Resultados orca HHH135 enhanced 0.1/Output_HHH135_enhanced_0.1.xlsx", 
                                         col_types = c("numeric", "numeric", "numeric"))
View(Output_HHH135_enhanced_0.1)

z_matrix135_enhanced_0.1 <- acast(Output_HHH135_enhanced_0.1, dH1H2 ~ dH1H3, value.var = "Eh")
d_h1h2_135_enhanced_0.1 <- as.numeric(colnames(z_matrix135_enhanced_0.1))
d_h1h3_135_enhanced_0.1 <- as.numeric(rownames(z_matrix135_enhanced_0.1))

fig_135_enhanced_0.1_contour <- plot_ly(
  x = ~d_h1h2_135_enhanced_0.1,
  y = ~d_h1h3_135_enhanced_0.1,
  z = ~z_matrix135_enhanced_0.1,
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
fig_135_enhanced_0.1_contour

fig_135_enhanced_0.1 <- plot_ly(x = ~d_h1h2_135_enhanced_0.1, y = ~d_h1h3_135_enhanced_0.1, z = ~z_matrix135_enhanced_0.1) %>%
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
fig_135_enhanced_0.1
#El estado de transicion se encuentra en x=0.9555556, y=0.9555556 y tiene una energia de -1.588305. Con una resolucion de 0.01Angstrom
# Grafico 90 --------------------------------------------------------------

z_matrix90<-acast(output_HHH90_orca_TP3, dH1H2 ~ dH1H3, value.var = "Eh")
d_h1h2_90<-as.numeric(colnames(z_matrix90))
d_h1h3_90<-as.numeric(rownames(z_matrix90))

fig_contour_90<-plot_ly(
  x= ~d_h1h2_90,
  y= ~d_h1h3_90,
  z= ~z_matrix90,
  type = "contour",
  color = "jet",
  autocontour= T,
  line= list(smoothing=0.85, colour="white", width=0.5)
) %>% 
  layout(
    title="SEP (90°) H + H-H -> H-H + H",
    scene = list(
      xaxis = list(title = "R(H-H, ˚A)"),
      yaxis = list(title = "R(H-H, ˚A)"),
      zaxis = list(title = "Energıa (E)")
    )
  )

fig_contour_90

fig_90<-plot_ly(x=~d_h1h2_90,
                y=~d_h1h3_90,
                z=~z_matrix90) %>% 
  add_surface(
    colorscale="jet",
    colorbar = list(title = "Energıa (E)")
  ) %>%
  layout(
    title = "SEP (90°) H + H-H -> H-H + H",
    scene = list(
      xaxis = list(title = "R(H-H, ˚A)"),
      yaxis = list(title = "R(H-H, ˚A)"),
      zaxis = list(title = "Energıa (E)")
    )
  )
fig_90


# Grafico 90 enhanced -----------------------------------------------------
Output_HHH90_enhanced <- read_excel("Resultados orca HHH90 enhanced/Output_HHH90_enhanced.xlsx", 
                                    col_types = c("numeric", "numeric", "numeric"))
View(Output_HHH90_enhanced)

z_matrix90_enhanced<-acast(Output_HHH90_enhanced, dH1H2 ~ dH1H3, value.var = "Eh")
d_h1h2_90_enhanced<-as.numeric(colnames(z_matrix90_enhanced))
d_h1h3_90_enhanced<-as.numeric(rownames(z_matrix90_enhanced))

fig_contour_90_enhanced<-plot_ly(
  x= ~d_h1h2_90_enhanced,
  y= ~d_h1h3_90_enhanced,
  z= ~z_matrix90_enhanced,
  type = "contour",
  color = "jet",
  autocontour= T,
  line= list(smoothing=0.85, colour="white", width=0.5)
) %>% 
  layout(
    title="SEP (90°) H + H-H -> H-H + H",
    scene = list(
      xaxis = list(title = "R(H-H, ˚A)"),
      yaxis = list(title = "R(H-H, ˚A)"),
      zaxis = list(title = "Energıa (E)")
    )
  )

fig_contour_90_enhanced

fig_90_enhanced<-plot_ly(x=~d_h1h2_90_enhanced,
                y=~d_h1h3_90_enhanced,
                z=~z_matrix90_enhanced) %>% 
  add_surface(
    colorscale="jet",
    colorbar = list(title = "Energıa (E)")
  ) %>%
  layout(
    title = "SEP (90°) H + H-H -> H-H + H",
    scene = list(
      xaxis = list(title = "R(H-H, ˚A)"),
      yaxis = list(title = "R(H-H, ˚A)"),
      zaxis = list(title = "Energıa (E)")
    )
  )
fig_90_enhanced



# Grafico 90 enhanced 0.1Angstrom -----------------------------------------

Output_HHH90_enhanced_0.1 <- read_excel("Resultados orca HHH90 enhanced 0.1/Output_HHH90_enhanced_0.1.xlsx", 
                                        col_types = c("numeric", "numeric", "numeric"))
View(Output_HHH90_enhanced_0.1)


z_matrix90_enhanced_0.1<-acast(Output_HHH90_enhanced_0.1, dH1H2 ~ dH1H3, value.var = "Eh")
d_h1h2_90_enhanced_0.1<-as.numeric(colnames(z_matrix90_enhanced_0.1))
d_h1h3_90_enhanced_0.1<-as.numeric(rownames(z_matrix90_enhanced_0.1))

fig_contour_90_enhanced_0.1<-plot_ly(
  x= ~d_h1h2_90_enhanced_0.1,
  y= ~d_h1h3_90_enhanced_0.1,
  z= ~z_matrix90_enhanced_0.1,
  type = "contour",
  color = "jet",
  autocontour= T,
  line= list(smoothing=0.85, colour="white", width=0.5)
) %>% 
  layout(
    title="SEP (90°) H + H-H -> H-H + H",
    scene = list(
      xaxis = list(title = "R(H-H, ˚A)"),
      yaxis = list(title = "R(H-H, ˚A)"),
      zaxis = list(title = "Energıa (E)")
    )
  )

fig_contour_90_enhanced_0.1

fig_90_enhanced_0.1<-plot_ly(x=~d_h1h2_90_enhanced_0.1,
                         y=~d_h1h3_90_enhanced_0.1,
                         z=~z_matrix90_enhanced_0.1) %>% 
  add_surface(
    colorscale="jet",
    colorbar = list(title = "Energıa (E)")
  ) %>%
  layout(
    title = "SEP (90°) H + H-H -> H-H + H",
    scene = list(
      xaxis = list(title = "R(H-H, ˚A)"),
      yaxis = list(title = "R(H-H, ˚A)"),
      zaxis = list(title = "Energıa (E)")
    )
  )
fig_90_enhanced_0.1

#El estado de transicion se encuentra en x=1.027778, y=1.027778 y tiene una energia de -1.553435. Con una resolucion de 0.01Angstrom

# Barrera de energia ------------------------------------------------------


#minimum barrier 180°=1.596517-1.596486  135°= 1.588395-1.588305  90°=1.553529-1.553435

cat("energy barrier for 180°=",1.596517-1.596486," for 135°=",1.588395-1.588305," and for 90°=",1.553529-1.553435)