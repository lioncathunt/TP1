library(tidyverse)
library(reshape2)
library(plotly)



df180 <- read_excel()
z_matrix180 <- acast(df180, dH1H2 ~ dH1H3, value.var = "Eh")
d_h1h2 <- as.numeric(colnames(z_matrix180))
d_h1h3 <- as.numeric(rownames(z_matrix180))
fig_180_contour <- plot_ly(
  x = ~d_h1h2,
  y = ~d_h1h3,
  z = ~z_matrix180,
  type = "contour",
  colorscale = "Jet",
  autocontour = TRUE,
  line = list(smoothing = 0.85, color = ’white’, width = 0.5)
) %>%
  layout(
    title = "Mapa de Contornos de Energ´ıa Potencial",
    xaxis = list(title = "R(H-H, ˚A)"),
    yaxis = list(title = "R(H-H, ˚A)"),
    plot_bgcolor = "#e5ecf6"
  )
fig_180_contour