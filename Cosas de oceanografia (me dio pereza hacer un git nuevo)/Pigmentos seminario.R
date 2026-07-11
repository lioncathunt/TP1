library(tidyverse)
library(readr)


tabla_1_pigmenos_semi <- read_csv("C:/Users/leonh/OneDrive/Escritorio/Facultad/ocq/tabla 1 pigmenos semi.csv")
View(tabla_1_pigmenos_semi)

tabla_1_pigmenos_semi<-tabla_1_pigmenos_semi |> mutate(Chl_a=(11.85*(A664-A750)-1.54*(A647-A750)-0.08*(A630-A750)),
                                                       Chl_b=(-5.43*(A664-A750) + 21.03*(A647-A750)-2.66*(A630-A750)),
                                                       Chl_c=(-1.67*(A664-A750)-7.6*(A647-A750) + 24.52*(A630-A750))) |> 
  mutate(Vol_extracto=5,
         Vol_muestra_filtrada=c(1,1,0.8,0.9,0.8,0.7)) |>
  mutate(Concentración_chl_a=(Chl_a*Vol_extracto/Vol_muestra_filtrada),
         Concentración_chl_b=(Chl_b*Vol_extracto/Vol_muestra_filtrada),
         Concentración_chl_c=(Chl_c*Vol_extracto/Vol_muestra_filtrada))
View(tabla_1_pigmenos_semi)

Concentraciones_clorofilas<-data.frame("muestra"=c("E1","E2","E3","E4"),
                                       "Concentración Clorofila A"=tabla_1_pigmenos_semi$Concentración_chl_a[3:6],
                                       "Concentración Clorofila B"=tabla_1_pigmenos_semi$Concentración_chl_b[3:6],
                                       "Concentración Clorofila C"=tabla_1_pigmenos_semi$Concentración_chl_b[3:6])
View(Concentraciones_clorofilas)

write.csv(Concentraciones_clorofilas, "Concentraciones_clorofilas.csv")

