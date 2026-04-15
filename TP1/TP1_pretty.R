library(tidyverse)
library(readxl)
library(ggplot2)
library(dplyr)
library(broom)
library(ggpubr)
library(plotly)
library(usethis)

#We import the data in the attached excel document making sure that the absorbency and time columns are set as numeric and the concentration as character
library(readxl)
Data_TP1_FQC_tidy <- read_excel("Data_TP1_FQC_tidy.xlsx", 
                                col_types = c("numeric", "numeric", "text"))
View(Data_TP1_FQC_tidy)




#we need the logarithms of the absorbency so we will add that column to the dataset

Data_TP1_FQC_tidy_with_logs<-Data_TP1_FQC_tidy %>%  
  mutate(Log_of_Absorbency=log(Absorbency))

view(Data_TP1_FQC_tidy_with_logs)

#now we plot the logarithms of the absorbency vs the time

Plot_logs_vs_time<-ggplot(data = Data_TP1_FQC_tidy_with_logs,
                          aes(x=Time,
                              y=Log_of_Absorbency,
                              Color = Concentration))+
  geom_point()+
  geom_smooth(method = "lm")+
  facet_wrap(~ Concentration, scales="free")

  Plot_logs_vs_time
  
  
  #and we make the linear regressions for the plots
  
  #for 0.05M
  lm_0.05M<-lm(Data_TP1_FQC_tidy_with_logs$Log_of_Absorbency~Data_TP1_FQC_tidy_with_logs$Time,
     data = Data_TP1_FQC_tidy_with_logs,
     subset =Concentration =="0.05M")
  
  summary(lm_0.05M)
  
  #for 0.1M
  lm_0.1M<-lm(Data_TP1_FQC_tidy_with_logs$Log_of_Absorbency~Data_TP1_FQC_tidy_with_logs$Time,
               data = Data_TP1_FQC_tidy_with_logs,
               subset =Concentration =="0.1M")
  
  summary(lm_0.1M)
  
#for 0.15M
  lm_0.15M<-lm(Data_TP1_FQC_tidy_with_logs$Log_of_Absorbency~Data_TP1_FQC_tidy_with_logs$Time,
               data = Data_TP1_FQC_tidy_with_logs,
               subset =Concentration =="0.15M")
  
  summary(lm_0.15M)
  
  #for 0.2M
  lm_0.2M<-lm(Data_TP1_FQC_tidy_with_logs$Log_of_Absorbency~Data_TP1_FQC_tidy_with_logs$Time,
               data = Data_TP1_FQC_tidy_with_logs,
               subset =Concentration =="0.20M")
  
  summary(lm_0.2M)
  
  #for 0.25M
  lm_0.25M<-lm(Data_TP1_FQC_tidy_with_logs$Log_of_Absorbency~Data_TP1_FQC_tidy_with_logs$Time,
               data = Data_TP1_FQC_tidy_with_logs,
               subset =Concentration =="0.25M")
  
  summary(lm_0.25M)
  
  #We are interested in the slope of the linear regressions, which are equal to -k', so we will now make a table with these values for each concentration
  #For this we need the actual concentrations of the NaOH solutions made in the lab
  Mass_of_NaOH_mg<-c(97.9,200.9,302.2,424.1,495.9)
  mMoles_of_NaOH<-Mass_of_NaOH_mg/40
  Concentration_of_NaOH_solutions<-mMoles_of_NaOH/50
  
  Data_TP1_NaOH_K<-data.frame("Concentration of NaOH"=Concentration_of_NaOH_solutions,
                              "K prime value"=c(coef(lm_0.05M)[2],coef(lm_0.1M)[2],coef(lm_0.15M)[2],coef(lm_0.2M)[2],coef(lm_0.25M)[2])*-1)
view(Data_TP1_NaOH_K)



#we get the value of m and the value of k from applying logs
#to both sides of k'=k[OH]^m
#we get ln(k')=ln(k)+m*ln([OH-]) then if we plot ln(k')vsln([OH-]) we get

Data_TP1_NaOH_K_with_logs<-Data_TP1_NaOH_K %>% 
  mutate(logarithm_of_k_prime=log(K.prime.value)) %>% 
  mutate(logarithm_of_NaOH=log(Concentration.of.NaOH))

ggplot(data = Data_TP1_NaOH_K_with_logs,
       aes(x=logarithm_of_NaOH,
           y=logarithm_of_k_prime))+
  geom_point()+
  geom_smooth(method = "lm")+
  stat_regline_equation()
lm_plot_logs<-lm(Data_TP1_NaOH_K_with_logs$logarithm_of_k_prime~Data_TP1_NaOH_K_with_logs$logarithm_of_NaOH)
summary(lm_plot_logs)

#intercept from this lm=ln(k)=
print(coef(lm_plot_logs)[1])
#slope from this lm=m=
print(coef(lm_plot_logs)[2])

#then k in L*mol-1*min-1=
print((exp(coef(lm_plot_logs)[1]))*60)

#m is equal to the slope = 1.23863
#Guia de Preguntas y Discusion
#1. ¿Por que la reaccion se considera de pseudo-primer orden respecto a la fenolftaleına?
  
  # Se considera de pseudo-primer orden respecto a la fenolftalina 
  # porque la concentracion de NaOH es tan grande en comparación que se puede tomar como constante.


#2. ¿Que efecto tiene la fuerza ionica constante en los resultados? Explicar en terminos de interacciones ionicas.

  # La velocidad de decoloracion aumenta con la fuerza ionica debido a que la reaccion involucra el
#acercamiento de dos iones cargados negativamente y su repulsion disminuye en un ambiente que contiene iones inertes.

#3. ¿Por que es importante retirar la cubeta del espectrofotometro entre mediciones?

  #Es importante retirar la cubeta del espectrofotometro entre mediciones porque puede aumentar la temperatura 
  #de la solucion por estar en el haz de luz del equipo y la velocidad de la reaccion depende de la temperatura.


#4. Comparar el valor de k obtenido con el reportado en la literatura. Analizar posibles fuentes de error.

  # Nos dió 1.243 L.mol-1min-1 m mientras que en la literatura es de 1.1 L.mol−1.min−1. 
  # Las fuentes de error pudieron haber sido las preparaciones de las soluciones de NaOH y NaCl 
  # pudiendo generarse errores de pesada. 
  # 

#5. Proponga un mecanismo de reaccion coherente con los ordenes determinados.

  #
                      