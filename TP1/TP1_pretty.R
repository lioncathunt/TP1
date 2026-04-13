library(tidyverse)
library(readxl)
library(ggplot2)
library(dplyr)
library(broom)
library(ggpubr)
library(plotly)
library(usethis)

#We import the data in the attached excel document making sure that the absorbency and time columns are set as numeric and the concentration as character


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
  
  Data_TP1_NaOH_K<-data.frame("Concentration of NaOH"=c(0.05, 0.10,0.15,0.20,0.25),
                              "K prime value"=c(coef(lm_0.05M)[2],coef(lm_0.1M)[2],coef(lm_0.15M)[2],coef(lm_0.2M)[2],coef(lm_0.25M)[2])*-1)
view(Data_TP1_NaOH_K)

#now we plot the concentration of NaOH vs K' for different values of m, to do this we need to plot
#with m=0, k'=k; with m=1, k'~[OH-]; with m=2, k'~[OH-]^2

Data_TP1_NaOH_K_squared<-Data_TP1_NaOH_K %>% 
  mutate("Concentration of NaOH squared"=Concentration.of.NaOH^2)
view(Data_TP1_NaOH_K_squared)

plot_NaOH_k_m1<-ggplot(data = Data_TP1_NaOH_K_squared,
                    aes(x=Concentration.of.NaOH,
                        y=K.prime.value))+
  geom_point()+
  geom_smooth(method = lm, se = F)

plot_NaOH_k_m1

plot_NaOH_k_m2<-ggplot(data = Data_TP1_NaOH_K_squared,
                       aes(x=`Concentration of NaOH squared`,
                           y=K.prime.value))+
  geom_point()+
  geom_smooth(method = "lm", formula = y~poly(x,2), se=F)
plot_NaOH_k_m2

lm_NaOH_vs_k_m1<-lm(Data_TP1_NaOH_K_squared$K.prime.value~Data_TP1_NaOH_K_squared$Concentration.of.NaOH)

summary(lm_NaOH_vs_k_m1)

#since m=1 and k'=k[OH-] the slope of the linear regression of this plot is equal to k therefore we can extract this coefficient from the linear model to get our value of k.

print(coef(lm_NaOH_vs_k_m1)[2])

#the value of k is then 0.0157 L*mol^-1*s^-1 =0.9419 L*mol^-1*min^-1 


#Guia de Preguntas y Discusi´on
#1. ¿Por qu´e la reacci´on se considera de pseudo-primer orden respecto a la fenolftale´ına?
#2. ¿Qu´e efecto tiene la fuerza i´onica constante en los resultados? Explicar en t´erminos de interacciones i´onicas.
#3. ¿Por qu´e es importante retirar la cubeta del espectrofot´ometro entre mediciones?
#4. Comparar el valor de k obtenido con el reportado en la literatura. Analizar posibles fuentes de error.
#5. Proponga un mecanismo de reacci´on coherente con los ´ordenes determinados.
                      