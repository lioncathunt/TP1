library(ggplot2)
library(dplyr)
library(broom)
library(ggpubr)
library(kableExtra)
library(tidyverse)

#crear la tabla
  p<- c(12,48,72,120)
 tmv<- c(52.5,210,315,525)
  
#Para determinar el orden y la k debemos usar el metodo del tiempo de vida media: para esto necesitamos sacar el log de tmv y de p0
  
  "logp"<-log10(p)
  "logtmv"<- log10(tmv)
  
#ahora graficamos logtmv vs logp
  Data_ejercicio_1<-data.frame(p,tmv,logp,logtmv)
  View(Data_ejercicio_1) 
  
  ggplot(data=Data_ejercicio_1,
         aes(x=logp,
             y=logtmv))+
    geom_point(size=5)+
    geom_smooth(method=lm, formula = 'y ~ x', colour="purple")+
    stat_regline_equation(label.x = 1.125*10^2,label.y = 2.625)+
    annotate("text",
             x=1.125,y=2.50, hjust=0,
             label="n=0 k=0.114")+
    labs(title = "log de Tiempo de vida media vs log de presion")
  
  #de la pendiente obtenemos el orden de la reacción ya que esta debe ser igual a -(n-1)
  #por lo tanto n=0
  #la K sale de la ordenada al origen ya que b=log10(/frac{2^{n-1}-1}{(n-1)K})
  #por lo que (10^-0.64)*0.5=K=0.114
  
  
  

