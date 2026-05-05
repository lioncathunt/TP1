library(tidyverse)
library(readxl)
library(ggplot2)
library(dplyr)
library(broom)
library(ggpubr)
library(plotly)
library(usethis)



# variables ---------------------------------------------------------------

Time<-c(3,5,7,10,15,20,25)*60
#Concentration 0
C0<-1
Concentrations<-c(.869,.790,.722,.623,.494,.386,.306)*C0
Data<-data.frame(Time, Concentrations)

# Test order 0 ------------------------------------------------------------

Test_order_0<-ggplot(data = Data,
                     aes(x=Time,
                         y=Concentrations))+
  geom_point()+
  geom_smooth(method = lm, formula = y~x)

Test_order_0
# Test order 1 ------------------------------------------------------------

Test_order_1<-ggplot(data = Data,
                     aes(x=Time,
                         y=log(Concentrations)))+
  geom_point()+
  geom_smooth(method = lm, formula = y~x)
Test_order_1


# Test order 2 ------------------------------------------------------------

Test_order_2<-ggplot(data = Data,
                     aes(x=Time,
                         y=1/Concentrations))+
  geom_point()+
  geom_smooth(method = lm, formula = y~x)
Test_order_2

lm_Order_0<-lm(Concentrations~Time)
lm_Order_1<-lm(log(Concentrations)~Time)
lm_Order_2<-lm((1/Concentrations)~Time)

summary(lm_Order_0)
summary(lm_Order_1)
summary(lm_Order_2)

#Since the reaction is of first order, the slope is -K therefore

cat("The value of k is ", -coef(lm_Order_1)[2])