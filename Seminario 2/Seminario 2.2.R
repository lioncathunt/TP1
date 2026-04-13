library(ggplot2)
library(dplyr)
library(broom)
library(ggpubr)
library(tidyverse)
library(Hmisc)
#Let's turn the data into vectors

time_ej2<-c(0, 100, 200, 300, 400, 500, 600, 700, 800)
concentration_ej2<-c(5.00, 3.50, 2.70, 2.20, 1.85, 1.60, 1.41, 1.26, 1.14)*10^2

#lets make a vector with the concentration we want to interpolate for the half-life times and correct the half-times

initial_concentrations<-c(5.00, 4.50, 4.00, 3.50, 3.00, 2.70)*10^2
Halflife_time_concentrations<-initial_concentrations/2
Interpolated_times<-c(approx(y=time_ej2, x=concentration_ej2, xout = initial_concentrations))
Interpolated_halflife_times<-c(approx(y=time_ej2, x=concentration_ej2, xout =Halflife_time_concentrations))

#the last two lines interpolate the time value for the data we input as "xout" using the data we put in the x= and y= arguments 

#the result the interpolations yield are not number but pairs of x(concentration) and y(times) values
#because of this to operate with them and get the half-life times we need to only pick the time part of these pairs
#to do this we just add a $y at the end when calling the variable
Halflife_times<-Interpolated_halflife_times$y-Interpolated_times$y

#we take the logs of the concentrations and the half-life times and plot them to get the slope

Logarithm_of_the_initial_concentrations<-log10(initial_concentrations)
Logarithm_of_the_halflife_times<-log10(Halflife_times)

#Now let's make a table with all the data
data_ej_2<-data.frame("initial_concentrations"=initial_concentrations,
                      "Halflife_times"=Halflife_times,
                      "Logarithm_of_the_initial_concentrations"=Logarithm_of_the_initial_concentrations,
                      "Logarithm_of_the_halflife_times"=Logarithm_of_the_halflife_times)
#now we plot yey

ggplot(data = data_ej_2,
       aes(x=Logarithm_of_the_initial_concentrations,
           y=Logarithm_of_the_halflife_times))+
  geom_point()+
  geom_smooth(method = lm, se=F)+
  stat_regline_equation(label.x = 2.6, label.y = 2.60)

lm_ej2_halflife<-lm(Logarithm_of_the_halflife_times~Logarithm_of_the_initial_concentrations)
summary(lm_ej2_halflife)

#from this regression equation we get that m=-(n-1)=-1 therefore n=2
#then log(\frac{2^{n-1}-1}{(n-1)k})=b
#with n=2 log(\frac{1}{k})=b
#then k=10^{-b} with b=5.0821 k=8.277515e-06

#for the integral method, since we already know the order is 2 we don't need to try with every equation, we'll go straight to the second order one

#for second order, we have the equation [A]_0-akt=[A]
#if we plot the concentration of A vs the time we can get K from the slope

data_ej_2_integral_method<-data.frame("concentration"=1/concentration_ej2,
                                      "time"=time_ej2)
ggplot(data = data_ej_2_integral_method,
       aes(x=time,
           y=concentration))+
  geom_point()+
  geom_smooth(method=lm, se=F)+
  stat_regline_equation()

lm_integral_method<-lm(1/concentration_ej2~time_ej2)
summary(lm_integral_method)

#from the slope we know that the value of k is: 8.468e-06

