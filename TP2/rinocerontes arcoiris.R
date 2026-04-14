library(tidyverse)

#Actividad 1: Cineticas sencillas

k<-(1)
dt<-0.001
R0<-100
t0<-0
R_list<-c(R0)
t_list<-c(t0)*
R=R0
t=t0
v0=k*R0
v_list<-c(v0)

for (i in 1:5000){
dR<- -k*R*dt
R<-R+dR
R_list<-c(R_list,R)
t<-t+dt
t_list<-c(t_list,t)
v=k*R
v_list<-c(v_list,v)
}

view(R_list)

data_list_TP2<-data.frame(R_list, t_list, v_list)

ggplot(data=data_list_TP2,
       aes(x=t_list, 
           y=R_list))+
      geom_point(size=1, color="purple")+
theme_bw()

data_list_TP2<-data_list_TP2 %>% 
  mutate(lnR_list=log(R_list))
view(data_list_TP2)


ggplot(data=data_list_TP2,
       aes(x=t_list, 
           y=lnR_list))+
  geom_point(size=1, color="purple")+
  theme_bw()


ggplot(data=data_list_TP2,
       aes(x=v_list, 
           y=R_list))+
  geom_point(size=1, color="purple")+
  theme_bw()

#calcular lo mismo para una reaccion de orden 2


#Compare los graficos que se obtienen cambiando “dt” a 0.01 y 0.1. ¿Que ocurre
#si aumenta “dt” a un valor relativamente grande, por ejemplo, 2?

#Probar con dt=2


#Actividad 2: Cinetica enzimatica


E0=1
S0=1000
dt=0.00005
kf=1
kr=1
kp=0.1
t0=0

E_list<-c(E0)
S_list<-c(S0)
ES_list<-c(0)
P_list<-c(0)
v_list<-c(0)
t_list<-c(t0)

E=E0
S=S0
P=0
ES=0
v=0
t=t0

for (i in 1:5000){
  dE=(-kf*E*S+kf*ES+kp*ES)*dt
  E=E+dE
  E_list<-c(E_list,E)
  
  dS=(-kf*E*S+kr*ES)*dt
  S=S+dS
  S_list<-c(S_list, S)
  
  dES=(kf*E*S-kr*ES-kp*ES)*dt
  ES=ES+dES
  ES_list<-c(ES_list, ES)
  
  dP=kp*ES*dt
  P=P+dP
  P_list<-c(P_list, P)
  
  v=kp*ES
  v_list<-c(v_list,v)
  
  t=t+dt
  t_list<-c(t_list, t)
  
}

data_list_TP2_A2<-data.frame(E_list, S_list, ES_list, P_list, t_list, v_list)
view(data_list_TP2_A2)


ggplot(data=data_list_TP2_A2,
       aes(x=t_list, 
           y=P_list))+
  geom_point(size=1, color="darkgreen")+
theme_bw()

ggplot(data=data_list_TP2_A2,
       aes(x=t_list,
           y=S_list))+
  geom_point(size=1, color="magenta4")+
  theme_bw()


ggplot(data=data_list_TP2_A2,
       aes(x=t_list,
           y=ES_list))+
  geom_point(size=1, color="darkorange2")+
  theme_bw()


ggplot(data=data_list_TP2_A2,
       aes(x=t_list,
           y=S_list))+
  geom_point(size=1, color="magenta4")+
  theme_bw()



ggplot(data=data_list_TP2_A2,
       aes(x=t_list,
           y=v_list))+
  geom_point(size=1, color="cyan4")+
  theme_bw()


#Actividad 3: Reacciones oscilatorias








