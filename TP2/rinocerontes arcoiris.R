library(tidyverse)

#Actividad 1: Cineticas sencillas

k<-(1)
dt<-0.001
R0<-100
t0<-0

R_list<-c(R0)
t_list<-c(t0)
v_list<-c(v0)
  
R=R0
t=t0
v0=k*R0


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
           y=v_list))+
  geom_point(size=1, color="cyan4")+
  theme_bw()


#Actividad 3: Reacciones oscilatorias


A0=1
B0=3
dt=0.005
k1=1
k2=1
k3=1
k4=1
t0=0
dt=0.005

t=t0
A=A0
B=B0
D=0
E=0
X=0
Y=0

t_list<-c(t0)
A_list<-c(A0)
B_list<-c(B0)
D_list<-c(0)
E_list<-c(0)
X_list<-c(0)
Y_list<-c(0)

for (i in 1:10000) {
  dA=-k1*A*dt
  A=A+dA
  A_list<-c(A_list, A)
  
  dB=-k3*B*X*dt
  B=B+dB
  B_list<-c(B_list, B)
  
  dD=k3*B*X*dt
  D=D+dD
  D_list<-c(D_list, D)
  
  dE=k4*X*dt
  E=E+dE
  E_list<-c(E_list, E)
  
  dX=(k1*A+k2*(X ^ 2)*Y-k3*B*X-k4*X)*dt
  X=X+dX
  X_list<-c(X_list, X)
  
  dY=(-k2*(X ^ 2)*Y+k3*B*X)*dt
  Y=Y+dY
  Y_list<-c(Y_list, Y)
  
  t=t+dt
  t_list<-c(t_list, t)

}

data_list_TP2_A3<-data.frame(A_list, B_list, X_list, Y_list, D_list, E_list, t_list)
View(data_list_TP2_A3)

ggplot(data=data_list_TP2_A3,
       aes(x=t_list, 
           y=A_list))+
  geom_point(size=1, color="deepskyblue")+
  theme_bw()

ggplot(data=data_list_TP2_A3,
       aes(x=t_list,
           y=B_list))+
  geom_point(size=1, color="deeppink2")+
  theme_bw()


ggplot(data=data_list_TP2_A3,
       aes(x=t_list,
           y=D_list))+
  geom_point(size=1, color="darkorchid3")+
  theme_bw()


ggplot(data=data_list_TP2_A3,
       aes(x=t_list,
           y=E_list))+
  geom_point(size=1, color="midnightblue")+
  theme_bw()


ggplot(data=data_list_TP2_A3,
       aes(x=t_list,
           y=Y_list))+
  geom_point(size=1, color="yellowgreen")+
  theme_bw()

ggplot(data=data_list_TP2_A3,
       aes(x=t_list,
           y=X_list))+
  geom_point(size=1, color="firebrick3")+
  theme_bw()
