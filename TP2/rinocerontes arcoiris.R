library(tidyverse)

#Actividad 1: Cineticas sencillas

k<-(1)
dt<-0.001
R0<-100
t0<-0
v0=k*R0

R_list<-c(R0)
t_list<-c(t0)
v_list<-c(v0)
  
R=R0
t=t0


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
       aes(x=R_list, 
           y=v_list))+
  geom_point(size=1, color="purple")+
  theme_bw()

#Calculo para una reaccion de orden 2 vamos a definir las variables con un 1c ya q ue corresponde a la actividad 1c

k_1c<-(1)
dt_1c<-0.001
R0_1c<-100
t0_1c<-0
v0_1c=k_1c*R0_1c^2

R_list_1c<-c(R0_1c)
t_list_1c<-c(t0_1c)
v_list_1c<-c(v0_1c)

R_1c=R0_1c
t_1c=t0_1c


for (i in 1:5000){
  dR_1c<- -2*k_1c*(R_1c^2)*dt_1c
  R_1c<-R_1c+dR_1c
  R_list_1c<-c(R_list_1c,R_1c)
  t_1c<-t_1c+dt_1c
  t_list_1c<-c(t_list_1c,t_1c)
  v_1c=k_1c*R_1c^2
  v_list_1c<-c(v_list_1c,v_1c)
}

data_list_TP2_act_1c<-data.frame(R_list_1c, t_list_1c, v_list_1c,
                                 R_list_squared=R_list_1c^2,
                                 R_list_inverse=1/R_list_1c)

view(data_list_TP2_act_1c)

ggplot(data = data_list_TP2_act_1c,
       aes(x=R_list_1c,
           y=v_list_1c))+
  geom_point(size=1, color="palevioletred2")+
  theme_bw()

ggplot(data = data_list_TP2_act_1c,
       aes(x=R_list_squared,
           y=v_list_1c))+
  geom_point(size=1, color="coral2")+
  theme_bw()
         
ggplot(data = data_list_TP2_act_1c,
       aes(x=t_list_1c,
           y=R_list_inverse))+
  geom_point(size=1, color="darkolivegreen3")+
  theme_bw()

#Compare los graficos que se obtienen cambiando “dt” a 0.01 y 0.1. ¿Que ocurre
#si aumenta “dt” a un valor relativamente grande, por ejemplo, 2?

#Probar con dt=2


#Actividad 2: Cinetica enzimatica


E0=1
S0=1000
dt=0.00005
kf=1
kr=1
kp=10
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

linear_model_of_S_vs_t<-lm(data_list_TP2_A2$S_list~data_list_TP2_A2$t_list,
                           data = data_list_TP2_A2,
                           subset = t_list>0.05)

summary(linear_model_of_S_vs_t)

#la regresion lineal de la parte lineal es:  [S]=9.991e+02-9.490e-02t, si buscamos el valor de t para el que [S]=0.95[S_0] tenemos

cat("El tiempo al que la concentracion de sustrato es el 95% de la inicial es:", ((0.95*S0-coef(linear_model_of_S_vs_t)[1])/coef(linear_model_of_S_vs_t)[2]),"segundos"
)
#para ver como cambia esto con kp=10 se volvio a correr la seccion del script correspondiente cambiando la variable kp y el valor obtenido por la linea anterior es: 5.219329 segundos

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
X0=0
Y0=0

t=t0
A=A0
B=B0
D=0
E=0
X=X0
Y=Y0

t_list<-c(t0)
A_list<-c(A0)
B_list<-c(B0)
D_list<-c(0)
E_list<-c(0)
X_list<-c(X0)
Y_list<-c(Y0)

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
  
  dX=(k1*A+k2*(X^2)*Y-k3*B*X-k4*X)*dt
  X=X+dX
  X_list<-c(X_list, X)
  
  dY=(-k2*(X^2)*Y+k3*B*X)*dt
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


#grafica X vs Y

ggplot(data=data_list_TP2_A3,
       aes(x=X_list,
           y=Y_list))+
  geom_point(size=1, color="orchid")+
  theme_bw()



